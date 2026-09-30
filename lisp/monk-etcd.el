;;; monk-etcd.el --- Shared state in etcd  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: tools, comm, data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A small etcd v3 client over the gRPC gateway (HTTP, JSON, base64
;; keys and values), enough for what a configuration tool needs from a
;; shared store: strings, per-host string maps, exported resources,
;; and the agent event stream.  Emacs has no gRPC, so the gateway is
;; the only route; it ships in every etcd since 3.0 on the same port.
;;
;; Key layout follows mgmt where the value format allows it, so a Monk
;; and an mgmt cluster can share one etcd:
;;
;;   /_mgmt/strings/KEY              str, value verbatim   (mgmt compatible)
;;   /_mgmt/strings/KEY/HOST         strmap, one per host  (mgmt compatible)
;;   /_mgmt/exported/TO/FROM/KIND/NAME  mgmt: gob+base64, unreadable here
;;
;; mgmt encodes exported resources with Go's gob, a reflection format
;; Emacs cannot decode (mgmt engine/util/util.go: ResToB64).  Monk
;; keeps its own resources and events under its own namespace as JSON:
;;
;;   /_monk/exported/TO/FROM/TYPE/NAME   JSON: {type,name,params,meta}
;;   /_monk/events/HOST/RUN/SEQ          JSON: one `monk-event' record
;;   /_monk/agents/HOST                  JSON: {ts, status...}, with a lease
;;
;; TO is a host name or "*" for every host, as in mgmt.  Watches use the
;; gateway's streaming endpoint over a raw `make-network-process'
;; connection; each JSON frame arrives as a line in the process filter.

;;; Code:

(require 'monk)
(require 'monk-event)
(require 'url)
(require 'url-http)
(require 'json)

(defvar url-http-response-status)

(defgroup monk-etcd nil
  "Shared state for Monk in etcd."
  :group 'monk
  :prefix "monk-etcd-")

;;;; Constants

(defconst monk-etcd-mgmt-namespace "/_mgmt"
  "Namespace mgmt uses.  Strings and string maps live here.")

(defconst monk-etcd-namespace "/_monk"
  "Namespace for Monk's own JSON data.")

(defconst monk-etcd-strings-path "strings")
(defconst monk-etcd-exported-path "exported")
(defconst monk-etcd-events-path "events")
(defconst monk-etcd-agents-path "agents")
(defconst monk-etcd-any-host "*"
  "Target that means every host, as in mgmt.")

(defconst monk-etcd-api-prefix "/v3"
  "Gateway path prefix.  etcd 3.4 and later; older ones use /v3beta.")

(defconst monk-etcd-endpoints
  '((range . "/kv/range") (put . "/kv/put") (delete . "/kv/deleterange")
    (grant . "/lease/grant") (keepalive . "/lease/keepalive") (watch . "/watch"))
  "Gateway endpoints by operation.")

(defconst monk-etcd-timeout 10
  "Seconds to wait for a unary request.")

;;;; Customization

(defcustom monk-etcd-url "http://127.0.0.1:2379"
  "Base URL of an etcd member's client port."
  :type 'string)

(defcustom monk-etcd-agent-ttl 30
  "Seconds an agent's presence key lives without a keepalive."
  :type 'natnum)

;;;; Encoding

(defun monk-etcd--b64 (string)
  "Return STRING base64 encoded as UTF-8, one line."
  (base64-encode-string (encode-coding-string string 'utf-8) t))

(defun monk-etcd--unb64 (string)
  "Return base64 STRING decoded as UTF-8."
  (decode-coding-string (base64-decode-string string) 'utf-8))

(defun monk-etcd--prefix-end (key)
  "Return the range end that covers every key with prefix KEY.
etcd has no prefix flag on range; the convention is the key with its
last byte incremented."
  (let ((bytes (encode-coding-string key 'utf-8)))
    (concat (substring bytes 0 -1)
            (string (1+ (aref bytes (1- (length bytes))))))))

;;;; HTTP

(defun monk-etcd--request (operation body)
  "POST BODY, an alist, to OPERATION on the gateway.  Return the parsed reply.
Signal `monk-error' on HTTP or etcd errors."
  (let* ((url (concat monk-etcd-url monk-etcd-api-prefix
                      (alist-get operation monk-etcd-endpoints)))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string
                            (json-serialize body :null-object nil :false-object :false)
                            'utf-8))
         (url-show-status nil)
         (buffer (url-retrieve-synchronously url t t monk-etcd-timeout)))
    (unless buffer
      (signal 'monk-error (list "etcd: no reply from" url)))
    (with-current-buffer buffer
      (unwind-protect
          (let ((status (or (and (boundp 'url-http-response-status) url-http-response-status)
                            ;; Fallback: parse the status line ourselves.
                            (save-excursion
                              (goto-char (point-min))
                              (and (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
                                   (string-to-number (match-string 1)))))))
            (goto-char (point-min))
            (re-search-forward "^\r?$" nil t)
            (let ((reply (condition-case nil
                             (json-parse-buffer :object-type 'alist :array-type 'list
                                                :null-object nil :false-object :false)
                           (error nil))))
              (unless (and status (< status 300))
                (signal 'monk-error (list (format "etcd %s: HTTP %s: %s" operation status
                                                  (or (alist-get 'message reply) "")))))
              reply))
        (kill-buffer buffer)))))

;;;; Key value primitives

(defun monk-etcd-put (key value &optional lease)
  "Store VALUE, a string, at KEY.  With LEASE, an id, attach it."
  (monk-etcd--request 'put `((key . ,(monk-etcd--b64 key))
                             (value . ,(monk-etcd--b64 value))
                             ,@(and lease `((lease . ,lease)))))
  key)

(defun monk-etcd-get (key)
  "Return the value at KEY as a string, or nil."
  (let ((kvs (alist-get 'kvs (monk-etcd--request 'range `((key . ,(monk-etcd--b64 key)))))))
    (and kvs (monk-etcd--unb64 (alist-get 'value (car kvs))))))

(defun monk-etcd-get-prefix (prefix)
  "Return an alist of (KEY . VALUE) for every key under PREFIX."
  (let ((kvs (alist-get 'kvs (monk-etcd--request
                              'range `((key . ,(monk-etcd--b64 prefix))
                                       (range_end . ,(monk-etcd--b64 (monk-etcd--prefix-end prefix))))))))
    (mapcar (lambda (kv) (cons (monk-etcd--unb64 (alist-get 'key kv))
                               (monk-etcd--unb64 (alist-get 'value kv))))
            kvs)))

(defun monk-etcd-delete (key &optional prefix)
  "Delete KEY.  With PREFIX, delete every key under it.  Return the count."
  (alist-get 'deleted
             (monk-etcd--request 'delete `((key . ,(monk-etcd--b64 key))
                                           ,@(and prefix
                                                  `((range_end . ,(monk-etcd--b64
                                                                   (monk-etcd--prefix-end key)))))))
             0))

(defun monk-etcd-lease (ttl)
  "Return a new lease id that expires after TTL seconds without keepalive."
  (string-to-number (alist-get 'ID (monk-etcd--request 'grant `((TTL . ,ttl))))))

(defun monk-etcd-keepalive (lease)
  "Refresh LEASE.  Return the remaining TTL, or nil when it is gone."
  (let ((reply (monk-etcd--request 'keepalive `((ID . ,lease)))))
    (alist-get 'TTL (alist-get 'result reply))))

;;;; mgmt-compatible strings

(defun monk-etcd-str-key (key)
  "Return the etcd key for string KEY."
  (format "%s/%s/%s" monk-etcd-mgmt-namespace monk-etcd-strings-path key))

(defun monk-etcd-str-set (key value)
  "Set string KEY to VALUE, where mgmt's world.StrSet would."
  (monk-etcd-put (monk-etcd-str-key key) value))

(defun monk-etcd-str-get (key)
  "Return string KEY, as mgmt's world.StrGet would."
  (monk-etcd-get (monk-etcd-str-key key)))

(defun monk-etcd-strmap-set (key host value)
  "Set the HOST entry of string map KEY to VALUE."
  (monk-etcd-put (format "%s/%s" (monk-etcd-str-key key) host) value))

(defun monk-etcd-strmap-get (key)
  "Return string map KEY as an alist of (HOST . VALUE)."
  (let ((prefix (concat (monk-etcd-str-key key) "/")))
    (mapcar (lambda (cell) (cons (substring (car cell) (length prefix)) (cdr cell)))
            (monk-etcd-get-prefix prefix))))

;;;; Exported resources, native JSON

(defun monk-etcd-exported-key (to from type name)
  "Return the key for a resource of TYPE and NAME exported by FROM for TO."
  (format "%s/%s/%s/%s/%s/%s" monk-etcd-namespace monk-etcd-exported-path to from type name))

(cl-defmethod monk-resource-export-alist ((r monk-resource))
  "Return R as an alist of type, name, params, meta, for JSON.
Params are the initarg values of the class minus the common keys."
  (let ((params nil))
    (dolist (slot (eieio-class-slots (eieio-object-class r)))
      (let* ((sname (cl--slot-descriptor-name slot))
             (key (intern (format ":%s" sname))))
        (unless (or (memq key monk--common-keys) (eq sname 'name))
          (when (slot-boundp r sname)
            (push (cons sname (monk-event--jsonable (slot-value r sname))) params)))))
    `((type . ,(monk-resource-type r))
      (name . ,(oref r name))
      (params . ,(nreverse params))
      (meta . ((sudo . ,(if (oref r sudo) t :false))
               (require . ,(monk-event--jsonable (oref r require)))
               (watch . ,(monk-event--jsonable (oref r watch))))))))

(defun monk-etcd-export (from to resource)
  "Publish RESOURCE from host FROM for host TO, or `monk-etcd-any-host'."
  (monk-etcd-put (monk-etcd-exported-key to from (monk-resource-type resource)
                                         (oref resource name))
                 (json-serialize (monk-resource-export-alist resource)
                                 :null-object nil :false-object :false)))

(defun monk-etcd-collect (host &optional type)
  "Return resources exported for HOST or for every host, as alists.
With TYPE, only that resource type.  Each alist carries `from' too."
  (let (out)
    (dolist (to (list host monk-etcd-any-host))
      (dolist (cell (monk-etcd-get-prefix
                     (format "%s/%s/%s/" monk-etcd-namespace monk-etcd-exported-path to)))
        (let* ((parts (split-string (car cell) "/" t))
               ;; _monk exported TO FROM TYPE NAME...
               (from (nth 3 parts))
               (kind (nth 4 parts))
               (data (monk-event-parse (cdr cell))))
          (when (and data (or (null type) (equal kind type)))
            (push (cons (cons 'from from) data) out)))))
    (nreverse out)))

;;;; Events and presence

(defun monk-etcd-publish-run (run)
  "Store every event of RUN under the events path, one key per event."
  (let ((host (or (and (monk-run-machines run) (oref (monk-run-machine run) name))
                  (system-name)))
        (seq 0))
    (dolist (event (monk-run-events run))
      (monk-etcd-put (format "%s/%s/%s/%s/%06d" monk-etcd-namespace monk-etcd-events-path
                             host (monk-run-id run) seq)
                     (json-serialize event :null-object nil :false-object :false))
      (cl-incf seq))
    seq))

(defun monk-etcd-runs (host)
  "Return the run ids stored for HOST, oldest first."
  (delete-dups
   ;; _monk events HOST RUN SEQ
   (mapcar (lambda (cell) (nth 3 (split-string (car cell) "/" t)))
           (monk-etcd-get-prefix (format "%s/%s/%s/" monk-etcd-namespace
                                         monk-etcd-events-path host)))))

(defun monk-etcd-run (host run-id)
  "Rebuild the `monk-run' stored for HOST under RUN-ID."
  (monk-events-to-run
   (mapcar (lambda (cell) (monk-event-parse (cdr cell)))
           (monk-etcd-get-prefix (format "%s/%s/%s/%s/" monk-etcd-namespace
                                         monk-etcd-events-path host run-id)))))

(defun monk-etcd-announce (host status &optional lease)
  "Write presence for HOST with STATUS, an alist, under LEASE.
Return the lease id, granting one with `monk-etcd-agent-ttl' when LEASE is nil."
  (let ((lease (or lease (monk-etcd-lease monk-etcd-agent-ttl))))
    (monk-etcd-put (format "%s/%s/%s" monk-etcd-namespace monk-etcd-agents-path host)
                   (json-serialize `((ts . ,(monk-event--time)) ,@status)
                                   :null-object nil :false-object :false)
                   lease)
    lease))

(defun monk-etcd-agents ()
  "Return an alist of (HOST . STATUS-ALIST) for agents whose lease is alive."
  (let ((prefix (format "%s/%s/" monk-etcd-namespace monk-etcd-agents-path)))
    (mapcar (lambda (cell)
              (cons (substring (car cell) (length prefix)) (monk-event-parse (cdr cell))))
            (monk-etcd-get-prefix prefix))))

;;;; Watch

(defun monk-etcd--watch-filter (proc chunk callback)
  "Feed CHUNK from PROC to CALLBACK as parsed events.
The gateway streams one JSON object per line after the HTTP header."
  (let ((buffer (concat (or (process-get proc 'monk-partial) "") chunk))
        (frames nil))
    (unless (process-get proc 'monk-headers-done)
      (when (string-match "\r?\n\r?\n" buffer)
        (setq buffer (substring buffer (match-end 0)))
        (process-put proc 'monk-headers-done t)))
    (when (process-get proc 'monk-headers-done)
      (while (string-match "\n" buffer)
        (push (substring buffer 0 (match-beginning 0)) frames)
        (setq buffer (substring buffer (match-end 0)))))
    (process-put proc 'monk-partial buffer)
    (dolist (frame (nreverse frames))
      (let* ((parsed (monk-event-parse (string-trim frame)))
             (result (alist-get 'result parsed)))
        (dolist (ev (append (alist-get 'events result) nil))
          (let ((kv (alist-get 'kv ev)))
            (funcall callback
                     (if (equal (alist-get 'type ev) "DELETE") 'delete 'put)
                     (monk-etcd--unb64 (alist-get 'key kv))
                     (and (alist-get 'value kv) (monk-etcd--unb64 (alist-get 'value kv))))))))))

(defun monk-etcd-watch (prefix callback)
  "Watch every key under PREFIX; call CALLBACK with KIND KEY VALUE.
KIND is `put' or `delete'.  Return the connection process; delete it
to stop.  Uses a raw HTTP request over `make-network-process' because
`url-retrieve' buffers a streaming body until it ends."
  (let* ((url (url-generic-parse-url monk-etcd-url))
         (host (url-host url))
         (port (or (url-port url) 2379))
         (body (json-serialize
                `((create_request . ((key . ,(monk-etcd--b64 prefix))
                                     (range_end . ,(monk-etcd--b64 (monk-etcd--prefix-end prefix))))))
                :null-object nil :false-object :false))
         (proc (make-network-process
                :name (format "monk-etcd-watch %s" prefix)
                :host host :service port
                :coding 'utf-8-unix
                :filter (lambda (p chunk) (monk-etcd--watch-filter p chunk callback))
                :noquery t)))
    (process-send-string
     proc
     (format "POST %s%s HTTP/1.1\r\nHost: %s:%d\r\nContent-Type: application/json\r\nContent-Length: %d\r\nConnection: keep-alive\r\n\r\n%s"
             monk-etcd-api-prefix (alist-get 'watch monk-etcd-endpoints)
             host port (string-bytes body) body))
    (monk-log 'info "etcd: watching %s" prefix)
    proc))

(monk-note-load 'monk-etcd)
(provide 'monk-etcd)
;;; monk-etcd.el ends here
