# Monk tutorial

This is the prose form of `M-x monk-tutorial`. The interactive version opens a lesson buffer beside a throwaway home directory; `C-c C-c` runs the form at point against it, `C-c C-n` moves to the next lesson, `C-c C-q` deletes the sandbox. Two variables are bound there: `tutorial-home`, the sandbox directory, and `tutorial-m`, a machine whose home is that directory. Every `~` in the lessons lands in the sandbox and nothing outside it changes.

To follow along here without the buffer, evaluate these two forms in `*scratch*` after `(require 'monk-tutorial)`:

```elisp
(monk-tutorial-reset)          ; makes the sandbox, binds tutorial-home and tutorial-m
(monk-tutorial-quit)           ; deletes it when you are done
```

## 1. A resource is a promise

A policy is a function of one argument, the machine. Inside it, constructors describe the state you want. Nothing runs at definition time.

```elisp
(defun tutorial-policy-1 (_m)
  (monk-directory "~/.ssh" :mode #o700)
  (monk-symlink "~/.zshrc" "~/dotfiles/zshrc")
  (monk-file "~/.config/app.conf" :content "level=3\n"))
```

`monk-converge` checks each resource and repairs it. Run it twice. The first run reports three `repaired` and takes two passes, because a pass after repairs confirms that they held. The second run reports three `kept` in one pass. That is the whole idea: the same policy, applied again, changes nothing.

```elisp
(monk-converge tutorial-m #'tutorial-policy-1)
```

The `*monk*` buffer opens with one row per machine: a badge, a bar, counts, and time. `RET` on the row opens the detail buffer. The symlink target was expanded on the machine, so `~/dotfiles/zshrc` became an absolute path under the sandbox:

```elisp
(file-symlink-p (expand-file-name ".zshrc" tutorial-home))
```

## 2. Plan, then apply

A plan is a dry run. It reports `would-repair` where a change is needed and touches nothing. Break something first:

```elisp
(with-temp-file (expand-file-name ".config/app.conf" tutorial-home)
  (insert "level=9\n"))
(setq tutorial-plan (monk-plan tutorial-m #'tutorial-policy-1))
```

The plan buffer has a purple `PLAN` badge, a `Would` column, and `a to apply` in the header. The detail buffer shows a unified diff for `app.conf` with the removed and added lines colored. Press `a` in `*monk*`, or:

```elisp
(monk-apply tutorial-plan)
```

Apply repeats the plan for real and compares. The header turns into a blue `APPLY from plan` badge and the row gains a `Plan` column that reads `as planned`. Change a file by hand between plan and apply, and that column reads `unfulfilled` (the plan predicted a change that was already done) or `unplanned` (apply changed something the plan said was kept). The detail buffer then starts with a `differs from plan` section.

## 3. Classes and guards

Facts become classes: `linux` or `darwin`, the distribution, the machine name. Inventory variables with the value `t` become classes too. The sandbox machine carries `tutorial`:

```elisp
(oref tutorial-m classes)
```

`:classes` runs a resource only when every listed class is present. `:only-if` and `:not-if` take a function of the machine. A resource that does not apply is `skipped`, not `failed`:

```elisp
(monk-converge tutorial-m
  (lambda (m)
    (monk-file "~/on-tutorial" :content "yes\n" :classes '(tutorial))
    (monk-file "~/on-mars" :content "no\n" :classes '(mars))
    (monk-file "~/only-if" :content "guard\n"
               :only-if (lambda (m) (monk-class-p m 'linux)))))
```

## 4. Requisites: require, watch, onchanges, onfail

`:require` orders and gates: the resource runs after its refs, and only when they succeeded. `:watch` does the same and also runs an action when a watched ref changed in this run. A systemd unit restarts; a command runs. A command without `:creates` or `:unless` is always `kept`, so with `:watch` it is a handler:

```elisp
(defun tutorial-policy-4 (_m)
  (monk-file "~/etc/app.conf" :content "level=3\n")
  (monk-command "reload-app" "sh"
                :args (list "-c" (format "date >> %s" (monk-expand "~/etc/reloads")))
                :watch '("~/etc/app.conf"))
  (monk-command "after-reload" "true" :require '("reload-app")))

(monk-converge tutorial-m #'tutorial-policy-4)
```

The first run creates the file, so `reload-app` runs once, then `after-reload`. The second run keeps the file, so `reload-app` is kept and does not run. Count the lines in `~/etc/reloads` after each run to see it.

`:onchanges` runs only when a ref changed; `:onfail` only when a ref failed. A ref that names nothing is a policy error, raised before any resource runs:

```elisp
(condition-case err
    (monk-converge tutorial-m (lambda (_m) (monk-file "~/x" :content "" :require '("nope"))))
  (monk-requisite-error (error-message-string err)))
```

## 5. Convergence and flapping

Monk runs passes until a pass repairs nothing, up to `monk-max-passes`. Two resources that disagree about one file never settle. Monk notices when any resource repairs a second time, stops, and names it:

```elisp
(monk-converge tutorial-m
  (lambda (_m)
    (monk-file "~/fight" :content "a\n")
    (monk-line-in-file "~/fight" "b" :create t)))
```

The header ends with an amber `flapping 2` badge; hover it for the names. `monk-run-flapping` returns them, and `monk-run-converged-p` is false.

## 6. Degree: success is not binary

A set resource reports how much of it holds. This lesson fakes a package manager that has two of four packages and refuses to install more. The result is `partial` with degree 0.5. A `:degree` threshold on a requisite can accept that where a plain `:require` cannot:

```elisp
(cl-letf (((symbol-function 'monk-exec)
           (lambda (_m program &rest _)
             (if (equal program "dpkg-query")
                 (monk-exec-create :exit-code 0
                                   :output "a installed\nb installed\nc x\nd x\n")
               (monk-exec-create :exit-code 1 :output "apt refused")))))
  (let ((m (monk-local-machine :name "deb")))
    (oset m facts '((os-family . "Debian") (home . "/tmp")))
    (oset m classes (monk-hard-classes m))
    (monk-converge m
      (lambda (_m)
        (monk-apt "pkgs" '("a" "b" "c" "d") :ignore-errors t)
        (monk-command "needs-all" "true" :require '("pkgs"))
        (monk-command "needs-half" "true" :require '((:ref "pkgs" :degree 0.5))))
      :passes 1)))
```

`needs-all` is skipped; `needs-half` runs.

## 7. Several machines as one graph

`monk-orchestrate` compiles the policy for a list of machines and runs every resource in one dependency graph. A requisite can then point at another machine, `(a . "~/member")`, or at a group with a threshold, `(:ref (pool . "~/member") :min 1)`. Here two sandbox machines share the group `pool`:

```elisp
(let ((a (monk-tutorial--machine)) (b (monk-tutorial--machine)))
  (oset a name "a") (oset b name "b")
  (dolist (x (list a b)) (oset x groups '(pool)) (oset x classes (monk-hard-classes x)))
  (monk-orchestrate (list a b)
    (lambda (m)
      (monk-file "~/member" :content (oref m name))
      (when (equal (oref m name) "b")
        (monk-file "~/after-a" :content "ok\n" :require '((a . "~/member")))
        (monk-file "~/after-any" :content "ok\n"
                   :require '((:ref (pool . "~/member") :min 1)))))))
```

The report shows two rows. Real machines come from the inventory in `monk-inventory.el`, a group tree as Lisp data; `(monk-inventory-select 'home)` returns the machines of a group.

## 8. Watching for drift

`monk-watch-start` keeps a policy enforced. Local paths use `filenotify`; remote machines are polled. In `assisted` mode a change produces a plan in `*monk*` with `drift:` and the path in the header; in `autopilot` it repairs. Some sandboxes report a notification library and deliver nothing, so check first:

```elisp
(monk-watch-notifications-work-p)
```

Start the watch, then edit the file:

```elisp
(progn (setq monk-watch-mode 'assisted)
       (monk-watch-start tutorial-m #'tutorial-policy-1)
       (monk-watch-status))

(with-temp-file (expand-file-name ".config/app.conf" tutorial-home)
  (insert "drifted\n"))
```

Within a second `*monk*` shows a `PLAN` for `app.conf` alone; the other resources are skipped as not selected. The header carries a green `WATCH` badge; `W` switches to `AUTOPILOT` and the badge turns amber; `w` opens the event log. Stop before the next lesson:

```elisp
(monk-watch-stop)
```

## 9. Events and the agent

Every result serializes to one JSON object per line. This is the ledger a standalone agent writes, the stream a controller reads, and the shape other configuration tools normalize to:

```elisp
(mapcar #'monk-event-line
        (monk-run-events (monk-converge tutorial-m #'tutorial-policy-1)))
```

The standalone agent, `emacs --batch -f monk-agent-main`, converges on start, watches in autopilot, converges in full on a timer, and appends to that ledger. Its query endpoint, socket-activated by systemd, answers the same way:

```elisp
(let ((monk-agent-ledger-file (expand-file-name "state/events.ndjson" tutorial-home)))
  (monk-agent-record (monk-converge tutorial-m #'tutorial-policy-1))
  (with-output-to-string (monk-agent-handle '((op . "status")))))
```

`(monk-agent-service-unit)` and `(monk-agent-socket-unit)` return the unit files a policy installs with `(monk-agent-units)`.

## 10. Your own resource

A resource is a class with a check and a repair. Check must not change the machine; it returns nil when the state holds, or a plist of changes. Repair applies the plist and returns a comment. The constructor calls `monk-add-resource`. This one keeps a file at exactly N lines:

```elisp
(defclass tutorial-lines-resource (monk-resource)
  ((count :initarg :count :type integer)))

(cl-defmethod monk-check ((r tutorial-lines-resource) (m monk-machine))
  (let* ((text (or (monk-file-read m (oref r name)) ""))
         (have (length (split-string text "\n" t))))
    (unless (= have (oref r count))
      (list :lines (list have (oref r count))))))

(cl-defmethod monk-repair ((r tutorial-lines-resource) (m monk-machine) changes)
  (monk-file-write m (oref r name)
                   (mapconcat (lambda (i) (format "line %d" i))
                              (number-sequence 1 (oref r count)) "\n"))
  (format "%d -> %d lines" (car (plist-get changes :lines)) (oref r count)))

(defun tutorial-lines (path count &rest args)
  (monk-add-resource
   (apply #'tutorial-lines-resource :name path
          (monk--resource-plist args :count count))))

(monk-converge tutorial-m (lambda (_m) (tutorial-lines "~/ten" 10)))
```

Add `monk-diff` for a unified diff in the report, `monk-resource-paths` so the watcher knows which files to watch, and `monk-default-watch-action` to give it an action for `:watch`.

That is every feature. The README has the resource catalog; `docs/mgmt-review-and-interop.md` has the design notes. `M-x monk-testdrive` runs all of this automatically and reports; `M-x monk-testdrive-tour` shows the visuals one at a time.
