# Monk

Monk is a configuration management system. Policy is Emacs Lisp. Remote access is TRAMP. There is no server, no agent, and no template language.

## Model

A policy is a function of one argument, the machine. The function calls resource constructors. Monk collects the resources, then checks and repairs each one on the machine. This is the Chef compile and converge split.

Each resource ends in one of six states. `kept` means the machine already matched. `repaired` means Monk changed it. `would-repair` means a dry run found a difference. `partial` means some of the resource holds and the rest failed. `failed` means the repair raised an error. `skipped` means a guard, class, or requisite did not match. These are the CFEngine promise outcomes.

Each result carries the machine, the resource name, its type, a degree from 0 to 1, the changes as a plist, a comment, and a unified diff for file content. The degree says how much of the resource holds. A package set with 121 of 123 packages installed that fails to install the rest reports `partial` with degree 0.98. Dry run returns the same shape as a real run. This is the Salt result shape.

Conditions are classes. Facts produce hard classes such as `darwin`, `debian`, `ubuntu`, `noble`, `arm64`. Inventory variables with the value `t` produce soft classes such as `home_leader`. Group names produce classes too. A resource with `:classes '(debian)` runs only on Debian-family hosts.

Richer conditions are CELL guards. See [Guards: CELL and CEL](#guards-cell-and-cel).

Monk runs passes until a pass repairs nothing, with a cap of `monk-max-passes`. A second pass after repairs confirms that the repairs held. A resource that repairs in more than one pass is flapping: a repair that held would be kept afterwards, so a second repair means another resource undid it, or its check and repair disagree. The passes need not be adjacent. With A undoing B, B undoing C, and C undoing A, each repairs every third pass. The classic two-resource case is one that installs a package and one that removes it. The run stops at the second repair of any resource, the report names the flapping resources, and `monk-run-flapping` returns their keys. `monk-max-passes`, default 6, bounds the cycle length Monk can name. When the cap is reached with repairs still happening and no single resource flaps, the report says `did not converge` and `monk-run-oscillating-p` returns non-nil. `monk-run-converged-p` is true only when neither happened.

A failure never stops a run. This is the Salt execution model, not the Ansible one. A failed resource is a result with status `failed`, the pass goes on to the next resource, and only resources that `:require` or `:watch` the failed one skip because of it, with a comment that names the unsatisfied ref. Everything unrelated converges. `monk-run-ok-p` is nil when any result failed.

## Requisites

Requisites order resources and gate them on results, as in Salt. Every constructor accepts four of them.

| Keyword | Effect |
|---|---|
| `:require REFS` | Run after REFS, and only when they succeeded. |
| `:watch REFS` | Like `:require`. When a ref changed, run the watch action after the check. A systemd unit restarts. A command runs. Set `:watch-action` to override. |
| `:onchanges REFS` | Run only when a ref changed in this run. |
| `:onfail REFS` | Run only when a ref failed in this run. |

A ref names a resource on the same machine, on another machine, or on a group:

```elisp
:require '("~/.ssh")                       ; same machine
:require '((kamen . "/etc/exports"))       ; host kamen
:require '((unix . "/etc/exports"))        ; every host in group unix
:require '((:ref (unix . "member") :min 2)) ; at least 2 hosts in unix
:require '((:ref (unix . "member") :fraction 0.5))
:require '((:ref "pkgs" :degree 0.9))      ; pkgs holds to 90 percent
```

Without a threshold, every matching resource must succeed. A ref that matches no resource fails the node. A cycle signals `monk-requisite-error` before anything runs.

Cross-machine refs resolve only inside `monk-orchestrate`, which compiles the policy for a set of machines and runs all resources as one graph. Machines interleave in inventory order where no requisite says otherwise. `monk-converge` runs one machine, so a ref to another machine matches nothing there.

A command without `:creates` or `:unless` is always kept. With `:watch` it becomes a handler that runs only when a watched resource changed.

## Example

```elisp
(require 'monk)
(require 'monk-file)
(require 'monk-package)

(defun my-policy (m)
  (monk-directory "~/.ssh" :mode #o700)
  (monk-symlink "~/.zshrc" "~/dotfiles/zshrc")
  (monk-line-in-file "/etc/sudoers" "me ALL=(ALL) NOPASSWD: ALL"
                     :regexp "^me " :sudo t)
  (monk-file "/etc/myapp.conf" :content "level=3\n" :sudo t)
  (monk-systemd-unit "myapp.service" :watch '("/etc/myapp.conf"))
  (when (monk-class-p m 'darwin)
    (monk-brew "tools" '("jq" "ripgrep"))))

;; Plan, then apply.  The plan is a dry run.  Apply repeats the same
;; policy on the same machines and compares the outcome with the plan.
(let ((plan (monk-plan (monk-local-machine) #'my-policy)))
  (monk-run-report plan)
  (monk-run-report (monk-apply plan)))

;; Remote host.
(monk-run-report (monk-converge (monk-machine :name "pi" :address "pi.local" :user "pi")
                                #'my-policy))

;; Several hosts as one graph, so cross-machine requisites resolve.
(monk-run-report (monk-orchestrate (monk-inventory-select 'home) #'my-policy))

;; Keep it enforced.  Local drift fires within a second; remote hosts
;; are polled.  Plans show up in *monk*; press a there, or set
;; monk-watch-mode to autopilot.
(monk-watch-start (monk-inventory-select 'home) #'my-policy)
```

## Guards: CELL and CEL

`:when GUARD` runs a resource only when GUARD is true. GUARD is a CELL form or a CEL string. CELL is CEL in Lisp: a guard language over the classes, facts, inventory variables, and groups of a machine. A CELL form is data. It has no side effects, it always terminates, and Monk evaluates it on the controller without a connection to the host. Because it is data, Monk checks it when the policy compiles, and a skipped resource reports the clause that was false, for example `class linux is absent` or `(> (var disk) 100) is false: (var disk) = 50, 100 = 100`.

```elisp
(monk-symlink "~/.config/niri/config.kdl" "~/autodist/files/niri.kdl"
  :when '(and (class linux) (= (fact arch) "x86_64") (var home_leader)))
```

The forms are `(class SYM)`, `(group SYM)`, `(fact KEY)`, `(var KEY)`, `(name)`, `(local)`, the whole collections `(classes)`, `(groups)`, `(facts)`, `(vars)`, presence with `(has (fact KEY))` and `(has (var KEY))`, the logic `and`, `or`, `not`, `if`, the comparisons `=`, `/=`, `<`, `<=`, `>`, `>=`, `in`, the string tests `starts-with`, `ends-with`, `contains`, `match`, and `size`, `list`, `+`, `-`, `*`, `/`, `%`. Strings, numbers, `t`, and `nil` are literals. `:classes LIST` is sugar for `(and (class A) (class B) ...)` and combines with `:when`.

CEL is Google's Common Expression Language. A string guard is CEL. Monk translates it to CELL once, at compile time, with `monk-cel-parse`. In CEL, `classes` and `groups` are lists, `facts` and `vars` are maps, `name` is a string, and `local` is a bool. The functions `has()`, `size()`, `startsWith()`, `endsWith()`, `contains()`, and `matches()` work. `'linux' in classes` becomes `(class linux)`, so the report can name the class.

```elisp
(monk-brew "gui" '("ghostty") :when "'darwin' in classes && facts.arch == 'arm64'")
```

`monk-cell-to-cel` prints a CELL form as CEL with the fewest parentheses that CEL precedence allows, and the output parses back to an equal form. `monk-cell-explain MACHINE FORM` returns why a guard is true or false. `monk-cell-select GUARD MACHINES` returns the machines that match. `monk-cell-matches` keeps the result or error per machine.

`:only-if FN` and `:not-if FN` stay for guards that must run code on the host. Their skip comment only names the function, because Monk cannot look inside it.

`M-x monk-ui-query`, or `/` in `*monk*`, asks for a guard and shows every inventory machine with its verdict, the explanation, and its facts folded under it. `e` edits the guard, `g` gathers facts over the network for machines that have none, `TAB` unfolds a machine, `q` closes. A guard that starts with a parenthesis is CELL; anything else is CEL.

## Paths

Paths are interpreted on the target machine. A leading `~` is the home of the login user, or of root under `:sudo`. File primitives expand it through TRAMP. A symlink target with a leading `~` is expanded on the target through the `home` fact before the link is made, because a literal `~` in a link is dangling. Inside a policy, `(monk-expand "~/x")` returns the same expansion for any other use, such as a command argument.

Batch:

```sh
emacs --batch -L lisp -l my-policy.el --eval '(monk-batch-converge "my-policy.el")'
```

The policy file must set `monk-batch-policy` to the policy function.

## Resources

Every constructor accepts these keywords. `:only-if FN` runs the resource only when `(funcall FN machine)` is non-nil. `:not-if FN` skips it when non-nil. `:classes LIST` runs it only when the machine has every class in the list. `:when GUARD` runs it only when the CELL form or CEL string GUARD is true. `:require`, `:watch`, `:onchanges`, and `:onfail` are described above. `:sudo BOOL` performs the resource as root through a TRAMP sudo hop.

File resources in `monk-file.el`:

| Constructor | Purpose |
|---|---|
| `(monk-directory PATH &key mode owner group)` | Directory exists with mode and ownership. |
| `(monk-symlink PATH TARGET &key force)` | Symlink at PATH points to TARGET. Creates parent directories with mode 0700. |
| `(monk-file PATH &key content source mode owner group)` | File has exactly the given content or a copy of a controller-side file. Reports a diff. |
| `(monk-line-in-file PATH LINE &key regexp state insert-after insert-before create)` | One line present or absent, in the style of Ansible `lineinfile`. |
| `(monk-absent PATH)` | Path does not exist. |

System resources in `monk-system.el`:

| Constructor | Purpose |
|---|---|
| `(monk-cron NAME SCHEDULE JOB &key user state)` | Crontab entry marked by a `#Monk: NAME` comment. |
| `(monk-git PATH REPO &key branch update)` | Clone, checkout branch, fast-forward. The check uses `git ls-remote`, so it reads the network but does not touch the checkout. |
| `(monk-user-group USER GROUP)` | Supplementary group membership. |
| `(monk-sysctl NAME VALUE)` | Live value and a line in `/etc/sysctl.d/90-monk.conf`. |
| `(monk-systemd-unit NAME &key content enabled started user-unit)` | Unit file, enablement, and active state. Supports actions `restart`, `reload`, `stop`, `start`, `daemon-reload`. |
| `(monk-command NAME PROGRAM &key args creates unless)` | Run a command unless a path exists or a probe command succeeds. Supports action `run`. |

Each resource type declares the paths it owns through `monk-resource-paths`, for the watcher. Directories own their subtree (`monk-resource-owns-subtree-p`). A git checkout owns only its `.git`, so edits inside the tree do not count as drift.

Package and macOS resources in `monk-package.el`:

| Constructor | Purpose |
|---|---|
| `(monk-brew NAME PACKAGES &key cask)` | Missing Homebrew formulae installed in one call. When brew refuses a formula from an untrusted tap, Monk runs `brew trust TAP` once and retries. Class `darwin`. |
| `(monk-brew-tap NAME)` | Homebrew tap present and trusted, where brew has `trust`. Class `darwin`. |
| `(monk-apt NAME PACKAGES)` | Missing Debian packages installed in one call. Class `debian`. |
| `(monk-osx-default DOMAIN KEY VALUE &key type)` | macOS user default. Types `string`, `int`, `float`, `bool`, `array`. Class `darwin`. |

Bridge resources in `monk-bridge.el` call another tool installed on the target. Each call starts a Python interpreter, so use them for modules Monk lacks.

| Constructor | Purpose |
|---|---|
| `(monk-ansible NAME MODULE ARGS &key become)` | One Ansible module through `ansible localhost -c local -m MODULE -a ... -o`. Check uses `--check --diff`. ARGS is an alist. A module without check mode counts as changed. |
| `(monk-salt NAME FUNCTION ARGS)` | One Salt state through `salt-call --local --out=json state.single FUNCTION name=NAME ...`. Check uses `test=True`. |

Both parse JSON and map `changed`, `failed`, `result`, `changes`, and `comment` onto Monk results. Neither is verified against a live install in this tree. The parsers are tested against captured output shapes.

## Inventory

`monk-inventory.el` holds a group tree as Lisp data. Variables merge from the root down.

```elisp
(monk-inventory-define
 '(world
   :vars ((observer . nil))
   :children
   ((home
     :vars ((mine . t))
     :children
     ((macs :hosts ((foxy :vars ((home_leader . t)))
                    (moi :address "moi.local")))
      (unix :hosts ((kamen :address "kamen.local" :user "pi"))))))))

(monk-converge-all 'self #'my-policy :dry-run t)   ; the local host by name
(monk-converge-all 'unix #'my-policy)              ; a group
(monk-converge-all '(foxy kamen) #'my-policy)      ; a list
```

A host with no `:address` is the local machine. `self` selects the host whose name matches `system-name`.

## Plan and apply

`monk-plan` is a dry run. It returns a run whose results say `would-repair` where a change is needed and `kept` where none is. Nothing on the machine changes. `monk-apply` takes that run, repeats its policy on its machines for real, and links the plan to the new run. `monk-run-compare` then gives one verdict per resource.

| Verdict | Meaning |
|---|---|
| `as-planned` | Plan and apply agree: both changed it, or both left it alone, or both failed on it. |
| `unfulfilled` | The plan said it would change. Apply found it already in place. Someone or something else made the change between plan and apply. |
| `unplanned` | The plan said it was kept. Apply changed it. The machine drifted between plan and apply. |
| `failed-planned` | Apply failed where the plan expected success. |
| `new`, `missing` | The policy compiled differently between plan and apply, for example because a guard read a fact that changed. |

`monk-run-as-planned-p` is true only when every verdict is `as-planned`. The text report prints the verdict counts and lists every resource that is not as planned.

## Watching for drift

`monk-watch.el` keeps a policy enforced between runs. `(monk-watch-start MACHINES POLICY)` compiles the policy, asks every resource which paths it owns through `monk-resource-paths`, and waits for changes to them.

Local machines have two backends, chosen by `monk-watch-backend`. On macOS with `fswatch` installed (`brew install fswatch`), Monk runs one `fswatch --recursive --print0` over every parent directory of an owned path and reads the NUL-separated paths through a process filter. FSEvents is recursive and reports writes, so one process covers everything and a file created later needs no new watch. Elsewhere, and on macOS without `fswatch`, Monk uses `filenotify.el` on the parent directory of each owned path, because a directory watch sees creates, deletes, and renames, which a file watch misses. On kqueue a directory watch does not report writes to the files inside it, so Monk also watches every owned file that exists and adopts a file the moment its directory reports it created. `fswatch` is the better choice on a Mac: kqueue costs one file descriptor per watched file. An event maps to the resources that own the path, and to the resources that require or watch those, transitively. Monk then runs only that set with `:only`; the rest of the policy compiles so requisites resolve, and is skipped. A burst of events within `monk-watch-debounce` seconds makes one run. Events during a run and for `monk-watch-settle` seconds after are dropped, because Monk's own repairs also touch watched paths.

Remote machines have no notification channel over TRAMP. Monk polls them: every `monk-watch-poll-interval` seconds, default 300, a dry run finds resources that would change, and those count as drift.

`monk-watch-mode` chooses the reaction. `assisted`, the default, plans the affected resources and shows the plan in `*monk*` for you to apply. `autopilot` repairs them. `W` in `*monk*` toggles the mode. The header shows a green `WATCH` or an amber `AUTOPILOT` badge while a watch is active, and a plan or apply that a watch triggered says `drift:` and the path that fired. `w` opens `*monk-watch*`, the event log. `monk-watch-stop` ends the watch.

A compiled-in notification library does not guarantee delivery. Sandboxes and some network filesystems report `inotify` and deliver nothing. `monk-watch-notifications-work-p` writes a probe file and waits for its event; `monk-watch-start` logs a warning when the probe fails, and local drift then goes unseen until the next manual run.

## Execution modes

Monk runs in three modes that share one engine and differ in who decides.

| Mode | Runs where | Approves a repair | Report |
|---|---|---|---|
| Standalone | `emacs --batch -f monk-agent-main` on the host, from systemd or launchd | nobody: local policy applies on drift and on a timer | ledger file and journald; a central Monk observes by tailing it |
| Supervised | the central Emacs, over TRAMP | a human, through the plan buffer and `a` | `*monk*` |
| Autopilot | the central Emacs, over TRAMP, driven by `monk-watch` | nobody | `*monk*` and the watch log |

Supervised and autopilot are `monk-watch-mode`. Standalone is `monk-agent.el`. The agent converges the local policy on start, watches owned paths in autopilot, and converges in full every `monk-agent-full-interval` seconds (3600). It appends every event to `monk-agent-ledger-file` as JSON Lines and mirrors them to journald through `systemd-cat` when present. `(monk-agent-observe MACHINE)` on the central Monk runs `tail -F` on that ledger through TRAMP and dispatches each parsed event to `monk-agent-observe-functions`; there is no port and no daemon beyond SSH. The agent and a controller applying the same policy do not fight: both apply the same policy, and the settle window drops the events the other side produced.

### systemd units

`(monk-agent-units)` inside a policy installs and enables two units on a Linux host. `monk-agent.service` is the long-lived loop; a drift watcher must stay resident, so it is not socket activated. `monk-agent.socket` plus `monk-agent@.service` is an on-demand query endpoint on `/run/monk/agent.sock` with `Accept=yes`: systemd accepts each connection and starts one `emacs --batch -f monk-agent-serve` with the connection on stdin and stdout, inetd style, because Emacs cannot adopt an inherited listening socket but reads and writes stdio well. Idle cost is zero. `:user t` installs user units under `~/.config/systemd/user/` with the socket in `$XDG_RUNTIME_DIR/monk/`. Set `monk-agent-emacs`, `monk-agent-load-path`, and `monk-agent-policy-file` before compiling the policy so the `ExecStart` lines point at the right places on the target.

The endpoint takes one JSON object per line and answers in JSON Lines. `{"op":"status"}` returns liveness and the last `run-end`. `{"op":"tail","n":50}` returns the last ledger lines. `{"op":"plan"}` and `{"op":"run"}` converge now and stream one result event per node, then `run-end`. From the controller, `(monk-agent-status M)`, `(monk-agent-remote-run M DRY-RUN)`, and `(monk-agent-query M REQUEST)` send the request through `socat` over TRAMP; no port is opened.

## etcd

`monk-etcd.el` is a small etcd v3 client over the gRPC gateway (HTTP and JSON with base64 keys), enough for shared state: strings, per-host string maps, exported resources, run events, and agent presence with a lease. Where the value format allows, the key layout is mgmt's, so one etcd can serve both: `/_mgmt/strings/KEY` and `/_mgmt/strings/KEY/HOST` are byte-compatible with mgmt's `world.StrGet` and `StrMapGet`. mgmt's exported resources under `/_mgmt/exported/` are Go `gob` blobs, which Emacs cannot decode, so Monk keeps its own JSON under `/_monk/`: `exported/TO/FROM/TYPE/NAME` for `monk-etcd-export` and `monk-etcd-collect`, `events/HOST/RUN/SEQ` for `monk-etcd-publish-run` and `monk-etcd-run`, `agents/HOST` for `monk-etcd-announce` and `monk-etcd-agents`. `monk-etcd-watch PREFIX CALLBACK` streams puts and deletes over one raw HTTP connection. Set `monk-agent-etcd` to make the standalone agent publish every run and keep a presence key alive. Tested against a fake gateway in the suite; not yet against a real etcd.

## Events

`monk-event.el` serializes results as one JSON object per line. This is the ledger format, and it is the shape every other configuration tool normalizes to.

```json
{"rgi":1,"event":"result","ts":"2026-09-26T10:00:01.234Z","host":"kamen","run":"r20260926T100001.000",
 "pass":1,"type":"file","name":"/etc/motd","status":"repaired","changed":true,"degree":1.0,
 "changes":{"content":true},"diff":"--- a/etc/motd\n+++ b/etc/motd\n...","comment":"content",
 "duration_ms":4.5,"dry_run":false,"native":{"tool":"monk"}}
```

`event` is `run-start`, `result`, `run-end`, or `drift`. `status` is Monk's vocabulary plus `unknown`; `native` carries a foreign tool's own status verbatim. Fields are added, never renamed; `rgi` is the format version. `monk-run-events` produces the list for a run, `monk-events-to-run` rebuilds a run from parsed lines. `docs/mgmt-review-and-interop.md` explains why a shared event protocol is realistic, a shared resource graph is realistic for import only, and a shared language is not.

## Report

`monk-ui.el` draws the run. Load it and `monk-run-report` shows a `*monk*` buffer with one row per machine: name, state badge, a stacked progress bar, counts, and time. The bar has one layer per status, failures on the left edge. While a run is in progress the row of the current machine shows a spinner and the bar grows as nodes finish. `RET` on a row opens a detail buffer with results grouped by status and with fontified diffs. The detail buffer starts with the classes, groups, facts, and vars of the machine, folded. `TAB` on that line unfolds it. `i` on a row opens the same data on its own, unfolded. `/` asks for a CELL or CEL guard and shows which inventory machines match. `.` or `M-.` jumps to the source: on a machine row or heading, the inventory file at that host; on a result line in a detail buffer, the policy file at that resource. `M-,` comes back. Each resource remembers the file its policy was defined in, and each machine the file that defined the inventory; the event record carries the policy file as `native.source`. `t` switches between SVG and text drawing. `g` runs the same thing again: a plan plans again, an apply applies again against its plan. `C-u g` only redraws. `q` closes any Monk buffer. `?` lists the keys. After a run you started, point moves into `*monk*` so the keys work at once. A run that a watch triggered only displays the buffer. `monk-ui-select-window` changes this.

A plan and an apply look different. A plan has a purple `PLAN` badge in the header, a purple header face, a `Would` column instead of `Changed`, and the hint `a to apply`. Pressing `a` asks for confirmation with the machine names and the number of changes, runs `monk-apply`, and replaces the plan in the buffer with the apply report. An apply from a plan has a blue `APPLY from plan` badge, a `Plan` column with one cell per machine that says `as planned` or lists the verdicts that differ, and a verdict strip in the header. `p` opens the plan an apply came from. The detail buffer of such an apply starts with a `differs from plan` section that pairs each drifted result with what the plan said.

Bars and the spinner are SVG on a graphical display with SVG support, and block glyphs elsewhere. Status glyphs fall back to ASCII on a terminal that cannot show them. Set `monk-ui-style` to `text` to force text and `monk-ui-unicode` to nil for ASCII. All colors come from faces with light and dark variants, so themes apply. In batch mode the report prints as text.

Progress events go through `monk-progress-functions`. Add your own function there to drive another display.

## Writing a resource

Subclass `monk-resource`. Implement `monk-check`, which inspects the machine and returns nil or a plist of changes, and `monk-repair`, which applies the plist and returns a comment. Optionally implement `monk-diff` for a unified diff and `monk-action` for notifications. `monk-check` must not change the machine. Add a constructor function that calls `monk-add-resource`.

Use `monk-exec` for commands. It returns a struct with exit code and output. `monk-exec-string` signals `monk-exec-error` on non-zero exit. Trailing keyword pairs: `:sudo t` to run as root, `:input STRING` for standard input, `:directory PATH` to run in a directory on the target. Use `monk-machine-path` to turn a target path into a controller-side path for file primitives.

A set resource, one that manages several members, returns `:degree` in its change plist: the fraction of members that already hold. A failed repair then reports `partial` with that degree, and requisites with `:degree` can accept it. Implement `monk-default-watch-action` to give the resource an action for `:watch`.

## Facts

`monk-gather-facts` runs the probes in `monk-fact-probes`. The default probes give `kernel`, `arch`, `nodename`, `hostname`, `os-family`, `distribution`, `distribution-release`, `distribution-version`, `home`, and `user`. Register a probe with `monk-register-fact-probe`.

## Test drive, tour, tutorial

`M-x monk-testdrive` runs every feature against a throwaway home and reports in `*monk-testdrive*`: converge, plan and apply, requisites, flapping, drift watching, the agent ledger and query handler, the report buffers. Every check is done by code that reads files, run results, and the text and properties of the `*monk*` buffers; nothing asks you anything while it runs. `C-u` runs one section. `M-x monk-testdrive-tour` is for your eyes: it sets up one thing at a time with nothing running underneath and asks whether it looked right, for the plan header and bars, the diff in the detail buffer, the apply comparison, the flapping badge, the watch badge, and the text fallback.

`M-x monk-tutorial` opens ten lessons beside a sandbox home. `C-c C-c` runs the form at point against the sandbox and shows the result in `*monk*`. The lessons cover the same ground as the drive, one feature at a time, and end with writing a resource of your own. `docs/tutorial.md` is the same content as prose. The test suite evaluates every lesson form, so the lessons cannot rot.

## Development

```sh
make compile   # byte-compile with warnings as errors
make test      # run ERT tests
make check     # checkdoc
```

`M-x monk-reload` reloads every loaded `monk-*` library from source in first-load order and fixes what a plain `load` leaves behind: keymap variables are unbound first and re-installed in live `*monk*` and tutorial buffers, an active watch is stopped and started again on the new code with the same policy and mode, the image cache is cleared, and the notification probe is reset. EIEIO classes redefine in place. A run already stored in `*monk*` keeps its old struct layout; the summary line says so, and rerunning refreshes it. `monk-reload-last-failures` holds any file that did not load.

Tests run against a temporary directory on the local host. Package, sudo, and bridge resources are tested through their pure helpers and a faked exec layer. Cross-machine tests use several local machine objects with different names.

## Not yet done

Docker containers. Package removal. A `monk-user` resource that creates accounts. Homebrew cask listing on older `brew`. Batched remote checks to cut TRAMP round trips. Parallel machines inside `monk-orchestrate`. A persistent ledger so a single-machine run can read results from an earlier run on another machine.
