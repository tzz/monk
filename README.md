# Monk

Monk is a configuration management system. Policy is Emacs Lisp. Remote access is TRAMP. There is no server, no agent, and no template language.

## Model

A policy is a function of one argument, the machine. The function calls resource constructors. Monk collects the resources, then checks and repairs each one on the machine. This is the Chef compile and converge split.

Each resource ends in one of six states. `kept` means the machine already matched. `repaired` means Monk changed it. `would-repair` means a dry run found a difference. `partial` means some of the resource holds and the rest failed. `failed` means the repair raised an error. `skipped` means a guard, class, or requisite did not match. These are the CFEngine promise outcomes.

Each result carries the machine, the resource name, its type, a degree from 0 to 1, the changes as a plist, a comment, and a unified diff for file content. The degree says how much of the resource holds. A package set with 121 of 123 packages installed that fails to install the rest reports `partial` with degree 0.98. Dry run returns the same shape as a real run. This is the Salt result shape.

Conditions are classes. Facts produce hard classes such as `darwin`, `debian`, `ubuntu`, `noble`, `arm64`. Inventory variables with the value `t` produce soft classes such as `home_leader`. Group names produce classes too. A resource with `:classes '(debian)` runs only on Debian-family hosts.

Richer conditions are CELL guards. See [Guards: CELL and CEL](#guards-cell-and-cel).

Monk runs passes until a pass repairs nothing, with a cap of `monk-max-passes`. A second pass after repairs confirms that the repairs held. A resource that repairs in more than one pass is flapping: a repair that held would be kept afterwards, so a second repair means another resource undid it, or its check and repair disagree. The passes need not be adjacent. With A undoing B, B undoing C, and C undoing A, each repairs every third pass. The classic two-resource case is one that installs a package and one that removes it. The run stops at the second repair of any resource, the report names the flapping resources, and `monk-run-flapping` returns their keys. `monk-max-passes`, default 6, bounds the cycle length Monk can name. When the cap is reached with repairs still happening and no single resource flaps, the report says `did not converge` and `monk-run-oscillating-p` returns non-nil. `monk-run-converged-p` is true only when neither happened.

A failure never stops a run. This is the Salt execution model, not the Ansible one. A failed resource is a result with status `failed`, the pass goes on to the next resource, and only resources that `:require` or `:watch` the failed one skip because of it, with a comment that names the unsatisfied ref. Everything unrelated converges. `monk-run-ok-p` is nil when any result failed. Two opt-in exceptions: `:fatal t` on a resource ends the pass on its machine when it fails, and other machines go on (Salt's `failhard`, per resource); `:handled t` on an `:onfail` handler makes the failures it handles count as rescued when the handler succeeds, so `monk-run-ok-p` stays true (`monk-run-rescued` lists them).

## Requisites

Requisites order resources and gate them on results, as in Salt. Every constructor accepts six of them.

| Keyword | Effect |
|---|---|
| `:require REFS` | Run after REFS, and only when they succeeded. |
| `:watch REFS` | Like `:require`. When a ref changed, run the watch action after the check. A systemd unit restarts. A command runs. Set `:watch-action` to override. |
| `:listen REFS` | Like `:watch`, but the action runs once, after the run converged, however many refs changed in however many passes. Five config files change, the service restarts once. |
| `:onchanges REFS` | Run only when a ref changed in this run. |
| `:onfail REFS` | Run only when a ref failed in this run. |
| `:prereq REFS` | Run before REFS, and only when one of them would change. Monk checks the refs without repairing them to decide. Stop a service before its binary changes. A ref that is itself skipped does not count. |

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

## Declared parameters

Every constructor is defined with `monk-define-resource`, which declares its positional and keyword parameters with a type, a default, choices, aliases, and a one-line doc, the way an Ansible module declares its `argument_spec`. A call is checked when the policy compiles: an unknown keyword, a wrong type, a value outside the choices, a missing required parameter, or two parameters that exclude each other signals `monk-argument-error` with the parameter list, for example `Unsupported parameter for monk-directory: :mdoe.  Supported parameters include: :group, :mode, :owner`. An explicit nil for an optional parameter means not given. The same declaration writes the constructor's docstring and fills `monk-resource-specs`, which the documentation and the importers read.

## Unreachable machines

A machine that does not answer is not a failed policy. When a connection fails, at fact time or during a resource, every remaining resource on that machine gets the status `unreachable` (Ansible's `dark`) without being tried, resources on other machines that require one of them skip with `HOST is unreachable`, and the run's `monk-run-unreachable` lists the machine and the first line of the error. Unreachable results have their own purple glyph and count and stay out of the degree math. `monk-run-ok-p` is nil unless the machine has `ignore-unreachable`, set by the inventory variable `ignore_unreachable`, for a laptop that comes and goes. Batch entry points exit with Ansible's bit-safe codes from `monk-run-exit-code`: 0, 2 when something failed, 4 when a machine was unreachable, 6 for both; the run-end event carries `exit_code` and `unreachable`.

## Run controls

`:changed-when` and `:failed-when` on `monk-command` take a function of the `monk-exec` or a CELL form over `(rc)`, `(stdout)`, and `(stderr)`. With `:changed-when` the command runs on every pass as a query and is repaired only when the predicate holds, so a command can have a real check without `:creates`. `:failed-when` replaces the exit-code rule in both directions. A predicate that errors fails the resource with the reason. Ansible's `changed_when` and `failed_when`.

`:no-log t` on any resource replaces the result's changes, diff, and comment with `[censored: no-log]` before anything records or shows them: the ledger, the history, etcd, the socket, the reports, and the log. Status and degree stay. A `line-in-file` with `:no-log` also hashes the line out of its resource name. Ansible's `no_log`.

`:tags '(ssh slow)` on any resource lets a run select by name: `(monk-plan machines policy :tags '(ssh) :skip-tags '(slow))`. The rules are Ansible's `evaluate_tags`: `always` runs unless a skip names it or `all`, `never` runs only when named, `all`, `tagged`, and `untagged` select by presence of tags. The selection feeds `:only`, so requisites still pull in what a selected resource needs and everything else compiles and reports `skipped`. A selection that matches nothing runs nothing. `monk-check-policy` lists the tags in use.

`:serial` on `monk-orchestrate` rolls a change out in batches: `:serial 1`, `:serial "25%"`, or a list such as `'(1 "25%" "100%")` whose last entry repeats. The graph is one, so a resource in a later batch can `:require` one from an earlier batch, but a batch runs to convergence before the next starts. `:max-fail-percentage 20` is the circuit breaker: when more than that share of a batch's machines failed or were unreachable, the remaining batches do not start and their resources report `skipped` with `an earlier batch failed past :max-fail-percentage`. Failures count per batch, not cumulatively, which departs from Ansible on purpose (its cumulative count is an accident of the code). The run records `monk-run-batches` and `monk-run-stopped-at-batch`; the `*monk*` row shows `b2`, or `b3×` for a batch that never started, and the header says where it stopped. A plan never stops.

`(monk-check-policy MACHINES POLICY)` compiles with no connection at all: machines use the facts they have, cached or none. It returns the nodes, every tag in use, per-machine compile errors such as a bad CELL form or an unknown keyword, and requisite refs that resolve to nothing. With `:tags`/`:skip-tags` it also returns the keys the selection would run. `monk-ui-check-policy` shows it in `*monk-check*`, with `.` to jump to a resource. `emacs --batch -l monk -f monk-batch-check POLICY-FILE` prints the same and exits 1 on a problem, for CI. Ansible's `--syntax-check`, `--list-tasks`, and `--list-tags`.


These come from Salt and apply to any resource.

`:retry PLIST` tries a failed check or repair again inside the same pass: `(:attempts 3 :interval 5 :backoff 2 :splay 1 :until success)`. `:attempts` is the total number of tries, `:interval` the wait before try two, each later wait is the previous one times `:backoff`, `:splay` adds up to that many random seconds, `:until` is `success`, `failure`, or a function of the result that says when to stop (Ansible's `until`). `:retry t` takes `monk-retry-defaults` (2 tries, 30 seconds, no backoff). `monk-retry-max-interval` caps one wait. The wait uses `sit-for`, so the display updates and `C-g` works; the run is still synchronous. A plan reports the retry policy and does not wait. The result comment says `attempt N of M` when more than one try ran.

`:verify` on `monk-command` runs after the command: a `(PROGRAM . ARGS)` list that must exit zero, or a function of the machine that returns nil or a reason. A failed verification fails the resource with the output as comment. Salt's `check_cmd`. Other resources need none, because the next pass re-checks them.

`:order 'first` or `:order 'last` breaks ties among resources the graph does not order. Requisites always win.

Each resource type can say when it cannot work on a machine (`monk-resource-available-p`): brew, apt, salt, and systemd-unit check for their program once per run and the resource is `skipped` with the reason, for example `brew is not on this machine`, instead of failing on its first command. Salt's `__virtual__`.

Each resource type can run something once per run per machine before its first resource (`monk-resource-class-init`): apt runs `apt-get update` (off with `monk-apt-update` nil), brew runs `brew update` and installs with `HOMEBREW_NO_AUTO_UPDATE=1`. Salt's `mod_init`.

Resources of one type on one machine with no requisite between them can run as one call (`monk-resource-aggregate`): several `monk-apt` or `monk-brew` sets install in one command, several `monk-salt` states run as one `state.high`. Each member still gets its own result, with its share of the changes and a comment that says it was aggregated. Salt's `mod_aggregate`.

`monk-run-context-get`, `monk-run-context-put`, and `monk-with-run-cache` hold values for the length of one run, keyed by anything. brew caches its installed list there and clears it after an install. Salt's `__context__`.

`(monk-orchestrate MACHINES POLICY :mock t)` compiles and orders the graph, evaluates requisites, and calls no resource: every result is `kept` with the comment `not called, mocked`. Use it in tests and to see the order before connecting anywhere. Salt's `mock=True`.

`monk-result-functions` is a hook called with every finished result and its run, for your own returner.

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

`(get X KEY...)` walks an alist by keys and gives nil when a key is absent, so `(get (fact local) monk role)` reads a nested local fact. In CEL the same is `facts.local.monk.role`. `(machine-fact HOST KEY)`, `(machine-var HOST KEY)`, and `(machine-class HOST SYM)` read another inventory machine, so one host can depend on what another is: `(= (machine-fact leader arch) (fact arch))`. They read the facts as known, live or cached, and never open a connection; a machine with no facts yields nil.

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

Each resource type declares the paths it owns through `monk-resource-paths`, for the watcher. A resource whose content is a tree, such as an rsync source, owns its subtree (`monk-resource-owns-subtree-p`). A `monk-directory` does not: it manages the directory's mode and owner, which a file written inside cannot change, so a scratch file under `~/.ssh` is not drift for it. A git checkout owns only its `.git`, so edits inside the tree do not count as drift.

Package and macOS resources in `monk-package.el`:

| Constructor | Purpose |
|---|---|
| `(monk-brew NAME PACKAGES &key cask rebuild)` | Missing Homebrew formulae installed in one call. When brew refuses a formula from an untrusted tap, Monk runs `brew trust TAP` once and retries. `:rebuild` is nil, t, or a function of the machine and the formula name that returns nil or a reason: an installed formula with a reason is built again with `brew reinstall`, falling back to `brew install --overwrite` when reinstall refuses; for formulae built from a moving branch, where brew's version never changes but the source does. A rebuild resource does not aggregate with others. Class `darwin`. |
| `(monk-brew-tap NAME)` | Homebrew tap present and trusted, where brew has `trust`. Class `darwin`. |
| `(monk-apt NAME PACKAGES)` | Missing Debian packages installed in one call. Class `debian`. |
| `(monk-osx-default DOMAIN KEY VALUE &key type)` | macOS user default. Types `string`, `int`, `float`, `bool`, `array`. Class `darwin`. |

Bridge resources in `monk-bridge.el` call another tool installed on the target. Each call starts a Python interpreter, so use them for modules Monk lacks.

| Constructor | Purpose |
|---|---|
| `(monk-rsync NAME SOURCES DEST &key archive recursive follow-links delete excludes options reachable)` | DEST mirrors SOURCES through rsync. Check is `rsync --dry-run --itemize-changes --stats`; the changed items are the changes and the degree is the share of files already in step. `:reachable HOST` probes the ssh port first and skips with `HOST is not reachable` instead of failing. Needs rsync. |
| `(monk-launchd LABEL &key plist loaded daemon)` | A launchd job on macOS: its plist under `~/Library/LaunchAgents` (or `/Library/LaunchDaemons` with `:daemon`) rendered from an alist by `monk-launchd-plist`, and its loaded state through `launchctl bootstrap`/`bootout`. The systemd-unit of macOS. |
| `(monk-ansible NAME MODULE ARGS &key become)` | One Ansible module. With `monk-ansible-library` set to a directory that holds the `ansible` Python package, the module runs directly: `python3 -m ansible.modules.MODULE` on the target with `{"ANSIBLE_MODULE_ARGS": ...}` on stdin and `PYTHONPATH` pointing at a zip of `module_utils`, `_vendor`, and `modules` that Monk builds once from the library and copies once to `~/.cache/monk` on each target. No controller Ansible, no inventory, no tmpdir; about 0.1s per call. Check mode passes `_ansible_check_mode` and `_ansible_diff`; the result's `diff` becomes structured entries (content or attributes), a non-zero exit without `failed` is failed, and `_ansible_new_secrets` values are masked. Without the library, the old path runs `ansible localhost -c local -m MODULE -a ... -o` on the target. Modules that live in an action plugin (`template`, `copy` with a local `src`, `fetch`, `package`, `service`) need the CLI path or a native Monk resource. |
| `(monk-salt NAME FUNCTION ARGS &key id)` | One Salt state through `salt-call --local --out=json`. Alone it uses `state.single`; independent salt resources on one machine aggregate into one `state.high` call with Salt's own requisites among them. Check uses `test=True`. `monk-salt-read-function` completes over `sys.list_state_functions`, cached per run. |

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

Derived groups follow the facts, like Salt's derived grains or Ansible's `group_by` and `keyed_groups`. `(monk-group 'big-disk "facts.disk > 100")` puts every machine whose guard is true into `big-disk`. `(monk-keyed-groups "facts.distribution" "distro")` makes one group per value, `distro_ubuntu`, `distro_macos`. Derived membership is recomputed whenever facts arrive, live or from the cache, and `monk-inventory-select` and `(group SYM)` see it like any other group. `monk-inventory-clear-derived` forgets the rules.

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

Local machines have two backends, chosen by `monk-watch-backend`. On macOS with `fswatch` installed (`brew install fswatch`), Monk runs two `fswatch --print0` processes and reads the NUL-separated paths through a process filter: one flat over the parent directories of owned files, one `--recursive` over owned subtrees (an rsync source tree; a managed directory is watched flat from its parent). The home directory that holds a few managed dotfiles is watched flat, never walked; walking it reported every application's scratch file under `~/Library`. FSEvents reports the changed file's own path, so a flat parent watch sees writes, creates, and deletes of the files in it. An event on a path no resource owns is counted and, for the first few, logged at info; `monk-watch-status` shows the count. An event maps to the resources that own the path, and to the resources that require or watch those or own the same path, transitively. Elsewhere, and on macOS without `fswatch`, Monk uses `filenotify.el` on the parent directory of each owned path, because a directory watch sees creates, deletes, and renames, which a file watch misses. On kqueue a directory watch does not report writes to the files inside it, so Monk also watches every owned file that exists and adopts a file the moment its directory reports it created. `fswatch` is the better choice on a Mac: kqueue costs one file descriptor per watched file. Monk then runs only that set with `:only`; the rest of the policy compiles so requisites resolve, and is left out. The summaries count what was left out apart from real skips: `0 skipped, 39 not selected` in the text, `partial: 18 of 57` on the terminal line. A burst of events within `monk-watch-debounce` seconds makes one run. Events during a run and for `monk-watch-settle` seconds after are dropped, because Monk's own repairs also touch watched paths.

Remote machines have no notification channel over TRAMP. Monk polls them: every `monk-watch-poll-interval` seconds, default 300, a dry run finds resources that would change, and those count as drift.

`monk-watch-mode` chooses the reaction. `assisted`, the default, plans the affected resources and shows the plan in `*monk*` for you to apply. `autopilot` repairs them. `W` in `*monk*` toggles the mode. The header shows a green `WATCH` or an amber `AUTOPILOT` badge while a watch is active, and a plan or apply that a watch triggered says `drift:` and the path that fired. `w` opens `*monk-watch*`, the event log. `monk-watch-stop` ends the watch.

A compiled-in notification library does not guarantee delivery. Sandboxes and some network filesystems report `inotify` and deliver nothing. `monk-watch-notifications-work-p` writes a probe file and waits for its event; `monk-watch-start` logs a warning when the probe fails, and local drift then goes unseen until the next manual run.

## Import from and export to Ansible and Salt

`<` in `*monk*` turns an Ansible playbook, role, or tasks file, or a Salt SLS, into a Monk policy for the machine at point; `>` turns the policy of the machine at point into a playbook or an SLS. Both ask which dialect. A language model does the mapping, through `monk-translate-ask-function`, by default efrit's `efrit-ask-once`: one request, off any session. Monk does the exact parts around it. The model receives the Monk catalog, generated from `monk-resource-specs` when you press the key, so it maps against the constructors, parameters, types, defaults, and choices that exist in this Emacs, not against what it remembers; the export side hands it each compiled resource as data (type, name, arguments, guard as CEL, requisites, tags), not Lisp. Anything with no native constructor is told to become `monk-ansible` or `monk-salt` with the original arguments, never an invented one.

Nothing the model returns runs. An import lands in `*monk-import*` as Lisp, is read as data, and is compiled with `monk-check-policy` against the machine, with no connection; `*monk-check*` shows the resources. When it does not compile, the errors and the previous answer go back to the model, up to `monk-translate-repair-rounds` (3) times, then the buffer is yours with the errors at the end. `C-c C-c` checks again after you edit, `C-c C-k` sends it back once more. An export lands in `*monk-export*` and is checked with `ansible-playbook --syntax-check` or `salt-call --local state.show_sls` when the tool is installed; `C-c C-c` checks again. For Salt with `salt-call` present, `monk-salt-insert-sls` below gives an exact import of the low state as `monk-salt` forms; the model then only has to make them native.

## Embark

`monk-embark.el` gives Embark two target types. A `monk-resource` is a resource name: the string at point in a policy file, the active region, or a path-like token at point in a comment or any other buffer (a path, or any name a compile has seen), or the result line at point in the report, detail, check, and query buffers. A `monk-machine` is a `*monk*` row, a machine heading, or an inventory host name in a Lisp buffer. `embark-act` on a resource offers `.` jump to the definition, `p` plan only it and its dependents, `a` apply the same, `h` its history, `w` what requires or watches it (a `*monk-dependents*` buffer where `.` jumps again), `r` rename it across the policy tree with `project-query-replace-regexp`. On a machine: `.` the inventory entry, `p` plan, `i` the inspector, `/` a guard query over it.

Names resolve through `monk-resource-index`, filled as a side effect of every compile with each resource's origin, type, machines, requisite refs, and tags. When a name is unknown and `monk-batch-policy` is set, the local policy is compiled once with no connection to fill it, so the first action in a fresh Emacs works. `monk-resource-origin-by-name`, `monk-resource-names`, and `monk-resource-dependents-by-name` are the API. Call `(monk-embark-setup)` in your embark configuration; it also runs when Embark loads after Monk.

## Detached jobs

Some policies take too long for the session that asks, or replace its binary: a Homebrew rebuild of Emacs itself. A running process keeps its mapped binary when the file is replaced, so the session survives; it must not block on the build. `monk-detach.el` writes a one-shot shell script under the Monk state directory that optionally waits for the asking Emacs to exit, runs a batch Emacs with the same load path and policy file, logs to `detach-NAME.log`, and sends a desktop notification at the end (`osascript` or `notify-send`). `(monk-detach NAME POLICY-FILE FUNCTION t)` writes and starts it; the function on the batch side calls `monk-detach-run`, which converges with terminal progress and exits with the run's code. `monk-detach-running-emacsen` lists the other Emacs processes on the host from `list-system-processes`, so a caller can say which instances, the agent included, keep running the old build until restarted. The script stays on disk, so the same job can be run from a shell.

`monk-app-link` links a formula's `.app` into `/Applications` through `brew --prefix FORMULA`, so the link follows upgrades; a stale link or a copy in the way is replaced, and a missing bundle is an error, not a dangling link. `monk-brew-versioned-formulae` and `monk-brew-latest-formula` read `brew search` for `STEM@N` versions, so a policy can follow the newest `emacs-plus@N` with a floor. `monk-brew-outdated-p` and `monk-brew-opt-prefix` are the other two brew questions a policy like that asks.

## Salt import and matchers

`(monk-salt-import-sls MACHINE "demo" FILE-ROOT PILLAR-ROOT)` runs `state.show_low_sls` and returns one `(monk-salt ...)` form per low chunk, in Salt's order, with `require`, `watch`, `onchanges`, `onfail`, `prereq`, and `listen` carried over as Monk requisites on the id. `M-x monk-salt-insert-sls` inserts them at point. Paste them into a policy and edit.

`(monk-salt-matcher-to-cell "G@os_family:Debian and not L@a,b")` returns a CELL form: `G@`/`P@` grains map through `monk-salt-grain-facts` to Monk facts (`os_family` values through `monk-salt-os-family-values`, so `MacOS` becomes `Darwin`; `G@kernel:X` becomes `(class x)`), `I@`/`J@` to inventory vars, `L@` to a name list, `E@` to a name regexp, `N@` to a group, a bare glob to the name. Salt compares case-insensitively and CELL `=` does not, so check values against `monk-machine-describe`. `S@` has no CELL form.

## History

`monk-history.el` writes every finished run to a SQLite database: one row per run and one per result, with the same fields as the event record. Plans, applies, and watch runs all count. Loading the file turns `monk-history-mode` on; it needs an Emacs built with SQLite (`sqlite-available-p`).

The database is `history.sqlite` under the Monk state directory: `$MONK_HOME` when set, `/etc/monk` when Emacs runs as root, else `$XDG_DATA_HOME/monk` or `~/.local/share/monk`. `monk-history-file` overrides the path.

`monk-history-runs` and `monk-history-results` return alists and take the same filters: `:host`, `:name`, `:type`, `:status`, `:run`, `:since` and `:until` (a timestamp or a duration such as `"7d"`), `:dry-run`, `:changed t` for repaired, would-repair, and partial only, `:limit`, `:offset`. `monk-history-repairs HOST NAME` is the timeline of one resource. `monk-history-run ID` rebuilds a `monk-run` for the report view. `monk-history-delete-before WHEN` prunes.

The detail buffer folds the `kept` and `skipped` groups by default and opens the rest, Salt's `state_output: changes`; `monk-ui-fold-statuses` changes the set and `TAB` on a group heading unfolds it.

In `*monk*`, `h` opens `*monk-history*`: newest run first with kind, hosts, counts, passes, and a note for flapping, watch-triggered, or from-plan runs. `RET` opens a run in the report view, `c` lists what changed in it, `r` asks for a host and resource and shows every change to it, `f` filters by host, `g` rereads.

An agent answers `{"op":"history-runs", ...}` and `{"op":"history-results", ...}` over its socket with the same filters as JSON keys (`dry_run`, `since`, `changed`, `limit`...). `monk-agent-history MACHINE 'runs FILTERS` asks a remote machine for its history without reading its database. CELL cannot read history on purpose: a guard must give the same answer while the facts are the same.

## Terminal output

`monk-cli.el` is what a batch Emacs shows on a terminal. When standard error is a terminal, one status line at the bottom is rewritten in place with a spinner, the pass, the count of nodes done, and the machine, type, and name of the resource in progress; every changed or failed result prints above it as a permanent line the moment it finishes, and the summary replaces the status line at the end. Columns are sized once per run from the longest machine and type names, so the line does not jump. Off a terminal (a pipe, a log file, launchd) nothing is rewritten and no escape bytes are written; the permanent lines and the summary still print. Color follows `TERM`, `NO_COLOR`, and `monk-cli-color`. The agent turns this on; `monk-cli-enable` does it for any batch entry point.


Between runs, the agent's last line is the same roll, slower (`monk-cli-idle-interval`, 0.25s), rewritten in place: `⠸ watching 12 dir(s)  3 run(s)  up 0d 02h 14m 07s  full in 00h 31m 12s`. It says the agent is alive without scrolling anything. A run takes the line over and gives it back; permanent lines go above it. Watch log lines on a UTF-8 terminal start with one frame of the roll instead of `M`, so consecutive lines roll too.
## Execution modes

Monk runs in three modes that share one engine and differ in who decides.

| Mode | Runs where | Approves a repair | Report |
|---|---|---|---|
| Standalone | `emacs --batch -f monk-agent-main` on the host, from systemd or launchd | nobody: local policy applies on drift and on a timer | ledger file and journald; a central Monk observes by tailing it |
| Supervised | the central Emacs, over TRAMP | a human, through the plan buffer and `a` | `*monk*` |
| Autopilot | the central Emacs, over TRAMP, driven by `monk-watch` | nobody | `*monk*` and the watch log |

The agent follows its own source. Before each timed full converge it compares every loaded Monk library and the policy file with their files on disk; an edit since they loaded is reloaded in place with `monk-reload` (the watch restarts on the new code) and the policy file is loaded again, and the run line says `reloaded monk-sync, reloaded policy monk-tzz-sync.el`. A reload that fails is logged and the summary line carries `STALE CODE: …` until it succeeds. Nothing restarts; this is Emacs.

Two `ansible-pull` habits are options of the agent. `monk-agent-splay` (seconds, default 0) delays the first converge and each timed one by a random amount up to that, so a fleet does not hit a shared server at the same second. `monk-agent-policy-repo` names the git checkout the policy is loaded from; before each timed converge the agent fast-forwards it through VC and, when HEAD moved, reloads `monk-agent-policy-file`. With `monk-agent-only-if-changed` set, a timed converge runs only after the checkout moved; the converge on start and the drift watch always run. A failed fetch is logged and the converge uses the policy on disk.

Supervised and autopilot are `monk-watch-mode`. Standalone is `monk-agent.el`. The agent converges the local policy on start, watches owned paths in autopilot, and converges in full every `monk-agent-full-interval` seconds (3600). It appends every event to `monk-agent-ledger-file` as JSON Lines and mirrors them to journald through `systemd-cat` when present. `(monk-agent-observe MACHINE)` on the central Monk runs `tail -F` on that ledger through TRAMP and dispatches each parsed event to `monk-agent-observe-functions`; there is no port and no daemon beyond SSH. The agent and a controller applying the same policy do not fight: both apply the same policy, and the settle window drops the events the other side produced.

### launchd

On macOS, `(monk-agent-launchd-units :policy-file FILE)` in a policy installs and loads `org.monk.agent`, a user agent that runs `emacs --batch -l FILE -f monk-agent-main` with `KeepAlive`, so the loop restarts if it dies and `ThrottleInterval` keeps a crash from spinning. Output goes to `agent.log` under the Monk state directory. There is no socket endpoint on launchd; read the ledger or the history instead. `monk-host-reachable-p` is the probe `:reachable` uses: one TCP connect with `make-network-process`, cached per run.

### systemd units

`(monk-agent-units)` inside a policy installs and enables two units on a Linux host. `monk-agent.service` is the long-lived loop; a drift watcher must stay resident, so it is not socket activated. `monk-agent.socket` plus `monk-agent@.service` is an on-demand query endpoint on `/run/monk/agent.sock` with `Accept=yes`: systemd accepts each connection and starts one `emacs --batch -f monk-agent-serve` with the connection on stdin and stdout, inetd style, because Emacs cannot adopt an inherited listening socket but reads and writes stdio well. Idle cost is zero. `:user t` installs user units under `~/.config/systemd/user/` with the socket in `$XDG_RUNTIME_DIR/monk/`. Set `monk-agent-emacs`, `monk-agent-load-path`, and `monk-agent-policy-file` before compiling the policy so the `ExecStart` lines point at the right places on the target.

The endpoint takes one JSON object per line and answers in JSON Lines. `{"op":"status"}` returns liveness and the last `run-end`. `{"op":"tail","n":50}` returns the last ledger lines. `{"op":"plan"}` and `{"op":"run"}` converge now and stream one result event per node, then `run-end`. From the controller, `(monk-agent-status M)`, `(monk-agent-remote-run M DRY-RUN)`, and `(monk-agent-query M REQUEST)` send the request through `socat` over TRAMP; no port is opened.

## etcd

`monk-etcd.el` is a small etcd v3 client over the gRPC gateway (HTTP and JSON with base64 keys), enough for shared state: strings, per-host string maps, exported resources, run events, and agent presence with a lease. Where the value format allows, the key layout is mgmt's, so one etcd can serve both: `/_mgmt/strings/KEY` and `/_mgmt/strings/KEY/HOST` are byte-compatible with mgmt's `world.StrGet` and `StrMapGet`. mgmt's exported resources under `/_mgmt/exported/` are Go `gob` blobs, which Emacs cannot decode, so Monk keeps its own JSON under `/_monk/`: `exported/TO/FROM/TYPE/NAME` for `monk-etcd-export` and `monk-etcd-collect`, `events/HOST/RUN/SEQ` for `monk-etcd-publish-run` and `monk-etcd-run`, `agents/HOST` for `monk-etcd-announce` and `monk-etcd-agents`. `monk-etcd-watch PREFIX CALLBACK` streams puts and deletes over one raw HTTP connection. Set `monk-agent-etcd` to make the standalone agent publish every run and keep a presence key alive. Tested against a fake gateway in the suite; not yet against a real etcd.

## Events

`monk-event.el` serializes results as one JSON object per line. This is the ledger format, and it is the shape every other configuration tool normalizes to.

```json
{"rgi":2,"event":"result","tag":"monk/run/r20260926T100001.000/prog/kamen/0",
 "ts":"2026-09-26T10:00:01.234Z","host":"kamen","run":"r20260926T100001.000",
 "pass":1,"seq":0,"type":"file","name":"/etc/motd","status":"repaired","changed":true,"degree":1.0,
 "changes":{"content":true},"diff":"--- a/etc/motd\n+++ b/etc/motd\n...","comment":"content",
 "duration_ms":4.5,"dry_run":false,"native":{"tool":"monk"}}
```

Every event carries a `tag`, a `/`-joined path like Salt's: `monk/run/<run>/new`, `monk/run/<run>/pass/<n>`, `monk/run/<run>/prog/<host>/<seq>`, `monk/run/<run>/end`, `monk/drift/<host>`, `monk/agent/<host>/start`. Resource names never go in a tag; they are in the data, and `seq` is the position in the run. Filter with `monk-event-tag-matches-p`. Results carry `seq` and `pass`, and so do the history rows; the history also has an `events` table that `monk-history-events PREFIX` reads.

`event` is `run-start`, `result`, `pass`, `run-end`, `drift`, or `agent-start`. `status` is Monk's vocabulary plus `unknown`; `native` carries a foreign tool's own status verbatim. Fields are added, never renamed; `rgi` is the format version. `monk-run-events` produces the list for a run, `monk-events-to-run` rebuilds a run from parsed lines. `docs/mgmt-review-and-interop.md` explains why a shared event protocol is realistic, a shared resource graph is realistic for import only, and a shared language is not.

## Report

`monk-ui.el` draws the run. Load it and `monk-run-report` shows a `*monk*` buffer with one row per machine: name, state badge, a stacked progress bar, counts, and time. The bar has one layer per status, failures on the left edge. While a run is in progress the row of the current machine shows a spinner and the bar grows as nodes finish. `RET` on a row opens a detail buffer with results grouped by status and with fontified diffs. The detail buffer starts with the classes, groups, facts, and vars of the machine, folded. `TAB` on that line unfolds it. `i` on a row opens the same data on its own, unfolded. `/` asks for a CELL or CEL guard and shows which inventory machines match. `.` or `M-.` jumps to the source: on a machine row or heading, the inventory file at that host; on a result line in a detail buffer, the policy file at that resource. `M-,` comes back. Each resource remembers the file its policy was defined in, and each machine the file that defined the inventory; the event record carries the policy file as `native.source`. `t` switches between SVG and text drawing. `g` runs the same thing again: a plan plans again, an apply applies again against its plan. `C-u g` only redraws. `q` closes any Monk buffer. `?` lists the keys. After a run you started, point moves into `*monk*` so the keys work at once. A run that a watch triggered only displays the buffer. `monk-ui-select-window` changes this.

A plan and an apply look different. A plan has a purple `PLAN` badge in the header, a purple header face, a `Would` column instead of `Changed`, and the hint `a to apply`. Pressing `a` asks for confirmation with the machine names and the number of changes, runs `monk-apply`, and replaces the plan in the buffer with the apply report. An apply from a plan has a blue `APPLY from plan` badge, a `Plan` column with one cell per machine that says `as planned` or lists the verdicts that differ, and a verdict strip in the header. `p` opens the plan an apply came from. The detail buffer of such an apply starts with a `differs from plan` section that pairs each drifted result with what the plan said.

Bars and the spinner are SVG on a graphical display with SVG support, and block glyphs elsewhere. Status glyphs fall back to ASCII on a terminal that cannot show them. Set `monk-ui-style` to `text` to force text and `monk-ui-unicode` to nil for ASCII. All colors come from faces with light and dark variants, so themes apply. In batch mode the report prints as text.

Progress events go through `monk-progress-functions`. Add your own function there to drive another display.

A check can leave evidence with its result even when nothing changed: `(monk-check-note :key value ...)` inside `monk-check` puts the pairs under `:evidence` in the result's changes, where the history and the events keep them. The rsync check records the command, exit code, file count, itemized count, and output line count, so a `kept` that was wrong can be read back from the history instead of guessed at. Evidence alone is not a change.

## Writing a resource

Subclass `monk-resource`. Implement `monk-check`, which inspects the machine and returns nil or a plist of changes, and `monk-repair`, which applies the plist and returns a comment. Optionally implement `monk-diff` for a unified diff and `monk-action` for notifications. `monk-check` must not change the machine. Add a constructor function that calls `monk-add-resource`.

Use `monk-exec` for commands. It returns a struct with exit code and output. `monk-exec-string` signals `monk-exec-error` on non-zero exit. Trailing keyword pairs: `:sudo t` to run as root, `:input STRING` for standard input, `:directory PATH` to run in a directory on the target. Use `monk-machine-path` to turn a target path into a controller-side path for file primitives.

A set resource, one that manages several members, returns `:degree` in its change plist: the fraction of members that already hold. A failed repair then reports `partial` with that degree, and requisites with `:degree` can accept it. Implement `monk-default-watch-action` to give the resource an action for `:watch`.

## Facts

`monk-gather-facts` runs the probes in `monk-fact-probes`. The default probes give `kernel`, `arch`, `nodename`, `hostname`, `os-family`, `distribution`, `distribution-release`, `distribution-version`, `home`, `user`, `pkg-mgr`, and `service-mgr`. Register a probe with `monk-register-fact-probe`.

`pkg-mgr` and `service-mgr` name the first manager found on the machine from `monk-package-managers` and `monk-service-managers`; they are classes too. `(monk-package NAME PACKAGES)` and `(monk-service NAME ...)` dispatch on them: brew or apt, launchd or systemd. A machine with a manager Monk has no resource for gets a `monk-unsupported` placeholder that reports skipped with the reason, so one policy compiles everywhere.

Local facts come from `*.fact` files in the directories of `monk-facts-d-directories`: `~/.config/monk/facts.d`, `/etc/monk/facts.d`, and `/etc/ansible/facts.d`, so a host that already publishes facts for Ansible needs nothing new. Each file gives one key under the `local` fact. JSON parses first, INI second, and an executable file is run for its output. A file that fails to parse contributes its error text, visible in the inspector.

Facts are cached when `monk-history` is loaded. A gather stores them in the history database, and the next compile within `monk-fact-ttl` (a day) reads them back instead of probing the host, so a plan over many machines starts at once. Set `monk-fact-ttl` to nil to always probe; `monk-facts-flush` drops the cache for a machine. The `*monk*` machine row, the fold heading in the detail buffer, and the inspector's `facts from` line show a badge with the age of cached facts: grey `·3h` when fresh, amber `!2d` when older than the TTL and Monk could not refresh them, `?` when a machine has none. Live facts show no badge.

`monk-diff` returns a list of `monk-diff-entry` structs, not a string. An entry has a kind: `content`, a unified diff of text; `attributes`, before and after values of mode, owner, and group; or `rendered`, text a resource produced itself. `monk-result-diff-text` renders a result's entries for the report, and the event stream carries them structured, so a consumer can tell a mode change from a content change without parsing a diff.

## Version

`M-x monk-version` prints `Monk 0.5.7 build 2026-10-09.3 from ~/.../monk/lisp/` and, when a loaded library's `.el` is newer than the moment it loaded, `STALE: monk-ui +312s (edited after load; run monk-reload)`. The drive and the tour print the same line first, so a report from old code says so. `monk-build` is bumped at every hand-over; `monk-version` gets a patch bump for small work and a minor bump for a large ask.

## Test drive, tour, tutorial

`M-x monk-testdrive` runs every feature against a throwaway home and reports in `*monk-testdrive*`: converge, plan and apply, requisites, flapping, drift watching, the agent ledger and query handler, the report buffers. Every check is done by code that reads files, run results, and the text and properties of the `*monk*` buffers; nothing asks you anything while it runs. `C-u` runs one section. `M-x monk-testdrive-tour` is for your eyes: it sets up one thing at a time with nothing running underneath and asks whether it looked right, for the plan header and bars, the diff in the detail buffer, the apply comparison, the flapping badge, the watch badge, and the text fallback.

`M-x monk-tutorial` opens ten lessons beside a sandbox home. `C-c C-c` runs the form at point against the sandbox and shows the result in `*monk*`. The lessons cover the same ground as the drive, one feature at a time, and end with writing a resource of your own. `docs/tutorial.md` is the same content as prose. The test suite evaluates every lesson form, so the lessons cannot rot.


Both stand apart from the Emacs that runs them. `HOME` and `MONK_HOME` point into the throwaway home, so the history database and fact cache are fresh; the inventory, batch policy, and resource index are bound empty; every Monk user option is at its default for the duration and untouched after. The first drive section, Isolation, asserts all of that, and greps the drive's own source for a literal path outside the throwaway home, so a step that starts reading the user's files fails the drive.
## Development

```sh
make compile   # byte-compile with warnings as errors
make test      # run ERT tests
make check     # checkdoc
```

`M-x monk-reload` reloads every loaded `monk-*` library from source in first-load order and fixes what a plain `load` leaves behind: keymap variables are unbound first and re-installed in live `*monk*` and tutorial buffers, an active watch is stopped and started again on the new code with the same policy and mode, the image cache is cleared, and the notification probe is reset. EIEIO classes redefine in place. A run already stored in `*monk*` keeps its old struct layout; the summary line says so, and rerunning refreshes it. `monk-reload-last-failures` holds any file that did not load.

Monk reuses Emacs core where it exists: TRAMP and the `file-*` primitives for files, `file-attributes` for ownership, the `diff` library for diffs, VC (`vc-git`) for git, `filenotify` for watches, built-in SQLite for history. Shell commands remain only where core has nothing: `crontab`, `sysctl`, `systemctl`, `brew`, `apt-get`, `defaults`, `chown` by name, and `fswatch` on macOS where kqueue misses writes inside a directory. Every new feature gets a test drive step or a tour stop as well as ERT tests; batch tests never look at a window.

Tests run against a temporary directory on the local host. Package, sudo, and bridge resources are tested through their pure helpers and a faked exec layer. Cross-machine tests use several local machine objects with different names.

## Not yet done

Docker containers. Package removal. A `monk-user` resource that creates accounts. Homebrew cask listing on older `brew`. Batched remote checks to cut TRAMP round trips. Parallel machines inside `monk-orchestrate`. A persistent ledger so a single-machine run can read results from an earlier run on another machine.
