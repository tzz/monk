# Monk

Monk is a configuration management system. Policy is Emacs Lisp. Remote access is TRAMP. There is no server, no agent, and no template language.

## Model

A policy is a function of one argument, the machine. The function calls resource constructors. Monk collects the resources, then checks and repairs each one on the machine. This is the Chef compile and converge split.

Each resource ends in one of six states. `kept` means the machine already matched. `repaired` means Monk changed it. `would-repair` means a dry run found a difference. `partial` means some of the resource holds and the rest failed. `failed` means the repair raised an error. `skipped` means a guard, class, or requisite did not match. These are the CFEngine promise outcomes.

Each result carries the machine, the resource name, its type, a degree from 0 to 1, the changes as a plist, a comment, and a unified diff for file content. The degree says how much of the resource holds. A package set with 121 of 123 packages installed that fails to install the rest reports `partial` with degree 0.98. Dry run returns the same shape as a real run. This is the Salt result shape.

Conditions are classes. Facts produce hard classes such as `darwin`, `debian`, `ubuntu`, `noble`, `arm64`. Inventory variables with the value `t` produce soft classes such as `home_leader`. Group names produce classes too. A resource with `:classes '(debian)` runs only on Debian-family hosts.

Monk runs passes until a pass repairs nothing, with a cap of `monk-max-passes`. A second pass after repairs confirms that the repairs held. A resource that repairs in more than one pass is flapping: a repair that held would be kept afterwards, so a second repair means another resource undid it, or its check and repair disagree. The passes need not be adjacent. With A undoing B, B undoing C, and C undoing A, each repairs every third pass. The classic two-resource case is one that installs a package and one that removes it. The run stops at the second repair of any resource, the report names the flapping resources, and `monk-run-flapping` returns their keys. `monk-max-passes`, default 6, bounds the cycle length Monk can name. When the cap is reached with repairs still happening and no single resource flaps, the report says `did not converge` and `monk-run-oscillating-p` returns non-nil. `monk-run-converged-p` is true only when neither happened. A fatal failure stops the run after one pass.

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

;; Local host, dry run.
(monk-run-report (monk-converge (monk-local-machine) #'my-policy :dry-run t))

;; Remote host.
(monk-run-report (monk-converge (monk-machine :name "pi" :address "pi.local" :user "pi")
                                #'my-policy))

;; Several hosts as one graph, so cross-machine requisites resolve.
(monk-run-report (monk-orchestrate (monk-inventory-select 'home) #'my-policy))
```

## Paths

Paths are interpreted on the target machine. A leading `~` is the home of the login user, or of root under `:sudo`. File primitives expand it through TRAMP. A symlink target with a leading `~` is expanded on the target through the `home` fact before the link is made, because a literal `~` in a link is dangling. Inside a policy, `(monk-expand "~/x")` returns the same expansion for any other use, such as a command argument.

Batch:

```sh
emacs --batch -L lisp -l my-policy.el --eval '(monk-batch-converge "my-policy.el")'
```

The policy file must set `monk-batch-policy` to the policy function.

## Resources

Every constructor accepts these keywords. `:only-if FN` runs the resource only when `(funcall FN machine)` is non-nil. `:not-if FN` skips it when non-nil. `:classes LIST` runs it only when the machine has every class in the list. `:require`, `:watch`, `:onchanges`, and `:onfail` are described above. `:sudo BOOL` performs the resource as root through a TRAMP sudo hop. `:ignore-errors BOOL` records a failure and continues instead of stopping the run.

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

Package and macOS resources in `monk-package.el`:

| Constructor | Purpose |
|---|---|
| `(monk-brew NAME PACKAGES &key cask)` | Missing Homebrew formulae installed in one call. Class `darwin`. |
| `(monk-brew-tap NAME)` | Homebrew tap present. Class `darwin`. |
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

## Report

`monk-ui.el` draws the run. Load it and `monk-run-report` shows a `*monk*` buffer with one row per machine: name, state badge, a stacked progress bar, counts, and time. The bar has one layer per status, failures on the left edge. While a run is in progress the row of the current machine shows a spinner and the bar grows as nodes finish. `RET` on a row opens a detail buffer with results grouped by status and with fontified diffs. `t` switches between SVG and text drawing. `g` redraws.

Bars and the spinner are SVG on a graphical display with SVG support, and block glyphs elsewhere. Status glyphs fall back to ASCII on a terminal that cannot show them. Set `monk-ui-style` to `text` to force text and `monk-ui-unicode` to nil for ASCII. All colors come from faces with light and dark variants, so themes apply. In batch mode the report prints as text.

Progress events go through `monk-progress-functions`. Add your own function there to drive another display.

## Writing a resource

Subclass `monk-resource`. Implement `monk-check`, which inspects the machine and returns nil or a plist of changes, and `monk-repair`, which applies the plist and returns a comment. Optionally implement `monk-diff` for a unified diff and `monk-action` for notifications. `monk-check` must not change the machine. Add a constructor function that calls `monk-add-resource`.

Use `monk-exec` for commands. It returns a struct with exit code and output. `monk-exec-string` signals `monk-exec-error` on non-zero exit. Trailing keyword pairs: `:sudo t` to run as root, `:input STRING` for standard input, `:directory PATH` to run in a directory on the target. Use `monk-machine-path` to turn a target path into a controller-side path for file primitives.

A set resource, one that manages several members, returns `:degree` in its change plist: the fraction of members that already hold. A failed repair then reports `partial` with that degree, and requisites with `:degree` can accept it. Implement `monk-default-watch-action` to give the resource an action for `:watch`.

## Facts

`monk-gather-facts` runs the probes in `monk-fact-probes`. The default probes give `kernel`, `arch`, `nodename`, `hostname`, `os-family`, `distribution`, `distribution-release`, `distribution-version`, `home`, and `user`. Register a probe with `monk-register-fact-probe`.

## Development

```sh
make compile   # byte-compile with warnings as errors
make test      # run ERT tests
make check     # checkdoc
```

Tests run against a temporary directory on the local host. Package, sudo, and bridge resources are tested through their pure helpers and a faked exec layer. Cross-machine tests use several local machine objects with different names.

## Not yet done

Docker containers. Package removal. A `monk-user` resource that creates accounts. Homebrew cask listing on older `brew`. Batched remote checks to cut TRAMP round trips. Parallel machines inside `monk-orchestrate`. A persistent ledger so a single-machine run can read results from an earlier run on another machine.
