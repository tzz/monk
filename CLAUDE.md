# Rules for work on Monk

Monk is an Emacs program. Reuse Emacs core before you write a shell
command or a parser: TRAMP handlers and `file-*` primitives for files,
`file-attributes` for ownership and modes, the `diff` library for
diffs, VC for git, `filenotify` for watches, `sqlite-*` for storage,
`auth-source` for secrets, `url` for HTTP. Core runs on every
platform Emacs runs on and honors the user's settings. Shell out only
when core is broken or absent, and say why in a comment next to the
call. Known justified cases: `fswatch` on macOS (kqueue misses writes
inside a directory), `crontab`, `sysctl`, `systemctl`, `brew`,
`apt-get`, `defaults`, `chown` by name.

Every new piece of functionality is verified in the test drive or the
tour, not only in ERT. If code can check it, add a drive step in
`lisp/monk-testdrive.el`. If only eyes can judge it (a badge, a
header line, focus, a fold), add a tour stop. Batch tests read
strings and never look at a window; the header line was invisible for
days for that reason. When you hand over a feature, name the drive
step or tour stop that covers it.

A parser of another program's output is only as good as the samples
it was tested on. Before trusting one, capture the real output on
every platform Monk targets (macOS rsync prints 12 itemize columns,
Linux rsync 3.2 prints 11; brew's JSON differs from what docs show)
and keep the captured lines in the tests. A parser that matches
nothing must not be read as "nothing to do": when a check finds no
changes but the tool printed lines the regexp did not match, log it.

Execution model is Salt's, not Ansible's: a failure is a result, the
pass goes on, only `:require` and `:watch` dependents skip. Do not
add a stop-on-failure path.

Guards are CELL forms or CEL strings. A guard reads facts, classes,
inventory variables, and groups only. It never reads history or the
clock, so the same facts give the same answer.

Before handing over: `make compile test check` clean with warnings as
errors, `monk-tzz.el` compiles against the tree, and the scan for
internal names in the public repo returns nothing. Git is read only
for the assistant; hand over the commit message.

## Version and build

`monk-version` and `monk-build` live at the top of `lisp/monk.el`. Bump `monk-build` (`YYYY-MM-DD.N`, N counts hand-overs within a day) with every hand-over, and say the new build in the hand-over message. The drive and tour print it on their first line, and print `STALE:` with the libraries edited after they loaded, so an old Emacs is caught before a report is trusted. `monk-version` is semantic: bump the patch for small work (a fix, a tour step, a small option), bump the minor for a large ask (a new library, a review group, a new resource family). Never leave a hand-over without one of the two bumped alongside `monk-build`.
