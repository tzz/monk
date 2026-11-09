#!/bin/sh
# Monk quickstart: clone Monk (and a policy repository) into the current
# directory, write a starter policy when there is none, and print the
# commands that come next.  Nothing runs Emacs; nothing is installed.
#
#   sh -c "$(curl -fsSL https://raw.githubusercontent.com/tzz/monk/main/bootstrap.sh)"
#   sh -c "$(curl -fsSL https://raw.githubusercontent.com/tzz/monk/main/bootstrap.sh)" -- POLICY_REPO_URL [POLICY_FILE]
#
# POLICY_REPO_URL: a git URL cloned beside monk/.  POLICY_FILE: the file
# inside that clone the agent loads (default policy.el).  Without a
# policy repository the script writes ./monk-policy/policy.el, one
# harmless resource and an inventory directory beside it.
#
# Environment: MONK_REPO (default https://github.com/tzz/monk), MONK_BRANCH
# (default main), EMACS (default emacs).

set -eu

MONK_REPO=${MONK_REPO:-https://github.com/tzz/monk}
MONK_BRANCH=${MONK_BRANCH:-main}
EMACS=${EMACS:-emacs}
POLICY_URL=${1:-}
POLICY_FILE=${2:-policy.el}
STARTER_DIR=monk-policy

EXIT_USAGE=2
EXIT_MISSING_TOOL=3
EXIT_EMACS_TOO_OLD=4

say() { printf '%s\n' "$*"; }
fail() { say "bootstrap: $*" >&2; exit "$EXIT_MISSING_TOOL"; }

case "$POLICY_URL" in
  -h|--help)
    sed -n '2,15p' "$0" 2>/dev/null | sed 's/^# \{0,1\}//'
    exit "$EXIT_USAGE" ;;
esac

command -v git >/dev/null 2>&1 || fail "git is not on PATH"
command -v "$EMACS" >/dev/null 2>&1 || fail "$EMACS is not on PATH (set EMACS=/path/to/emacs)"

emacs_version=$("$EMACS" --batch --eval '(princ emacs-major-version)' 2>/dev/null || echo 0)
if [ "$emacs_version" -lt 29 ]; then
  say "bootstrap: Monk needs Emacs 29.1 or newer; $EMACS is $emacs_version" >&2
  exit "$EXIT_EMACS_TOO_OLD"
fi

clone() {
  url=$1; dir=$2; branch=${3:-}
  if [ -d "$dir/.git" ]; then
    say "$dir exists; not cloned again"
  elif [ -n "$branch" ]; then
    git clone --branch "$branch" --single-branch "$url" "$dir"
  else
    git clone "$url" "$dir"
  fi
}

clone "$MONK_REPO" monk "$MONK_BRANCH"
monk_dir=$(cd monk && pwd)

if [ -n "$POLICY_URL" ]; then
  policy_dir=$(basename "$POLICY_URL" .git)
  clone "$POLICY_URL" "$policy_dir"
  policy_path=$(cd "$policy_dir" && pwd)/$POLICY_FILE
  [ -f "$policy_path" ] || say "note: $policy_path does not exist yet; the agent needs it"
else
  policy_dir=$STARTER_DIR
  mkdir -p "$policy_dir/inventory.d"
  policy_path=$(cd "$policy_dir" && pwd)/policy.el
  if [ -f "$policy_path" ]; then
    say "$policy_path exists; not written again"
  else
    cat > "$policy_path" <<'EOF'
;;; policy.el --- starter Monk policy  -*- lexical-binding: t; -*-

;; Written by bootstrap.sh.  Edit freely; the agent reloads it on save.
;; Add resources to `starter-policy'.  See monk/README.md.

(require 'monk)
(require 'monk-file)
(require 'monk-inventory)
(require 'monk-agent)

(setq monk-inventory-path
      (expand-file-name "inventory.d/" (file-name-directory
                                       (or load-file-name buffer-file-name))))
(monk-inventory-load)

(defun starter-policy (machine)
  "One harmless resource, so the first plan shows the shape of a run."
  (ignore machine)
  (monk-directory "~/.config/monk" :mode #o700))

(setq monk-batch-policy #'starter-policy)

;;; policy.el ends here
EOF
    say "wrote $policy_path"
  fi
fi

cat <<EOF

Monk is in $monk_dir
Policy is $policy_path

Next:

  1. Plan this machine (a dry run, nothing changes):

     $EMACS --batch -L "$monk_dir/lisp" -l "$policy_path" \\
       --eval '(monk-run-report-text (monk-plan (monk-local-machine) monk-batch-policy) t)'

  2. Start the agent in the foreground (Ctrl-C stops it).  A host the
     inventory does not name is enrolled on probation: every run is a
     dry run until you accept it.

     $EMACS --batch -L "$monk_dir/lisp" -l "$policy_path" -f monk-agent-main

  3. From an Emacs on this machine, with $monk_dir/lisp on load-path:
     M-x monk-connect, pick local, read the plan, then
     M-x monk-enroll-accept.  M-x monk-enroll-plan-agent installs the
     agent as a launchd or systemd user service.

  4. Commit the policy and the new inventory fragment in $policy_dir.
EOF
