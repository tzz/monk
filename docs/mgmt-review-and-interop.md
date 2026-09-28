# mgmt review, execution modes, and interop

Written 2026-09-26 after reading purpleidea/mgmt (Go, ~100k lines) at
`source/mgmt` and surveying the interchange formats of Salt, Ansible,
Puppet, Chef, CFEngine, and DSC v3. Sources are cited as `file:line`
in the mgmt tree. Claims about other tools come from memory and are
marked so; nothing was fetched.

## 1. What mgmt is

mgmt is an event-driven, continuously running engine. A resource
implements `Watch` and `CheckApply`. `Watch` blocks on inotify, a
timer, systemd, or a network stream and pokes the engine when the
world changes. The engine then calls `CheckApply(ctx, apply)`, which
returns `checkOK bool, err`. `checkOK` true means kept; false means
repaired, or in noop mode, would repair (`engine/resources.go:296`,
`engine/graph/actions.go:277-403`). There is no pass loop. Each
vertex is its own goroutine, and the graph is "converged" when every
vertex has reported OK and stayed quiet for a timeout
(`converger/converger.go`: `Timeout`, `holdUntil`, `StateFns`).

The graph itself is reactive. Policy is `mcl`, a functional language
whose expressions are a function graph. When a fact changes, the
function graph re-emits a new resource graph, and the engine pauses,
diffs old against new, and resumes (`engine/graph/state.go:396-465`,
`Pause`, `Resume`, `event`). Resources removed between graphs can
declare a `Reversed()` counterpart that cleans up
(`engine/reverse.go:43-66`).

Multi-host is etcd. Every host is an etcd member. `World` is the
interface to shared state (`engine/world.go`): `StrWorld` is a key
value store with watches, `ResWorld` exports and collects resources
across hosts (`ResExport`, `ResCollect`, `engine/world.go:124`),
`SchedulerWorld` elects which N hosts run a resource. There is no
wire protocol of mgmt's own; etcd is the protocol.

## 2. What Monk should take

Ordered by value.

**Refresh as a separate signal from state.** mgmt carries a per-edge
`notify` bit and a per-vertex "refresh pending" flag
(`engine/refresh.go:36`, `actions.go:283-313`). A resource can be in
the correct state and still owe a refresh because an upstream changed.
Monk conflates these: `:watch` runs the watch action inside the
verdict. Split them: a `refresh` flag set by the scheduler when a
watched ref changed, consumed by the resource after its check. Then
noop mode can report "would refresh" separately from "would repair",
which today it cannot.

**Autoedges.** A file resource declares the UIDs it depends on (its
parent directory, `engine/resources/file.go:1674-1716`) and the engine
adds edges to any resource that provides them. Monk has
`monk-resource-paths`; add `monk-resource-provides` and
`monk-resource-needs` and let `monk-schedule` add the edges. That
removes most hand-written `:require` between a directory and its
files, and between a package and the unit that needs its binary.

**Autogrouping of package resources.** mgmt groups all `pkg` vertices
into one transaction (`engine/graph/autogroup.go`). Monk already does
this by hand: `monk-apt "debian packages" LIST` is one resource. Keep
that shape, do not add machinery.

**Meta parameters as one struct.** `engine/metaparams.go:74`: `Noop,
Retry, RetryReset, Delay, Timeout, Poll, Limit, Burst, Reset, Sema,
Rewatch, Dollar, Hidden, Export, Realize`. Monk has `:sudo`. Worth adding, in order: `:retry N` with `:delay`,
`:timeout`, `:poll SECONDS` for resources that cannot be watched, and
`:sema NAME` so two resources never run at once across a run. Skip
`Limit/Burst` (rate limiting a converge loop is not a Monk problem),
`Dollar`, `Hidden`, `Realize`.

**Exported resources.** `ResExport` publishes a resource from one host,
`ResCollect` pulls matching ones on another (`engine/world.go:119-235`).
This is the Puppet exported-resource idea done over etcd. Monk's
cross-machine requisites cover ordering across hosts; they do not cover
"host A declares a resource that host B must realize" (an SSH key, a
`/etc/hosts` line, an exporter target). Add a `monk-export` constructor
and a `monk-collect` selector on the orchestrator, resolved at compile
time inside one `monk-orchestrate`. No shared store: the orchestrator
already compiles every machine.

**Converged as a quiet period, not a pass count.** For the standalone
agent mode below, mgmt's model fits better than Monk's pass loop. An
agent that reacts to events is converged when its last N seconds held
no repair. Keep passes for one-shot runs, add a quiet timer for agents.

**Prometheus metric names.** `mgmt_checkapply_total{kind,apply,eventful,errorful}`,
`mgmt_resources{kind}`, `mgmt_failures_total`, `mgmt_failures`,
`mgmt_graph_start_time_seconds` (`prometheus/prometheus.go:93-140`).
Copy the vocabulary for Monk's event stream counters; do not run an
HTTP server in Emacs.

**Reversible resources.** Worth having for `monk-symlink`,
`monk-line-in-file`, `monk-cron`, `monk-systemd-unit`: when a resource
leaves the policy between two runs, remove what it made. Needs a
per-machine record of the last applied graph. Defer until the ledger
exists (section 3).

## 3. What Monk should not take

The function-graph language. `mcl` is the reason mgmt can react to a
weekday change in real time. Emacs Lisp already is a language; Monk's
policy is a function of the machine, and re-running it is cheap.
Reactivity in Monk comes from `monk-watch` re-planning the affected
nodes, not from a dataflow graph.

Goroutine per resource. Emacs has one thread that matters. Monk runs
nodes in order; concurrency across machines belongs in separate
`emacs --batch` agents, not inside one Lisp image.

etcd. Monk's controller is an Emacs with SSH. Shared state is the
controller's memory plus a ledger file per host.

Send/Recv (`engine/sendrecv.go:45-119`), where one resource's output
field feeds another's input. Useful in mgmt because the language is
pure; in Lisp a policy can compute the value and pass it.

The converger's `StateFns` machinery. A hook on `run-end` covers it.

## 4. Three execution modes

The three modes share one engine and differ in who decides.

| Mode | Who runs | Who approves a repair | Where the report goes |
|---|---|---|---|
| **standalone** | `emacs --batch -l monk-agent` on the host, from systemd or launchd | nobody: local policy applies on drift and on a timer | local ledger; NDJSON stream to a central Monk when one is connected |
| **supervised** | the central Emacs, over TRAMP | a human, through the PLAN buffer and `a` | `*monk*` |
| **autopilot** | the central Emacs, over TRAMP, driven by `monk-watch` | nobody | `*monk*` and the watch log |

Supervised and autopilot exist today as `monk-watch-mode`. Standalone
is new and is the mgmt-shaped one. Its pieces:

`monk-agent.el`, loaded in batch. It calls `monk-watch-start` on the
local machine with `monk-watch-mode` bound to `autopilot`, plus a poll
timer for resources with no owned path, plus a full converge on start
and every `monk-agent-full-interval`. It runs `(while t
(accept-process-output nil 1))` so timers and filenotify fire. It
writes every result as one JSON line to `monk-agent-ledger-file`
(`/var/lib/monk/events.ndjson`, or `~/.local/state/monk/` without
root), and to journald through `systemd-cat -t monk` when present.

Observability from the central Monk. The controller runs
`tail -F /var/lib/monk/events.ndjson` over TRAMP with
`start-file-process` and parses lines in a process filter. That is the
"connection": SSH, no port, no daemon of Monk's own, no auth beyond
what SSH already did. The controller merges the agent's events into a
`monk-run` for `*monk*` with a `STANDALONE` badge, read-only. For a
disconnected period it replays `journalctl -t monk -o json --since` or
seeks the file. Prometheus later, if wanted, is a textfile collector
written from the same events.

Control from the central Monk. Not in scope for standalone: the point
of the mode is that the host converges without a controller. A
controller that wants control uses supervised or autopilot over TRAMP
against the same policy. The two do not fight, because the agent's
policy and the controller's policy are the same file checked out on
the host, and the settle window drops the events the other side made.

## 5. A common protocol

Three layers, three answers.

**A common language: no.** Every attempt became one more language with
bridges: DMTF MOF for DSC v1, XCCDF fix scripts for compliance, DSC v3
configuration documents. Each tool's language is its type vocabulary
and its evaluation model. Puppet's `ensure`, Salt's `state.fun`,
Ansible's module arguments, mgmt's reactive graph, CFEngine's classes
do not unify without losing the reason each exists. Monk keeps Lisp.

**A common result and event protocol: yes, and cheap.** Every tool
already emits the same tuple under different names. Salt: `{result,
changes, comment, duration}`. Ansible: `{changed, failed, msg, diff}`.
Chef: `{status, diff, duration}`. CFEngine: `kept/repaired/notkept`.
mgmt: `checkOK, err` plus the Prometheus counters. Monk: `{status,
degree, changes, comment, diff, duration}`. One JSON Lines record
covers all of them:

```json
{"rgi": 1, "ts": "2026-09-26T10:00:01.234Z", "host": "kamen",
 "run": "r7", "pass": 1, "type": "file", "name": "/etc/motd",
 "status": "repaired", "changed": true, "degree": 1.0,
 "changes": {"content": true}, "diff": "--- a/etc/motd\n+++ b/etc/motd\n...",
 "comment": "content", "duration_ms": 4.5, "error": null,
 "native": {"tool": "monk"}}
```

`status` is the six Monk values plus `unknown`; `native` carries the
tool's own status verbatim, because Salt cannot tell kept from
repaired without looking at `changes`, Ansible has no kept-versus-
skipped inside a module, and nobody else has `degree`. This is the
format the standalone agent writes and the controller reads. It is
also what `monk-ansible` and `monk-salt` already normalize to
internally; emitting it is a serializer on `monk-result`.

**A common resource graph IR: yes for import, lossy for export, and
only for the common core.** Puppet catalog JSON is the strongest
precedent: `resources[]` with `type/title/parameters` and relation
metaparams, applied directly by `puppet apply --catalog`. Felix Frank
translated it to mgmt YAML in 2016, and mgmt's `yamlgraph`
(`yamlgraph/gconfig.go`) is a smaller version of the same shape:
`resources: {kind: [{name, meta, ...fields}]}`, `edges: [{from, to,
notify}]`. Salt's lowstate is the equivalent IR on the Salt side:
`[{state, fun, name, __id__, require: [{pkg: x}], watch, onchanges,
onfail, order}]`, exported by `state.show_lowstate` and ingested by
`state.template_str` or `state.low`. Monk's node and requisite model
maps onto both with two losses: thresholds and cross-machine refs have
no home outside Monk and Salt orchestration, and `partial` degrades to
`changed`.

The graph part is a weekend. The parameter part is the real work: a
per-type mapping table per tool. `file.content` is a string in Monk
and mgmt, a hash in a Puppet catalog, `contents` or `source` in Salt,
a `copy` module with a controller-side source in Ansible. Build the
table for `file, directory, symlink, line, package, service, user,
cron, exec` and stop.

Recommended order: the event protocol now, because the standalone
agent needs it anyway and it is Monk's own format. Then Salt lowstate
export and import, because the requisite model is the same and
`monk-salt` already exists. Then Puppet catalog import, read-only, for
people arriving with a catalog. Ansible stays a resource provider
(`monk-ansible` hosting modules through their JSON contract), not an
IR target, because Ansible has no graph to speak of.

## 6. Transport for the agent channel

Candidates, judged on what Emacs does natively on both ends: NDJSON
over SSH stdin/stdout, JSON lines over TCP or a Unix socket, ZeroMQ,
etcd, OTLP, Prometheus exposition, MQTT, journald.

Primary: NDJSON over the SSH session, `tail -F` of the agent's ledger
through TRAMP with `start-file-process` and a process filter. One
connection, no port, no daemon, no keys beyond SSH, backpressure and
ordering from the pipe. Fallback for disconnected periods: journald
with `SYSLOG_IDENTIFIER=monk`, replayed with `journalctl -o json`;
on macOS, the same NDJSON file with a byte offset. If a fleet store is
ever wanted, the controller forwards to OTLP logs; agents never speak
OTLP themselves.

## 7. Decisions taken

The standalone agent is `monk-agent.el`: batch loop, autopilot watch,
full converge on a timer, NDJSON ledger, journald mirror. The
controller gains a read-only `STANDALONE` view fed by tailing the
ledger over TRAMP. The event record above is Monk's serialization of
`monk-result` and is stable from now on; add fields, never rename.
Refresh becomes a separate signal from state. Autoedges come from
`monk-resource-needs` and `monk-resource-provides`. Meta gains `:retry`,
`:delay`, `:timeout`, `:poll`, `:sema`. Exported resources resolve at
compile time inside `monk-orchestrate`. Salt lowstate export is the
first interop target. Nothing from `mcl`, etcd, Send/Recv, or per-
resource goroutines is copied.
