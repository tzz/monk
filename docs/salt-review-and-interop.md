# SaltStack internals for Monk: execution model, what to copy, interop, and gotchas

## 0. How I checked this

- **Source:** the local checkout at `~/ai_workspace/source/salt`, commit `f37cdcd` (2026-09-18). `grains.items` reports `saltversion: 3009.0+0na.f37cdcd`, so this is the 3009 development line.
- **Imports:** `import salt.state` worked after two small stubs. The sandbox has no network, so pip could not install anything.
  - `looseversion` was stubbed from `distutils.version`.
  - `msgpack` was stubbed with pickle. Local mode does not send msgpack over the wire.
- **What ran:** the real `scripts/salt-call` and `scripts/salt-ssh` from the tree, on Linux (Ubuntu 22.04, aarch64).
  - `salt-call` used `-c /tmp/sc/conf --local` with a throwaway minion config, file roots and pillar roots.
  - `salt-ssh` ran against a private `sshd` on 127.0.0.1:2222.
  - I also called `State.requisite_in` and `State.compile_high_data` directly from Python.
- **Markers:** "(ran)" means I saw the output. "(code)" means I read it in source only. Anything specific to macOS is from code only, because the sandbox is Linux.
- **Removed from this tree** (the 3008.0 extension purge, #65970, `doc/topics/releases/3008.0.md:74`):
  - `salt/returners/sqlite3_return.py`. I cannot quote its schema from source. Section 2.8 compares against `salt/returners/postgres.py` instead.
  - `salt/renderers/pydsl.py`.
- **Renamed:** `check_requisite` does not exist here. The function is `State._check_requisites` (`salt/state.py:2967`).
- **Requisite graph:** requisite handling moved into `salt/utils/requisite.py` (`DependencyGraph`, `RequisiteType`).

---

## 1. Salt's execution model

### 1.1 Pipeline

1. **Render.** `salt.template.compile_template` (`salt/template.py:27`) reads the SLS file. `template_shebang` (`:158`) reads a first line like `#!jinja|yaml`. `check_render_pipe_str` (`:215`) splits it on `|` into `(render_func, argline)` pairs. Each renderer gets the output of the one before it. The default pipe is `renderer: "jinja|yaml"` in the minion defaults (`salt/config/__init__.py`).
2. **High data.** `BaseHighState.render_state` (`salt/state.py:4355`) handles `include`, `extend` and `exclude`. `_handle_iorder` (`:4573`) adds `{"order": N}` to every state declaration that has no order. N starts at `self.iorder = 10000` (`:3782`) and counts up in file order, when `state_auto_order: True` (the default).
3. **Verify.** `_verify_high` (`salt/state.py:395`) runs. For example, two functions in one declaration give `"Too many functions declared in state 'a' in SLS 'c'..."` (ran).
4. **Reverse requisites.** `State.requisite_in` (`salt/state.py:1976`) rewrites every `*_in`, plus `use` and `use_in`, into `__extend__` entries. `reconcile_extend(high, strict=True)` (`:1879`) then merges them.
5. **Excludes.** `apply_exclude` / `_apply_exclude` (`:354`) removes excluded SLS files and IDs.
6. **Compile to low data.** `State.compile_high_data` (`salt/state.py:1743`) makes one chunk per (ID, state module, name). A `names:` list expands into one chunk per name, each with a fractional `name_order`. Every chunk is added to `DependencyGraph`.
7. **Order.** `State.order_chunks` (`:1680`) does three things:
   - `_reconcile_watch_req` (`:1728`) turns `watch` into `require` when the module has no `mod_watch`.
   - `DependencyGraph.add_requisites` (`salt/utils/requisite.py:547`) adds the edges.
   - `aggregate_and_order_chunks` (`:603`) does a lexicographic topological sort (Kahn's algorithm with a heap). Cycles raise `DiGraphCycle`, and the error is `"Recursive requisites were found: ..."` (ran).
8. **Execute.** `State.call_chunks` (`salt/state.py:2788`) walks the ordered list. For each chunk:
   - `call_chunk` (`:3147`) runs `_mod_aggregate`, then `_mod_init`, then `_check_requisites`, then branches on the status.
   - `State.call` (`:2338`) runs the function.
   - After the main loop, `call_high` (`:3549`) runs `call_listen` (`:3443`) and then `call_beacons` (`:3425`).

`call_high` shows the top level in order: `reconcile_extend`, `verify_high`, `requisite_in`, `apply_exclude`, `compile_high_data`, `call_chunks`, `call_listen`, `call_beacons`.

### 1.2 Exact lowstate chunk (ran)

`salt-call --local --out=json state.show_low_sls demo` on a test SLS gave:

```json
{"state": "file", "name": "/tmp/sc/work/a.txt", "__sls__": "demo", "__env__": "base",
 "__id__": "/tmp/sc/work/a.txt", "contents": "hello Debian\n", "order": 10000, "fun": "managed"}
{"state": "cmd", "name": "touch /tmp/sc/work/b", "__sls__": "demo", "__env__": "base",
 "__id__": "touch-b", "creates": "/tmp/sc/work/b",
 "require": [{"file": "/tmp/sc/work/a.txt"}], "order": 10001, "fun": "run"}
{"state": "cmd", "name": "echo last", "__sls__": "demo", "__env__": "base",
 "__id__": "last-one", "order": 1010100, "fun": "run"}
```

- Jinja already ran: `{{ grains['os_family'] }}` became `Debian`.
- `order: last` became `1010100`. That is `cap + 1000000`, with `cap = max(order) + 100`.

The code for this is `_get_chunk_order`, `salt/utils/requisite.py:420-477`:
- `"last"` becomes `cap + 1000000`.
- `"first"` becomes `0`.
- A negative order becomes `cap + 1000000 + order`.

A chunk with `prereq` gets `"__prerequiring__": true`. The target gets `"__prereq__": true` and appears twice in the lowstate (ran).

The keywords that Salt consumes itself, and does not pass to the state function, are listed in three places. The union is `STATE_INTERNAL_KEYWORDS`.

- **`STATE_REQUISITE_KEYWORDS`** (`salt/state.py:78`): every `RequisiteType` value, plus `onfail_stop`.
- **`STATE_REQUISITE_IN_KEYWORDS`** (`:84`): `onchanges_in`, `onfail_in`, `prereq_in`, `watch_in`, `require_in`, `listen_in`.
- **`STATE_RUNTIME_KEYWORDS`** (`:87`): `fun`, `state`, `check_cmd`, `cmd_opts_exclude`, `failhard`, `onlyif`, `unless`, `creates`, `retry`, `order`, `parallel`, `prereq`, `prereq_in`, `reload_modules`, `reload_grains`, `reload_pillar`, `runas`, `runas_password`, `no_log`, `fire_event`, `saltenv`, `umask`, `use`, `use_in`, `__env__`, `__sls__`, `__id__`, the `__pub_*` keys, `__prereq__`, `__prerequiring__`, `__umask__`.

`RequisiteType` (`salt/utils/requisite.py:77-88`) lists the requisite types in order of precedence: `ONFAIL`, `ONFAIL_ANY`, `ONFAIL_ALL`, `REQUIRE`, `REQUIRE_ANY`, `ONCHANGES`, `ONCHANGES_ANY`, `WATCH`, `WATCH_ANY`, `PREREQ`, `PREREQUIRED`, `LISTEN`.

### 1.3 Exact result dict (ran)

Real `state.apply demo` output, one entry each:

```json
"cmd_|-touch-b_|-touch /tmp/sc/work/b_|-run": {
  "name": "touch /tmp/sc/work/b",
  "changes": {"pid": 51, "retcode": 0, "stdout": "", "stderr": ""},
  "result": true, "comment": "Command \"touch /tmp/sc/work/b\" run",
  "__sls__": "demo", "__run_num__": 1,
  "start_time": "08:44:07.337533", "duration": 1593.192, "__id__": "touch-b"}
"cmd_|-after-bad_|-echo never_|-run": {
  "changes": {}, "result": false, "duration": 0.002, "start_time": "08:44:08.932295",
  "comment": "One or more requisite failed: demo.bad", "__state_ran__": false,
  "__run_num__": 4, "__sls__": "demo", "__id__": "after-bad", "name": "echo never"}
```

- **Key:** `_gen_tag` (`salt/state.py:151`) builds `"{state}_|-{__id__}_|-{name}_|-{fun}"`.
- **`start_time`:** local time of day only, no date (`State.call`, near the end).
- **`duration`:** milliseconds, as a float.
- **`__run_num__`:** execution order. It counts per `State` instance.
- **`__state_ran__: false`:** set by `_assign_not_run_result_dict` (`:3333`) when a state did not run.
- **Optional keys:** `skip_watch`, `warnings`, `__parallel__`, `sub_state_run` (expanded into extra entries, `:3312`), `force_mod_watch` (popped).
- **`pchanges`:** zero occurrences in `salt/`. It is gone.
- **Compile errors:** the value under `local` is a list of strings instead of a dict. Examples: `["No matching sls found for 'nonexist' in env 'base'"]`, `["Specified state 'test.nope' was not found"]` (ran).

### 1.4 `test=True` and `result: None`

- `test` is not a keyword that the engine interprets. Each state module reads `__opts__["test"]` and returns `result: None` with predicted `changes`. Examples:
  - `cmd.run`: `'Command "X" would have been executed'`, `changes: {"cmd": X}` (`salt/states/cmd.py:918`, ran).
  - `file.managed`: calls `file.check_managed_changes` (`salt/states/file.py:3667`) and returns `changes: {"diff": "--- \n+++ \n@@ -1 +1 @@\n-x\n+y\n"}`. With `show_changes=False` it returns `{"diff": "<show_changes=False>"}`. A mode change adds `"mode": "0600"`. The comment ends with `"Note: No changes made, actual changes may\nbe different due to other states."` (ran).
- **Where the flag comes from:** `modules/state.py:_get_test_value` (`:623`) takes, in order:
  1. a `test=` kwarg in any case (`salt.utils.args.test_mode`, `salt/utils/args.py:361`),
  2. `config.get("test")`, which also reads pillar and grains,
  3. `__opts__["test"]`.

  `test=False` forces off.
- **(ran):** `test: True` in the minion config made `state.single` return `result: null` with no `test` arg. `test=False` overrode it. A pillar key `test: True` also turned on test mode.
- `check_result` (`salt/utils/state.py:331`) treats only `False` as failure. `None` is success. Test runs return rc 0.
- `check_failhard` (`salt/state.py:2852`) returns `False` in test mode. `failhard` never stops a test run.
- **Test mode cannot predict failures.** A dependent of a state that "would change" sees the old system. In test mode, the `onfail` handler of a state that returns `None` shows as "would have been executed" (ran: `on-bad` with `result: None`). The reason: `None` is not `True`, so the requisite counts as met (`_check_requisites`, `:3020-3035`).
- **The outputter counts `None` as success.** It shows `Succeeded: N (unchanged=K, changed=M)` (`salt/output/highstate.py:660-690`).

### 1.5 `onlyif`, `unless`, `creates`, `check_cmd`

In `State.call` (`salt/state.py:2419-2425`), if any of `onlyif`, `unless` or `creates` is in the chunk, and `"{state}.mod_run_check"` is not a loaded state function, the engine runs `self._run_check(low)` before the state function. No module in `salt/states/` defines `mod_run_check` now (checked with `rg`), so the generic path always runs.

`_run_check` (`:1103`):

- **Command options:** it copies `cwd`, `root`, `runas`, `env`, `prepend_path`, `umask`, `timeout` and `success_retcodes` from the chunk. `cmd_opts_exclude` can remove some. `shell` comes from the chunk or from `grains["shell"]`.
- **Order:** `onlyif` (`:1173`), then `unless` (`:1250`), then `creates` (`:1364`). The results are OR'ed. Any `True` means skip.
- **`onlyif` list:** the state runs only if every entry returns 0. The first non-zero gives `result True`, `"onlyif condition is false"`, `skip_watch True`.
- **`unless` list:** the state is skipped only if every entry returns 0. The first non-zero stops the loop and the state runs.
- **Entry forms:**
  - A string runs through `cmd.retcode(..., python_shell=True)`. On `CommandExecutionError`, `onlyif` treats the code as 100 and `unless` treats it as 0.
  - A dict `{fun: mod.func, args: [...], get_return: path, ...}` calls an execution module and uses `__context__["retcode"]` or truthiness.
- **`creates`:** a string or a list. For a list, all paths must exist. The check uses `os.path.exists` on the minion.
- **Test mode runs these commands** (ran: `both` returned `'onlyif condition is true\nunless condition is true'` in test mode).

`check_cmd` runs after the state function, in `State.call` (`:2497-2505`):
- If `"{state}.mod_run_check_cmd"` does not exist, or the function signature has no `check_cmd` parameter, `_run_check_cmd` (`:1330`) runs every command.
- The first non-zero gives `result False, "check_cmd determined the state failed"`. All zero gives `result True`.
- It **overrides** the state's result. That includes test mode (ran: `chk-false` returned `result False` under `test=True`, and `chk-true` turned `None` into `True`).
- `file.managed` has its own `check_cmd` argument. It checks a temp file before the file moves into place (`salt/states/file.py:3792`, `mod_run_check_cmd` at `:9173`). Same name, different meaning.

### 1.6 `mod_watch`, `mod_init`, `mod_aggregate`

Which modules define these (checked with `rg` in `salt/states/`):
- `mod_watch`: at, cmd, etcd_mod, mount, pkg, service, test
- `mod_init`: pkg
- `mod_aggregate`: iptables, pkg, selinux
- `mod_beacon`: file, pkg, service
- `mod_run_check_cmd`: file

**`mod_init`** (`State._mod_init`, `salt/state.py:1065`):
- It runs `"{state}.mod_init"(low)` the first time each state module is seen in a `State` instance.
- It is recorded in `self.mod_init` only if it returns truthy.
- `pkg.mod_init` (`salt/states/pkg.py:3625`) writes the "rtag" file, so the package database refreshes once per run.

**`mod_watch`** (`call_chunk`, `:3219-3262`):
- When the status is `"change"`, the state first runs normally.
- If that run produced no changes and no `skip_watch`, the engine copies the chunk with `sfun = fun`, `fun = "mod_watch"`, `__reqs__ = reqs`, calls it, and uses that result.
- If the normal run produced changes, `mod_watch` runs only when the state returned `force_mod_watch: True`. The two results are then merged.
- `cmd.mod_watch` (`salt/states/cmd.py:1330`) dispatches on `sfun` to `run`, `script` or `call`. `service.mod_watch` restarts or reloads.
- (ran) `watcher` returned `changes {"Requisites with changes": ["test: changer"]}, comment "Watch statement fired."`.

**`mod_aggregate`** (`_mod_aggregate`, `:1086`):
- It runs only if `_allow_aggregate` (`:1838`) is true. That needs `state_aggregate: True` (or a list of state names) in opts, or `aggregate: True` on the chunk, and `"{state}.mod_aggregate"` must exist.
- `DependencyGraph.aggregate_and_order_chunks` groups nodes of the same state type that have no path between them. The group limit is `max_group_size = 500`.
- At run time, `pkg.mod_aggregate(low, chunks, running)` (`salt/states/pkg.py:3654`) merges the names of every pkg chunk with the same `fun` and `fromrepo` that has not run yet into one `pkgs:` list. It marks those chunks `__agg__`. Chunks with `hold` are left out.

### 1.7 Requisite evaluation: `_check_requisites` (`salt/state.py:2967`)

For each requisite type, it computes a set of per-requisite stats (quoted from the code):
- `"unmet"`: the required chunk has not run yet.
- `"skip_req"`: the required result has `skip_req`.
- For `onfail*`: `"onfail"` if the required result is `True`.
- For `onchanges*`: `"onchanges"` if the required result is `False` or has empty changes, else `"onchangesmet"`. The comment says: *"onchanges is a soft trigger: a failed target is treated the same as a target with no changes."*
- Otherwise `"fail"` if the result is `False`.
- For `watch*` with changes: `"change"`.
- For `prereq`: `"premet"` if the test-mode result is `None`, else `"pre"`.
- Otherwise `"met"`, but only if `__state_ran__` is true (the default is true).

The `_any` and `onfail` adjustments come next. For `_any`: a `met` or `change` removes `fail`, `onchangesmet` removes `onchanges` and `fail`, and `met` removes `onfail`. For `_all` with `onfail`: `met` is removed.

The final status, first match wins:

| Status | Condition |
|---|---|
| `"unmet"` | any unmet |
| `"pending"` | parallel chunk still running |
| `"fail"` | any fail |
| `"skip_req"` | skip_req with onchangesmet or premet |
| `"skip_watch"` | skip_req with change |
| `"met"` or `"pre"` | pre present: met if premet also present, else pre |
| `"onfail"` | onfail without onchangesmet |
| `"onchanges"` | onchanges without onchangesmet |
| `"change"` | change |
| `"onfail"` | onfail |
| `"met"` | otherwise |

`call_chunk` (`:3147`) acts on the status:
- **`unmet`:** `_call_unmet_requisites` (`:3358`) recursively runs the missing dependencies first, out of list order, up to depth 20 (`"Recursive requisite found"`). This is how `require_in` targets placed earlier in the file still work.
- **`met`:** call the state.
- **`fail`:** result `False`, comment `"One or more requisite failed: sls.id, ..."`, `__state_ran__ False`. **The dependent counts as a failure, not a skip.** It adds to `Failed:` and to the retcode.
- **`onfail`:** result `True`, `"State was not run because onfail req did not change"`.
- **`onchanges`:** result `True`, `"State was not run because none of the onchanges reqs changed"`.
- **`pre`:** result `True`, `"No changes detected"`.
- **`skip_req`:** result `True`, `"State was not run because requisites were skipped by another state"`.
- **`change`:** see `mod_watch` above.

Consequence (code): if B requires A and A was not run because of `onchanges`, A has `__state_ran__ False`. No stat is added, so B's status falls through to `"met"` and B runs.

### 1.8 `_in`, `prereq`, `listen`, `use`, `_any`, `_all`

**`require_in` and the other `_in` forms** (`requisite_in`, `:1976`): X with `require_in: [{svc: Y}]` becomes an `__extend__` entry. It adds `require: [{state_of_X: X}]` to Y.
- (ran) `watch_in` on `conf` produced `svc: {"service": ["running", {"watch": [{"file": "conf"}]}]}`.
- `watch` then became `require` in the compiled chunk, because my fake `svc` had no `mod_watch`. That was `_reconcile_watch_req`.
- The `_in` keys stay in the low chunk as dead weight.

**`prereq`** (`DependencyGraph._add_prereq`, `salt/utils/requisite.py:352`):
- The engine adds a node `"{tag}_|-__prereq_test__"` for the target and sets `__prereq__` on it. `State.call` runs that node with `self.states.opts["test"] = True` (`salt/state.py:2410`), and stores the result in `self.pre`.
- Edges are added so the check runs before the prerequiring state, and the prerequiring state runs before the real target.
- If the check returns `None` (would change), the prerequiring state runs.
- If it returns `True` with no changes, the prerequiring state gets `"No changes detected"`.
- (ran) `pre.sls`: `stop-svc` returned `'No changes detected'`, because its target had nothing to do.
- (ran) `req.sls`: `stop-before` ran, then `prereq-target` ran with changes.

**`listen` and `listen_in`** (`call_listen`, `salt/state.py:3443`):
- These run after all chunks. For each listened-to state that has changes, the engine makes a new chunk `__id__ = "listener_<id>"` with `fun = "mod_watch"` and all requisites removed, and runs it through `call_chunks`.
- Several changes coalesce into one run per listener.
- (ran) The last entry was `test_|-listener_listener_|-listener_|-mod_watch` at `__run_num__` 17.

**`use` and `use_in`** (`requisite_in`, `:2140-2185`): these copy the arguments of another state at compile time. `name`, `names`, requisites and arguments that are already set are not copied. (ran) `user-of-args` got `comment: shared comment` from `base-args`.

**`require_any`, `onfail_any`, `onfail_all`** (ran):
- `require_any` on one failing and one passing target: the dependent ran.
- `onfail_any` on the same pair: it ran.
- `onfail_all`: not run, `"State was not run because onfail req did not change"`.

### 1.9 `failhard`, `onfail_stop`, `order`, `retry`, `parallel`, `umask`, `runas`

**`failhard`** (a per-chunk key, or the global `opts["failhard"]`):
- `check_failhard` (`:2852`) is true when the result is `False`, not in test mode.
- `call_chunks` returns immediately. `_call_unmet_requisites` sets `running["__FAILHARD__"]`.
- (ran) `hard.sls`: only `one` is in the output. `two` never ran and is absent from the return. rc 1.

**`onfail_stop`** only affects the overall success of the run:
- `check_onfail_requisites` and `search_onfail_requisites` (`salt/utils/state.py:239-328`) are used by `check_result(running, highstate=...)`.
- By default, a failed state counts as a failure even if an `onfail` handler succeeded (ran: `demo` returned rc 1 although `on-bad` handled `bad`).
- A handler with `onfail_stop: False` makes the handled failure not count (ran: `check_result(run, highstate=hs)` returned `True`).
- The code comment calls it *"an undocumented poorly named req type"* (`salt/state.py:1995`).

**`order`:** the sort key is `(_get_chunk_order(cap, node), "{state}{name}{fun}")`. `_get_chunk_order` returns `(min(own order, min order of direct dependents with the highest requisite weight), own order)`. A dependency's order is pulled down to its dependents' order. The result is valid but surprising. In a direct compile (ran):
- `pkgs(git)` got order 1.0001.
- `/etc/foo` (requires git) got order 1.
- git still ran first, and `emacs` (1.0002) ran after the service.

**`retry`** (end of `State.call`; `verify_retry_data` at `:2732`):
- Defaults: `{"until": True, "attempts": 2, "splay": 0, "interval": 30}`. `retry: True` uses the defaults. `retry: False` means 0 attempts.
- It re-calls the whole state until `result == until`, sleeping `interval + randint(0, splay)`. The comments are joined as `Attempt N: Returned a result of ...`.
- It is disabled with `parallel`.
- In test mode it only adds `"The state would be retried every ..."`.
- (ran) Output: `'Attempt 1: Returned a result of "False", with the following comment: "Failure!"\nFailure!'`.

**`parallel: True`** (`call_parallel`, `:2312`; `ParallelState`, `:712`; `_call_parallel_target`, `:2200`):
- The state runs in a separate process. The result is written as msgpack to `cachedir/<invocation_id>/<sha1(tag)>`, and `reconcile_procs` (`:2921`) collects it.
- `state_max_parallel` limits concurrency. Results carry `__parallel__`.
- (ran) Two parallel `sleep 1` states plus one dependent took 1.89 s wall time.

**`umask`:** `verify_data` (`:1637`) converts it to `__umask__` (octal). It is applied with `salt.utils.files.set_umask` around the call.

**`runas`:** it goes to `state_con["runas"]` and to the command module. `cmdmod._run` loads the target user's environment with `su -l` or `sudo -u` (`salt/modules/cmdmod.py:555-620`).

### 1.10 Failure semantics and passes

- A failure never stops the run unless `failhard` is set. Unrelated states continue.
- Dependents through `require`, `watch` and `prereq`, and their `_any` forms when every target failed, become `result False` with `__state_ran__ False`. Salt calls them failed, not skipped.
- `onchanges` dependents of a failed state do not run (result True). `onfail` dependents run.
- **Salt does one pass.** `call_chunks` calls each tag once (the `running` dict is keyed by tag). The only re-runs are:
  - `retry` (the same chunk, immediately),
  - `mod_watch` (a second function on the same chunk),
  - `listen` (new chunks at the end),
  - the `pending` loop for parallel chunks.

  There is no convergence loop and no flap detection. Converging means running highstate again, from cron or a schedule.

### 1.11 How to reproduce

```sh
salt-call -c CONF --local --out=json state.show_low_sls demo     # low chunks
salt-call -c CONF --local --out=json state.apply demo test=True  # result dicts
PYTHONPATH=. python3 -c 'import salt.state, salt.config; st=salt.state.State(opts); st.requisite_in(high); st.compile_high_data(high)'
```

---

## 2. What Monk should copy, in priority order

Monk runs several passes and already has check/repair, so several Salt features cost less in Monk.

### 2.1 `prereq`. High value, low cost

- **What:** run A before B, and only if B would change.
- **Why:** it covers "stop the service before the binary changes" and "drain before the kernel update". Monk's `monk-check` is already a dry run, so `:prereq REFS` needs no second test-mode path. Call `monk-check` on the target, and run the prerequiring resource only if the change plist is non-nil. If the target is kept, record A as kept with the comment "no changes detected".
- **Reference:** `DependencyGraph._add_prereq` (`salt/utils/requisite.py:352`), and the `"premet"`/`"pre"` branch of `_check_requisites` (`salt/state.py:3046-3051`, `3068-3072`).
- **Edge case to copy:** Salt's #68438 fix (`_call_unmet_requisites`, `:3390-3410`). When the target's own prereq chain reports no changes, the prerequiring state must not run either.

### 2.2 `mod_aggregate` plus a per-run cache (`__context__`). High value for brew and apt

- **What:** one `brew install a b c` instead of three calls, and one `brew info --json=v2 --installed` per run.
- **Reference:** `pkg.mod_aggregate` (`salt/states/pkg.py:3654`). Salt's `pkg.list_pkgs` caches in `__context__["pkg.list_pkgs"]` (`salt/modules/mac_brew_pkg.py:240-302`) and drops the cache after install or remove (`aptpkg.py:859, 921, 993, 1180`).
- **Monk shape:** a generic `monk-resource-aggregate` that a resource class can implement. The scheduler groups ready resources of the same class on the same machine with no dependency path between them, which is the same test as `DependencyGraph.has_path`. Add a per-run, per-machine hash table `monk-run-context` for list caches, and clear it on repair.

### 2.3 `mod_init` equivalent: a once-per-run hook per resource class

- **What:** `apt update` once per run per machine, only if an apt resource exists.
- **Reference:** `State._mod_init` (`salt/state.py:1065`) and `pkg.mod_init` (`salt/states/pkg.py:3625`).
- **Monk shape:** a generic `monk-resource-class-init (class machine run)` that runs the first time a class is seen. Record it in the run context.

### 2.4 `listen`: deferred, coalesced watch actions at the end of the run

- **What:** five config files change, and the service restarts once after the run converges, not five times.
- **Why this matters more for Monk:** with several passes, a `:watch-action` that fires in pass 1 can fire again in pass 2.
- **Monk shape:** `:listen REFS`. Collect triggers across all passes, then run the watch action once after convergence.
- **Reference:** `call_listen` (`salt/state.py:3443`).

### 2.5 `retry`: bounded retries inside a pass

- **What:** apt locks, network blips and `brew` lock files. Multi-pass is not a substitute, because passes stop when nothing changes, and a failed resource can look stable.
- **Monk shape:** `:retry (:attempts N :interval S :splay S :until STATUS)` with Salt's defaults (`until True`, `attempts 2`, `interval 30`, `splay 0`). In dry-run, only report the policy. Record every attempt in the comment.
- **Reference:** the end of `State.call` and `verify_retry_data` (`salt/state.py:2732`).

### 2.6 `check_cmd`: verify after repair

- **Why:** a command resource that exits 0 can still leave the system wrong.
- **Monk shape:** `:verify` (a function or command) on `monk-command`. Monk's next pass re-checks resources that have `check`, but a command without `:creates` or `:unless` is always "kept". `:verify` fills that gap.
- **Do not copy:** Salt's behavior of overriding the result in test mode (1.5).
- **Reference:** `_run_check_cmd` (`salt/state.py:1330`).

### 2.7 Low data as an import and export format

- **Import:** `state.show_low_sls SLS` (ran) gives flat chunks with `state`, `fun`, `name`, `__id__`, the arguments, and requisites as `[{mod: id-or-name}]`. Map each chunk to a `monk-salt` resource:
  - `(monk-salt ID "STATE.FUN" ARGS :require ...)`
  - Requisite targets resolve by `(state, id)` or `(state, name)`. This is `DependencyGraph.nodes_lookup_map` (`salt/utils/requisite.py:494-506`). Keep both keys.
- **Export:** emit high data and run it with `state.high '<json>' test=True` (ran). Requisites between the exported states are honored.
- This is also the batching fix for the bridge (see 3.9).

### 2.8 Wrapper generation from `sys.state_argspec` and `sys.state_doc`

(ran) The output shape:

```json
{"local": {"file.symlink": {"args": ["name","target","force",...], "defaults": [false,null,...],
                            "varargs": null, "kwargs": true}}}
```

- `defaults` lines up with the tail of `args`. `kwargs: true` means the function accepts `**kwargs`.
- Generate `monk-salt-file-symlink` with `&key` arguments and a docstring from `sys.state_doc file.symlink`. Cache the result per Salt version (`grains.get saltversion`).
- `sys.*` lives in `salt/modules/sysmod.py` (`__virtualname__ = "sys"`). Functions: `argspec` `:423`, `state_argspec` `:448`, `list_state_functions` `:525` (256 on this host), `doc` `:30`, `state_doc` `:86`.
- (ran) `sys.state_argspec macdefaults.write` returned `{}` on Linux, because `__virtual__` hides it. Generate wrappers on the target OS.

### 2.9 Outputter rules for Monk's UI

From `salt/output/highstate.py`:
- `state_verbose: False` hides `result True` with no changes (`:489-496`).
- `terse` prints one line per state.
- `mixed` prints full output only for failures.
- `changes` prints full output for failures and changes, terse for the rest (`:545-561`).
- The summary is `Succeeded: N (unchanged=K, changed=M)` and `Failed: F`. `None` counts under Succeeded, as "unchanged" (`:640-690`).
- `--output-diff` shows only changed states.
- `_id` variants show the ID instead of the name. `_color` colors diffs.

The `changes` mode is what people want by default. Monk's plan and report views already have more states than Salt (partial, degree), so copy only the visibility rule: collapse kept, expand repaired, would-repair and failed.

### 2.10 Event tag scheme for Monk's bus

Salt's tags come from `salt.utils.event.tagify(suffix, prefix, base="salt")` (`salt/utils/event.py:201`), joined with `/`. Real ones (ran through `tagify`):
- `salt/job/<jid>/ret/<id>`
- `salt/job/<jid>/prog/<id>/<run_num>`: per-state progress, from `State.event`, `salt/state.py:3092`, with `state_events: True`.
- `salt/state_result/<jid>/<id>/<name|fire_event>`
- `salt/minion/<id>/start` (`salt/minion.py:4088`)
- `salt/beacon/<id>/<beacon>/<data.tag>` (`salt/beacons/__init__.py:195, 206`)

Data carries `_stamp` (UTC ISO, set in `fire_event`, `:1006`). The jid is `%Y%m%d%H%M%S%f` in UTC (`salt/utils/jid.py:22`), so it sorts as a string.

Proposed scheme for Monk's JSON lines, etcd keys and agent socket:

```
monk/run/<run>/new                  targets, policy, dry_run
monk/run/<run>/pass/<n>             pass start
monk/run/<run>/prog/<host>/<seq>    one resource result (= salt/job/<jid>/prog/<id>/<run_num>)
monk/run/<run>/ret/<host>           per-host summary
monk/run/<run>/end                  converged, flapping, summary
monk/drift/<host>                   watch saw drift
monk/watch/<host>/<watcher>         like salt/beacon/<id>/<beacon>/
monk/agent/<host>/start
```

- Do not put resource names in tags. Paths contain `/`, and Salt's `state_result` tag has exactly that problem.
- Put `name` and `type` in the data, and use `<seq>` (like `__run_num__`) in the tag.
- Use a jid-style run ID so run IDs sort by time.

### 2.11 `order` including `last`, only as a tie-breaker

Useful for a "finally" resource (a report, a cleanup). Copy only `first` and `last` as tie-breakers inside the DAG. Do not copy the rule that pulls a dependency's order down to its dependents' (1.9). It makes `__run_num__` hard to predict.

### 2.12 `__virtual__` as resource availability with a reason

- Salt modules return `(False, "reason")`, and the loader shows the reason: `"State 'test.nope' was not found ... Reason: 'test.nope' is not available."` (ran).
- Monk: an `availability` generic per resource class that returns nil or a reason string. The run marks the resource `skipped` with that reason, instead of failing with an "exec: brew not found" error.
- **Reference:** `LazyLoader._process_virtual` (`salt/loader/lazy.py:1352`).

### 2.13 `mock` mode

- `state.apply SLS mock=True` (ran) does not call any state. Every result is `'Not called, mocked'`, but requisites still evaluate. `onchanges` gives "none of the onchanges reqs changed", and `onfail` gives "onfail req did not change".
- **Reference:** `mock_ret` (`salt/state.py:335`).
- Monk: `:mock` compiles and orders the graph with no host connection. It is useful for policy tests in ERT and for showing the order before connecting.

### 2.14 Beacons, reactor, and `disable_during_state_run`

- Monk's watch is already better integrated than Salt's beacons: filenotify locally, polling remotely.
- **Copy:** `disable_during_state_run` (`salt/beacons/__init__.py:164-186`). Monk's watcher must ignore events caused by its own repairs. If `monk-watch` does not already suppress self-caused events during a run, add it. I did not verify Monk's current behavior.
- **Copy:** the beacon data shape `{tag, path, change, id}`. inotify gives `change: "IN_MODIFY"` (`salt/beacons/inotify.py:289-293`). watchdog gives `change: event.event_type` (`salt/beacons/watchdog.py:166`).
- **Skip:** the reactor (`salt/utils/reactor.py`). It renders SLS files per event and dispatches to `local`, `runner`, `wheel` or `caller` (`ReactWrap.reaction_class`, `:314-317`, alias `cmd` becomes `local`). Monk's watch policy in Lisp is simpler.
- **Skip:** Thorium. It is a register machine over events, and its own docstring says it is unfinished (`salt/thorium/__init__.py:5-9`).

### 2.15 Lower priority

- **Compound matcher translator:** see 5.3. Useful for migration only.
- **Returners:** Monk has history and events. Copy only the hook idea: a `monk-result-functions` list.
- **History schema:** Salt's in-tree SQL returner (`salt/returners/postgres.py:59-99`):
  - `jids(jid PK, load)`
  - `salt_returns(fun, jid, return, full_ret, id, success, alter_time)`
  - `salt_events(id, tag, data, alter_time, master_id)`

  That is one row per minion per job, with JSON blobs. Monk's `runs` and `results` tables (`lisp/monk-history.el:98-127`) are better normalized. Salt has two things Monk lacks:
  - an `events` table with `tag` and `data`,
  - an order column.

  I suggest adding `seq` (like `__run_num__`) and `pass` to `results`, and optionally an `events(ts, tag, data)` table.
- **The removed sqlite3 schema, from memory and not verifiable here:** `jids(jid, load)`, `salt_returns(fun, jid, id, fun_args, date, full_ret, success)`.
- **`use`:** skip. A Lisp `let` or helper function does it.
- **`failhard`:** copy only as an opt-in per resource, `:fatal t`. It stops that machine's pass, never the whole orchestration. It is off by default, as in Salt.
- **`onfail_stop`:** Monk must decide whether a failure handled by `:onfail` counts against run success. Salt counts it by default. I suggest the explicit `:handled t` on the handler instead of the double negative `onfail_stop: False`.

---

## 3. Invoking Salt from Monk

### 3.1 Base command and output wrapping (ran)

```sh
salt-call --local --out=json --out-indent=-1 --log-level=quiet [--retcode-passthrough] FUN ARGS...
```

- **`local` wrapper:** `Caller.run` (`salt/cli/caller.py:108`) always prints `{"local": <return>}`. `--metadata` prints `{"local": {"jid", "return", "retcode", "id", "fun", "fun_args"}}` (ran).
- **`--out-indent=-1`:** `salt/output/json_out.py` turns a negative indent into `None`. Output is one line. Without it, the default indent is 4.
- **`--local`:** sets `file_client = "local"` (`salt/cli/call.py:67-68`). The rest of the minion config still applies: `id`, `test`, `state_output`, `user`, `file_roots`.
- **`--out-file=PATH`** (ran) writes the JSON to a file and leaves stdout empty. Over TRAMP, this separates the JSON from shell and stderr noise.
- **Startup time (ran, from source, Linux):** about 0.7 s for `test.ping`, 1.0 s for `state.single`. `--skip-grains` gave no measurable speedup here.
  - On macOS, `os_data` shells out to `sw_vers` three times and to `system_profiler SPHardwareDataType` (`salt/grains/core.py:1712, 2841-2853`), so grains will be slower there (not measured).
  - `grains_cache: True` with `grains_cache_expiration: 300` (defaults `False` and `300`) caches grains in `cachedir/grains.cache.p` (`salt/loader/__init__.py:1456`).
  - `--refresh-grains-cache` forces a refresh. With `--skip-grains`, grains are empty (ran: `grains.get os` returned `""`), so `os`-based `__virtual__` choices break. Do not use it for `pkg` or `service`.

### 3.2 Execution modules (ran)

```sh
salt-call --local --out=json --out-indent=-1 --log-level=quiet cmd.run_all 'echo hi; echo err >&2; exit 3'
{"local": {"pid": 37, "retcode": 3, "stdout": "hi", "stderr": "err"}}            # exit 1 (3 with --retcode-passthrough)

salt-call ... file.stats /tmp/sc/work/a.txt
{"local": {"inode": 5900, "uid": 1005, "gid": 1005, "group": "...", "user": "...", "atime": 1790599449.886,
           "mtime": ..., "ctime": ..., "size": 13, "mode": "0644", "type": "file", "target": "/tmp/sc/work/a.txt"}}

salt-call ... pkg.list_pkgs             {"local": {"adduser": "3.118ubuntu5", "apt": "2.4.14", ...}}
salt-call ... sys.argspec file.stats    {"local": {"file.stats": {"args": ["path","hash_type","follow_symlinks"],
                                                    "defaults": [null, true], "varargs": null, "kwargs": null}}}
salt-call ... sys.doc file.stats        {"local": {"file.stats": "\n    Return a dict containing the stats ..."}}
salt-call ... grains.items              {"local": {"os": "Ubuntu", "os_family": "Debian", ...}}
salt-call ... match.compound 'G@os_family:Debian and I@role:web'   {"local": true}
```

- On brew, `pkg.list_pkgs` lists each formula under every alias, plus `name` and `full_name`. Casks appear under `token`, `full_token` and `tap/token` (`mac_brew_pkg.py:273, 281-296`) (code).
- **Errors are not JSON.** `file.stats /nonexistent` printed `Error running 'file.stats': Path not found: /nonexistent` on stderr, stdout was empty, rc 1 (ran).

### 3.3 `state.single`, `state.low`, `state.high`, `state.sls_id`

- **`state.single FUN name=N [k=v ...] [test=True]`** (`salt/modules/state.py:2506`):
  - Sets `__id__ = name = N`. The key is `file_|-N_|-N_|-managed`, and `__sls__` is `null` (ran).
  - `name` can also be positional. `state.single file.managed /tmp/c.txt` without `test` created an empty file (ran).
  - It honors `test` and pillar overrides.
- **`state.low '<json chunk>'`** (`:591`):
  - Needs `state`, `fun`, `name` and `__id__`. Without `__id__` it crashed with `KeyError: '__id__'` (ran).
  - **It ignores a `test=True` argument.** `state.low '{...}' test=True` created the file (ran). Only `test: True` in the minion config or pillar affected it (ran).
  - Its return is one result dict under `local`, with no tag key (ran).
  - Do not use it from Monk.
- **`state.high '<json high>' test=True`** (`:640`): runs a whole graph with requisites. It returned two results, with the file not created (ran). It is the right batching call.
- **`state.sls_id ID SLS test=True`** (`:2036`): runs one ID plus its requisites. `touch-b` pulled in `a.txt` (ran). Note that `skip_watch: true` showed up in the result.
- **`state.apply SLS test=True`** is `sls`, or `highstate` with no SLS (`apply_`, `:773`). `--file-root` and `--pillar-root` make ad hoc directories work (ran):
  ```sh
  salt-call --local --file-root=/tmp/adhoc/states --pillar-root=/tmp/adhoc/pillar \
            --out=json --out-indent=-1 --log-level=quiet state.apply hi test=True
  {"local": {"test_|-hello_|-tzz_|-succeed_without_changes": {"name": "tzz", ...}}}
  ```
  `pillar='{"who": "override"}'` overrides pillar per call (ran).
- **Import calls:**
  - `state.show_sls SLS` returns high data, with `order` injected and `order: last` kept (ran).
  - `state.show_low_sls SLS` returns ordered chunks (ran).
  - `state.show_lowstate` and `state.show_highstate` do the same for top.sls. They returned `[]` here because no top file exists.
  - `state.show_top`, `state.graph` and `state.graph_highstate` exist (code, `:2464, 2362, 1839`).
- **`mock=True`**: see 2.13 (ran).

### 3.4 Return codes

| Case | Default rc | `--retcode-passthrough` |
|---|---|---|
| All states ok, or `None` in test mode | 0 | 0 |
| A state returned `False` | 1 | 2 (`EX_STATE_FAILURE`) |
| Compile error (list return) | 1 | 1 (`EX_STATE_COMPILER_ERROR`) |
| Pillar render error | 1 | 5 (`EX_PILLAR_FAILURE`) (code) |
| `cmd.run_all` / `cmd.run` exit 3 | 1 | 3 (`__context__["retcode"]`) |
| `CommandExecutionError` | 1, text on stderr | 1 |
| Unknown function | `sys.exit(-1)`, so 255 (code, `caller.py:157`) | same |

- Exit codes are in `salt/defaults/exitcodes.py`: `EX_GENERIC = 1`, `EX_STATE_COMPILER_ERROR = 1`, `EX_STATE_FAILURE = 2`, `EX_PILLAR_FAILURE = 5`, `EX_THIN_PYTHON_INVALID = 10`, `EX_THIN_DEPLOY = 11`, `EX_AGGREGATE = 20`.
- `state.*` sets the retcode through `_set_retcode` (`modules/state.py:100`), which uses `check_result`.
- For other functions, `Caller.call` takes `__context__["retcode"]`. If that is 0, it checks `return.get("result")` and `return.get("success")` (`caller.py:226-243`).
- The meaning of 1 overlaps: compiler error and generic error share the same code. Read the JSON, not the rc.

### 3.5 Parsing the result key

- Do not parse `__id__` or `name` out of the key. A name that contains `_|-` breaks `split_low_tag` (`salt/state.py:142`), which does a four-way unpack.
- Take `__id__`, `name` and `__run_num__` from the value. Take `state` as the part before the first `_|-` and `fun` as the part after the last one.
- Sort by `__run_num__`, not by key order.
- `start_time` has no date. Combine it with the call's own start date.

### 3.6 Noise and stderr

- Without `--log-level=quiet`, module import failures print tracebacks with `[ERROR   ]` on stderr. Examples here: `tls`, `x509` and the `channel` utils, because of my stale pyOpenSSL and tornado.
- `event.fire` in `--local` mode returned `{"local": false}` and then printed a `RecursionError` traceback from `SyncWrapper.__del__` on stderr at exit, even with `--log-level=quiet` (ran). That is not logging, so the quiet flag does not stop it.
- **Monk's `monk-exec` merges stderr into the output.** `lisp/monk-bridge.el:84` says so. Any stderr text breaks `json-parse-string`. Use one of these:
  - a separate stderr stream (`process-file` with `(BUFFER . STDERR-FILE)`),
  - `--out-file` to a temp file read through TRAMP,
  - `--out-indent=-1` and parse only the last line that starts with `{"local"`.

### 3.7 `test` from the minion config: a hazard for repair

`test: True` in `/etc/salt/minion`, in `minion.d`, or in pillar turns every run into a test run (ran). The bridge's repair (`lisp/monk-bridge.el:279`) passes no `test` argument. On such a host, repair gets `result: null` and signals `"salt: <comment>"`, which is correct but confusing. **Pass `test=False` explicitly on repair.**

### 3.8 Argument typing: a real bug in `monk-bridge--format-args`

`key=value` arguments go through YAML (`salt.utils.args.yamlify_arg`) (ran):

```
'yes' -> True   'no' -> False   'on' -> True   'null' -> None   '~' -> None
'0600' -> 600   '12:30' -> 750  '1.10' -> 1.1  '[a, b]' -> ['a','b']   'a,b' -> 'a,b'
key="12:30" -> '12:30'   key="1.10" -> '1.10'   env={"A": "1"} -> {'A': '1'}
```

- `monk-bridge--format-args` (`lisp/monk-bridge.el:93-103`) joins lists with commas. `pkgs=a,b` reaches Salt as the **string** `"a,b"`, not a list (ran: `test.arg pkgs=a,b` returned `"pkgs": "a,b"`).
- Git revisions like `1.10`, times like `12:30`, and values such as `on` and `no` are also mangled. `mode=0600` survives only because `file.managed` re-normalizes with `normalize_mode` (`salt/utils/files.py:910`). The file got mode 600 (ran).
- **Fix:** encode every value with `json-serialize`, so lists, alists and strings become JSON. JSON is valid YAML flow syntax, and a JSON string is quoted, so `"12:30"` stays a string.

### 3.9 Batching

- One `salt-call` per check plus one per repair costs about 1 s each. Fifty `monk-salt` resources give about 100 s per pass.
- Batch every `monk-salt` resource of a machine into one `state.high` call: `test=True` for check, then `test=False` with only the resources that need repair. Map results back by `__id__`.
- Requisites between those resources map to Salt requisites inside the high data. Cross-machine requisites stay in Monk.

### 3.10 salt-ssh with a roster generated from Monk's inventory (ran)

Roster YAML (keys from `doc/topics/ssh/roster.rst:36-78`):

```yaml
laptop:
  host: 127.0.0.1
  port: 2222
  user: tzz
  priv: /path/id_ed25519      # or "agent-forwarding"
  sudo: False
  sudo_user: root             # optional
  tty: False                  # True if sudo + requiretty
  timeout: 10
  thin_dir: /tmp/sshm/thin    # default /var/tmp/.%%USER%%_%%FQDNUUID%%_salt
  cmd_umask: 0077
  set_path: '$PATH:/usr/local/bin:/opt/homebrew/bin'
  ssh_options: ['ControlPath=~/.ssh/cm-%C']
  grains: {monk_groups: [web, laptop]}   # becomes real grains on the target
  minion_opts: {state_verbose: False}
```

```sh
salt-ssh -c /tmp/sshm/conf --roster-file=/tmp/sshm/roster -i \
         --out=json --out-indent=-1 --log-level=quiet laptop state.apply req test=True
{"laptop": {"test_|-changer_|-changer_|-succeed_with_changes": {...}, ...}}
```

- Output is keyed by roster ID, not `local`.
- `-r` (raw shell) returns `{"laptop": {"stdout": "...", "stderr": "", "retcode": 0, "parsed": null}}` (ran).
- Roster `grains` became real grains (ran: `grains.get monk_groups` returned `["web","laptop"]`).
- **Exit codes** go through `_ssh_cli_process_exit_code` (`salt/client/ssh/__init__.py:89`): 0 or 20 (`EX_AGGREGATE`), and thin failures keep their own codes (10, 11, ...). A failhard run returned rc 20 (ran). You cannot tell 1 from 2 from the rc. Read the JSON.
- **Bootstrap:**
  - `SSH_SH_SHIM` (`:158`) looks for `PYTHON_CMDS="python3 python27 ... /usr/libexec/platform-python"` (`:181`) on the target.
  - It copies a thin tarball into `thin_dir` (33 MB here, ran) and runs `salt-call` from it.
  - **The target needs Python 3.** `--relenv` instead downloads a Salt onedir tarball from `packages.broadcom.com` on the controller (`salt/utils/relenv.py:78`) and ships it. That avoids Python on the target but needs network.
- It needs a master config (`-c`) with `file_roots` for `state.apply`, plus `-i` or a known_hosts setup.
- **Compared with TRAMP push mode:** salt-ssh re-implements what TRAMP already does: connection reuse, sudo, and shipping files. Use it only to run existing SLS trees on hosts without a minion.

### 3.11 Python API

`salt.client.Caller` (`salt/client/__init__.py:2338`), `LocalClient` (`:285`) and `salt.runner.RunnerClient` (`salt/runner.py:23`) run inside Python only. From Emacs, each would mean a Python subprocess with a less stable interface than the CLI. **Do not use them.**

### 3.12 rest_cherrypy from `url.el` (code)

- **URL map:** `API.url_map` (`salt/netapi/rest_cherrypy/app.py:3118`) has `index /`, `login`, `logout`, `token`, `minions`, `run`, `jobs`, `keys`, `events` and `stats`, plus `hook` and `ws`.
- **Login:** `POST /login` with `{"username","password","eauth"}` returns `{"return": [{"token": "...", "start", "expire", "user", "eauth", "perms"}]}` and an `X-Auth-Token` header.
- **Run:** `POST /` with header `X-Auth-Token` and body `[{"client": "local", "tgt": "*", "fun": "test.ping", "arg": [], "kwarg": {}}]` returns `{"return": [{"minion": ...}]}`.
- **`/run`:** takes `username`, `password` and `eauth`, or `token`, inside each lowstate chunk, without sessions (`Run`, `:2282`).
- **`/events`** (`:2420`):
  - It is `text/event-stream`. It first sends `retry: 400\n`, then `tag: <tag>\ndata: <json>\n\n` per event.
  - It takes the token only from the `X-Auth-Token` header or the session cookie. A query string gives HTTP 400.
  - `url.el` does not stream SSE well. Use `make-network-process` or `curl -N` in a process filter.
- **Clients must be enabled.** `netapi_enable_clients` defaults to `[]` (`salt/config/__init__.py:1950`), so nothing is enabled until you list clients.
- It is worth doing only if a salt-master already exists. Not for masterless use.

---

## 4. Interop the other way

| Idea | How (verified?) | Worth it? |
|---|---|---|
| **Monk facts as grains** | `grains_dirs: [/dir]` with a module whose public functions return dicts. (ran) `monk_facts()` read a JSON file that Monk writes, and `grains.get monk` returned the facts. `grains.setval k v` writes `<config_dir>/grains` (ran), one call per key, and persists. | **Yes**, if Salt also runs on these hosts. Use `grains_dirs` plus a JSON file. `setval` is slow and leaves state behind. |
| **Inventory vars as `ext_pillar`** | Signature `ext_pillar(minion_id, pillar, *args, **kwargs)`. The engine calls it as `(self.minion_id, pillar, *val)` or `**val` (`salt/pillar/__init__.py:1042-1079`). With no Python: `ext_pillar: [{cmd_json: '/path/monk-vars %s'}]`, where `%s` is the minion ID (`salt/pillar/cmd_json.py`). (ran) In `--local` mode, pillar gained `{"monk": {"host": "monktest", "groups": ["web"], "vars": {...}}}`. | **Yes.** Zero Python. Monk exports one JSON file per host, or runs `emacsclient --eval`. |
| **Returner into Monk's ledger** | `returner_dirs: [dir]`, module with `returner(ret)`, `salt-call --return=monk ...` (ran). `ret` has `jid, return, retcode, out, id, fun, fun_args, success`. In local mode, `id`, `fun` and `fun_args` are added only when returners are set (`caller.py:263-274`). | **Yes** for Salt runs that start outside Monk (cron highstate), so Monk's history sees them. **No** for Monk-started runs, which should parse the JSON directly. |
| **Monk events onto the Salt bus** | `salt.utils.event.get_event("minion"|"master", ...).fire_event(data, tag)`, or `salt-call event.fire_master`. In `--local` mode, `event.fire` returned `false` plus a stderr traceback (ran). It needs a running minion or master. | **No**, unless a master exists. Then use `event.fire_master` or `POST /hook/<tag>`. |
| **Salt bus into Monk** | `salt-call --local state.event pretty=False` or `salt-run state.event`. Output is `"<tag>\t<json>"` lines (`salt/modules/state.py:2851`) (code). Or SSE `/events`. | **Maybe.** Easy with a process filter, but only useful if a master or minion daemon runs. |
| **Monk agent as a salt-ssh target** | salt-ssh needs sshd, not a Unix socket. | **No.** Generate a roster from the inventory instead (3.10). Cheap. |
| **`_states/monk.py` running `emacs --batch`** | A state module that shells out to `emacs --batch -l monk -f monk-batch-apply` and turns Monk results into Salt result dicts. Status mapping: `repaired` becomes `True` plus changes, `would-repair` becomes `None`, `failed` becomes `False`, `partial` becomes `False` with the degree in the comment. | **Low.** Emacs startup on every call, and Monk must be installed on the target. Only for a fleet that is driven by Salt. |
| **Monk `salt://` file server** | A fileserver backend module on a master. | **No.** Monk sources files from Emacs and TRAMP already. |

Mapping a returner `ret` to Monk's `monk-event-result` (`lisp/monk-event.el:96`), one event per state key:

| Monk field | From Salt |
|---|---|
| `event` | `"result"` |
| `host` | `ret.id` |
| `run` | `ret.jid` |
| `pass` | `1` (Salt has one pass) |
| `type` | `"salt:" + state + "." + fun` (from the key) |
| `name` | `value.name` (keep `value.__id__` in `native`) |
| `status` | `result false` → `failed`. `result null` → `would-repair`. `true` with non-empty `changes` → `repaired`. `true` with empty changes → `kept`. `__state_ran__ false` with `true` → `skipped`. `__state_ran__ false` with `false` → `skipped` with the root cause in the comment. |
| `changed` | `changes` non-empty and `result` not false |
| `degree` | 1 or 0. Salt has no partial state. |
| `changes` | `value.changes` (an alist) |
| `diff` | `value.changes.diff` if it is a string |
| `comment` | `value.comment` |
| `duration_ms` | `value.duration` |
| `dry_run` | any `result null`, or `fun_args` containing `test=True` |
| `native` | `{tool: "salt", sls: __sls__, id: __id__, run_num: __run_num__, start_time}` |

It converts cleanly, except that `start_time` has no date. Use `jid_to_time(jid)` (`salt/utils/jid.py:52`) for the date.

---

## 5. Mapping table

### 5.1 Concepts

| Salt | Monk | Notes |
|---|---|---|
| grains | facts | see 5.2 |
| pillar (top.sls plus `ext_pillar`) | inventory group vars | Salt masks pillar output by default here. `pillar.get` and `pillar.items` print `**********` unless `unmask=True`. `pillar.raw` is unmasked (ran). |
| top.sls targeting, compound matchers | CELL `:when` over the inventory | see 5.3 |
| nodegroups | inventory groups | |
| SLS ID (`__id__`) and `name` | resource name | Salt has two identifiers. Requisites match either one (`nodes_lookup_map`). |
| `state.function` | resource type | |
| `test=True` | `:dry-run`, plan | |
| `result: None` | `would-repair` | |
| `result: False` | `failed` | |
| `result: True`, empty `changes` | `kept` | |
| `result: True` with `changes` | `repaired` | |
| dependent of a failed require | `skipped` | **Salt reports it as `result False`**, counted as Failed. |
| (none) | `partial`, `degree` 0..1 | **Salt has no equivalent.** `merge_subreturn` (`salt/utils/state.py:369`) only folds sub-results into True, None or False. |
| one pass plus `order` | several passes until converged, flap detection | Salt re-runs only through `retry`, `mod_watch` and `listen`. |
| beacons plus reactor | monk-watch | |
| returners, job cache (`local_cache`: `.load.p`, `return.p`, `out.p`) | history (sqlite) plus the event ledger | |
| orchestrate runner (`state.orchestrate`, `salt.function`/`salt.state` states) | `monk-orchestrate` (one DAG across machines) | Monk's single DAG is stronger. Orchestrate states are sequential calls to targets. |
| salt-ssh | TRAMP push mode | |
| salt-minion | monk-agent | |
| master event bus (ZeroMQ, TCP or WS publish; IPC `master_event_pub.ipc`) | etcd, agent socket, JSON lines | |
| `salt://` | no Monk equivalent. The nearest is files next to the policy, or a TRAMP path. | |
| `__context__` | nothing today | propose a per-run, per-machine context (2.2) |
| `unless` / `onlyif` | `:not-if` / `:only-if` | Salt's take shell strings or execution module calls. Monk's take Lisp functions. |
| `creates` | `:creates` (on `monk-command`) | |
| `require`, `watch`, `onchanges`, `onfail` | same names | Monk adds cross-machine refs and thresholds. |
| `require_any`, `onfail_any`, `onfail_all`, `onchanges_any`, `watch_any` | thresholds `:min 1` or `:fraction 1.0` | Monk's thresholds cover these. |
| `*_in` | missing | skip, see 6 |
| `prereq` | **missing** | copy, 2.1 |
| `listen` | **missing** | copy, 2.4 |
| `use` | **missing** | skip, use Lisp |
| `order` / `last` | **missing** | copy as a tie-breaker only, 2.11 |
| `retry` | **missing** | copy, 2.5 |
| `check_cmd` | missing | `:verify`, 2.6 |
| `mod_aggregate`, `mod_init` | missing | copy, 2.2 and 2.3 |
| `failhard` | missing on purpose | at most `:fatal t` per machine |
| `mock` | missing | cheap to add, 2.13 |
| `__virtual__` | missing | availability with a reason, 2.12 |
| `parallel` | Monk's cross-machine scheduler | Salt's `parallel` is per state inside one minion, in separate processes. |
| `runas` | `:sudo` (TRAMP) | |

### 5.2 Grains and facts (ran on Ubuntu 22.04 aarch64; macOS from code)

| Monk fact | Monk source (`lisp/monk.el:575-635`) | Salt grain | Salt value, Ubuntu (ran) | Salt value, macOS (code) |
|---|---|---|---|---|
| `kernel` | `uname -s` | `kernel` | `Linux` | `Darwin` |
| `arch` | `uname -m` | `cpuarch` | `aarch64` | `arm64` |
| (none) | | `osarch` | `arm64` (from `dpkg --print-architecture`, `_osarch`, `core.py:2551`) | `cpuarch` |
| `os-family` | Darwin gives `"Darwin"`. Linux maps debian and ubuntu to Debian, fedora/rhel/centos to RedHat, anything else to `capitalize(id)`. | `os_family` | `Debian` | **`MacOS`** (`core.py:2846`) |
| `distribution` | Darwin gives `"MacOS"`. Linux gives `capitalize(ID)`. | `os` | `Ubuntu` | `MacOS` |
| `distribution-release` | `VERSION_CODENAME` | `oscodename` | `jammy` | (not set, code) |
| `distribution-version` | `VERSION_ID` / `sw_vers -productVersion` | `osrelease` | `22.04` | `sw_vers -productVersion` |
| (none) | | `osmajorrelease`, `osrelease_info` | `22`, `[22, 4]` | from `osrelease` |
| (none) | | `osfinger` | `Ubuntu-22.04` | `macOS 15.1-15` (ran through `_osrelease_data`: it uses `osfullname` = `"macOS <ver>"` because `"MacOS"` is not in its list, `core.py:2575-2616`) |
| `nodename` | `uname -n` | `nodename` | `claude` | `platform.uname` |
| `hostname` | first label of `uname -n` | `host` (first label of fqdn), `localhost` (`socket.gethostname()`) | `claude`, `claude` | same (`core.py:2944`) |
| (none) | | `fqdn`, `domain` | `claude` | |
| (none) | inventory name | `id` | `monktest` (from config or `--id`) | |
| `user` | `user-login-name` / `id -un` | `username` (the Salt process user, `salt/grains/minion_process.py:54`) | `kind-stoic-ptolemy` | |
| `home` | `~` / `$HOME` | **none** (not in `grains.items`, ran) | | |
| (none) | | `init` | `systemd` | `launchd` |
| (none) | | `shell`, `virtual`, `pythonversion`, `saltversion` | `/bin/sh`, `physical`, `[3,10,12,"final",0]`, `3009.0+0na.f37cdcd` | |

**How the family names differ:**
- **macOS.** Salt uses `os = os_family = "MacOS"`. Ansible uses `ansible_os_family = "Darwin"` and `ansible_distribution = "MacOSX"`. Monk uses `os-family "Darwin"` (as Ansible does) and `distribution "MacOS"` (as Salt's `os` does). That is a mix. Pick one and document it.
- **Red Hat.** Salt and Ansible both say `RedHat`. Monk only maps `fedora`, `rhel` and `centos` by ID or `ID_LIKE`. `capitalize` gives `"Rhel"`, `"Centos"`, `"Almalinux"` and `"Rocky"` for `distribution`. Salt says `RedHat`, `CentOS`, `AlmaLinux` and `Rocky` (`_OS_NAME_MAP`, `core.py:1812`).
- **SUSE.** Salt's `os_family` is `Suse`. Monk gives `"Opensuse-leap"` and similar. Salt's `_OS_FAMILY_MAP` (`core.py:1890-1960`) has about 60 entries.
- **Arch.** Salt maps Manjaro, EndeavourOS and Antergos to `Arch`. Monk falls back to `capitalize(id)`.
- **Mint.** Salt's `os` is `Mint`. Monk gives `"Linuxmint"`.

If Monk wants these names to match Salt's, port `_OS_FAMILY_MAP` and `_OS_NAME_MAP` as a table, or read `ID_LIKE` more fully.

### 5.3 Compound matchers and CELL (ran through `match.compound`)

Engines, from `TARGET_REX` (`salt/utils/minions.py:35`) and `compound_match.match`: `G` grain, `P` grain pcre, `I` pillar, `J` pillar pcre, `L` list, `N` nodegroup (expanded in place), `S` ipcidr, `E` pcre on the ID, `R` range (only with `seco.range`), `T` resource, `M` managing_minion. Operators are `and`, `or`, `not`, `(` and `)`, separated by spaces. A bare `not` gets an implicit `and`. The whole string is joined into Python and passed to **`eval()`**.

Semantics to keep in a translator:
- `G@` and `I@` use `subdict_match` (`salt/utils/data.py:1058`). It lowercases both sides and uses **fnmatch glob**. `G@os:ubuntu` matched `Ubuntu`, and `G@os_family:Deb*` matched (ran).
- Nested keys use the `:` delimiter. A different delimiter looks like `G%@os_family%Debian` (ran). List grains match any element: `G@osrelease_info:22` matched (ran).
- `P@` and `J@` use `re.match`, anchored at the start, case-insensitive through the same lowercasing.
- `E@` uses `re.match(tgt, minion_id)`, case-sensitive.

| Salt compound | CELL |
|---|---|
| `web*` (bare glob on the ID) | `(match (name) "\\`web")` (anchor both ends for a full glob) |
| `G@os_family:Debian` | `(= (fact os-family) "Debian")` (Salt is case-insensitive, CELL `=` is exact) |
| `G@os:Deb*` | `(starts-with (fact distribution) "Deb")` or `(match (fact distribution) "\\`[Dd]eb")` |
| `P@os:(Ubuntu\|Debian)` | `(in (fact distribution) (list "Ubuntu" "Debian"))` or `(match ...)` |
| `I@role:web` | `(= (var role) "web")` |
| `J@role:w.b` | `(match (var role) "\\`w.b")` |
| `L@a,b` | `(in (name) (list "a" "b"))` |
| `E@web.*` | `(match (name) "\\`web.*")` |
| `N@webs` | `(group webs)` |
| `S@10.0.0.0/8` | no CELL equivalent. It needs an `ipv4` fact and a CIDR function. |
| `not X`, `X and Y`, `X or Y`, `( ... )` | `(not X)`, `(and X Y)`, `(or X Y)`, nesting |
| `G@kernel:Darwin` | `(class darwin)` (Monk's hard classes include the lowercased kernel) |

**Going back from CELL to compound** works only for `class`/`fact`/`var`/`group`/`name` with `=`, `in`, `match`, `and`, `or`, `not`. The fact names must be mapped through 5.2, since Salt's `os_family` is `MacOS` where Monk's `os-family` is `Darwin`. Comparisons (`<`, `>`), `size` and arithmetic have no compound form.

**Monk bug risk (code, not run):** CELL's `match` uses `string-match-p` (`lisp/monk-cell.el:216-218`) and does not bind `case-fold-search`. Emacs defaults it to `t`, so CELL regexes are case-insensitive or not depending on the caller's buffer. Bind it explicitly. I suggest nil, with an explicit case-insensitive operator if needed.

---

## 6. Gotchas and things not to copy

1. **Jinja inside YAML.** Text templating of a data format. It causes whitespace and indentation bugs, YAML 1.1 type guessing (`on`, `0600`, `12:30`, see 3.8), and render order surprises: pillar is compiled before states, and grains are fixed at render time. Monk's Lisp policies avoid all of this. Do not add a renderer pipeline.
2. **The `_in` reversal.** `requisite_in` is about 230 lines (`salt/state.py:1976-2197`). It rewrites high data through `__extend__`, guesses targets for malformed entries, and leaves the `_in` keys in the chunks. Monk refs resolve in Lisp. If reverse edges are wanted, add them at graph build time, not by rewriting declarations.
3. **`pchanges` history.** `pchanges` was removed. The `changes` dict has no schema across modules: `{"diff": ...}`, `{"pid", "retcode", "stdout", "stderr"}`, `{"testing": {"old", "new"}}`, `{"Requisites with changes": [...]}`. Keep Monk's changes plist typed per resource, plus `:degree`.
4. **Surprises in `__run_num__` and ordering.** Auto order starts at 10000 per declaration. `names` get fractional orders. Dependencies pull the order of their dependents down. `order: last` is `cap + 1000000`. Out-of-order recursive calls happen for unmet requisites. The order you read in the file is not the order that runs. Monk should show its real schedule, which the plan view already does.
5. **`failhard` as a global.** `opts["failhard"]` in the minion config stops every run at the first failure, on every host. It is off in test mode, so the plan and the apply behave differently. Do not copy the global.
6. **`mod_watch` naming.** `watch` silently becomes `require` when the module lacks `mod_watch` (`_reconcile_watch_req`). `mod_watch` replaces the state's own result when the state had no changes. `force_mod_watch` exists to patch that. Monk's explicit `:watch-action` is better.
7. **`state.single` and `state.low` confusion.**
   - `state.single` sets `__id__ = name`.
   - `state.low` needs `__id__` and ignores `test=True` as an argument. It applied a real change (ran).
   - `state.high` honors `test`.
8. **`saltenv` sprawl.** `saltenv`, `pillarenv`, `pillarenv_from_saltenv`, `lock_saltenv`, `top_file_merging_strategy`, `__env__`. A lot of machinery for multi-environment file servers. Monk has one policy tree.
9. **Name aliasing through `__virtual__`.** `pkg` is `aptpkg.py` or `mac_brew_pkg.py`. `service` is `systemd_service.py` or `mac_service.py`. `sys` is `sysmod.py`. File names do not match call names, and a module that fails `__virtual__` disappears with only a log line. Jump-to-source needs the real file. The loader knows it, `grep` does not.
10. **Grains go stale, and the grains cache.** Grains are computed at minion start. They refresh only on `saltutil.refresh_grains`, on `grains_refresh_every`, or when the cache expires (`grains_cache_expiration: 300`). A package install that changes `init` or `os` is invisible until a refresh. Monk re-probes facts per run, which is correct. Keep it.
11. **ZeroMQ transport.** Three transports (`salt/transport/zeromq.py` 2441 lines, `tcp.py` 3136, `ws.py` 751). Custom framing with `TAGEND = "\n\n"` and msgpack payloads. Each client has its own publish and request semantics. Monk's etcd plus the Unix socket JSON ops are enough.
12. **When pillar refreshes.** Pillar compiles at minion start and on `saltutil.refresh_pillar`. `state.apply` uses the in-memory pillar unless a `pillar=` override is passed. `ext_pillar` runs after the top-file pillar unless `ext_pillar_first` is set (`salt/pillar/__init__.py:1164-1205`). Output is now masked by default. Monk reading inventory vars at compile time is simpler.
13. **Inconsistent retcodes.** 1 means both compiler error and generic error. Without `--retcode-passthrough`, all failures become 1. salt-ssh flattens everything to 20. Execution errors print text instead of JSON. A dependent of a failure counts as failed. An `onfail`-handled failure still counts, unless the handler has `onfail_stop: False`. Monk should keep one clear rule: the run fails if and only if some resource failed and no handler with `:handled` covered it.
14. **`check_cmd` in test mode.** It can turn `None` into `True` or `False` in a dry run (ran). Any `:verify` in Monk must not run in dry-run.
15. **`onlyif` and `unless` run commands during test.** A dry run in Salt is not free of side effects if those commands have effects. Monk's `:only-if` and `:not-if` are Lisp and run on the controller side. Keep them without side effects.
16. **`start_time` without a date,** and `duration` in ms. Monk stores full timestamps. Keep that.
17. **Explicit `user:` in the minion config.** With `user: root` and a non-root caller, `salt-call` exits with `[CRITICAL] Salt configured to run as user "root" but unable to switch.` (ran). On such hosts the bridge must use `:sudo t`.
18. **Removed and moved code.** `sqlite3_return`, `pydsl` and many execution modules moved to "saltext" packages in 3008. Code that assumes a module exists (for example `sys.state_argspec macdefaults.write` on Linux) returns `{}`. Probe with `sys.list_state_functions` on the target.

### Relevant to open Monk issues

`salt/modules/mac_brew_pkg.py` added `trust`, `untrust`, `is_trusted` and `list_trusted` (`brew trust --json=v1`, versionadded 3008.2, `:916-1066`), plus the `pkg.trusted` state (`salt/states/pkg.py:3981`). They cover taps, formulae, casks and commands under `$HOMEBREW_REQUIRE_TAP_TRUST`. Your notes list an open brew trust issue in Monk, and this is a working reference for that flow. `_call_brew` (`:148`) runs brew as the owner of the brew binary via `sudo -i -n -H -u <owner>`.

`macdefaults` now reads with `defaults export <domain> -` and writes with `defaults import`, through `plistlib` (`salt/modules/macdefaults.py:375-440`). It supports nested keys (`key_separator`), `dict_merge` and `array_add`. That is more robust than `defaults write` type flags, and Monk's `osx-default` could use the same export/import approach.
