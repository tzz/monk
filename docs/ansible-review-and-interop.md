# Ansible (ansible-core) execution model and lessons for Monk

## 0. What I checked and how

- **Source tree.** `~/ai_workspace/source/ansible`, commit `7ec731bfd2d` (2026-09-25). `lib/ansible/release.py` gives `__version__ = '2.23.0.dev0'`.
- **Real runs.** The sandbox has Python 3.10. I ran target-side modules directly, for example `python3 -m ansible.modules.lineinfile` with `PYTHONPATH=lib`. Every module JSON sample below is real output from those runs.
- **Not run.** The controller CLIs refuse to start. The error is `ERROR: Ansible requires Python 3.13 or newer on the controller` (`pyproject.toml: requires-python = ">=3.13"`). Statements about `ansible`, `ansible-playbook`, `ansible-inventory` and `ansible-doc` output come from reading the code, not from runs.
- **Not in the tree.** The `json` callback is not here. `lib/ansible/config/ansible_builtin_runtime.yml` redirects `callback: json` to `ansible.posix.json`. Any shape I give for it comes from memory and is marked as such.

Four findings change what Monk does today:

1. **`-o` and `--tree` are deprecated for removal in 2.23**, which is the version of this tree. See `cli/arguments/option_helpers.py` `add_output_options`: `DeprecatedArgument(version='2.23', ...)`. The callbacks `oneline` and `tree` carry `removed_in: "2.23"`. `monk-ansible` depends on `-o`.
2. **`-o` drops the diff.** `CallbackBase._dump_results` deletes `diff` from the JSON when verbosity is below 3. The `oneline` callback has no `v2_on_file_diff`. So `ansible -o --check --diff` never prints a diff. `monk-ansible--diff-string` receives nothing at default verbosity.
3. **Unsupported check mode stops meaning "skipped".** Today `AnsibleModule.__init__` returns `skipped=True` when check mode is unsupported. The source marks that return `deprecated ... core_version='2.25'`. The replacement `AnsibleActionNoCheckMode` (errors/__init__.py) returns `changed=False` with a message. After that change, Monk would read "this module cannot dry-run" as `kept`.
4. **Top-level facts such as `ansible_os_family` are deprecated.** `vars/manager.py` has `_DEPRECATE_TOP_LEVEL_FACT_TAG`, version `'2.24'`, with the help text "Use `ansible_facts["fact_name"]` (no `ansible_` prefix) instead." Translate `when:` clauses from both spellings.

---

## 1. Ansible's execution model

### 1.1 Object tree

- **`Playbook`** is a list of `Play`.
- **`Play`** (`playbook/play.py`) has these attributes: `hosts`, `gather_facts`, `gather_subset`, `gather_timeout`, `fact_path`, `vars_files`, `roles`, `handlers`, `pre_tasks`, `tasks`, `post_tasks`, `force_handlers`, `max_fail_percentage`, `serial`, `strategy`, `order`.
- **`Play.compile()`** flattens a play into blocks in this order:
  - `pre_tasks`, then flush
  - roles and `tasks`, then flush
  - `post_tasks`, then flush

  Each flush is an implicit task: `action='meta'`, `args['_raw_params']='flush_handlers'`, `implicit=True`, `tags=['always']`. With `force_handlers`, each section becomes a `Block` whose `always` is the flush block.
- **`Block`** (`playbook/block.py`) has `block`, `rescue` and `always`.
- **`Task`** (`playbook/task.py`) has `action`, `args`, `async`, `changed_when`, `delay` (default `5`), `failed_when`, `loop`, `loop_control`, `poll`, `register`, `retries`, `until`.
- **Inherited keywords** (`playbook/base.py`): `connection`, `remote_user`, `vars`, `module_defaults`, `environment`, `no_log`, `run_once`, `ignore_errors`, `ignore_unreachable`, `check_mode`, `diff`, `any_errors_fatal`, `throttle` (default 0), `timeout`, `become*`. Mixins add `when` (Conditional), `tags` (Taggable), `notify` (Notifiable), and `delegate_to`/`delegate_facts` (Delegatable).
- **`Handler(Task)`** (`playbook/handler.py`) adds `listen` and a `notified_hosts` list.

### 1.2 Playbook and strategy loop

**Batches.** `PlaybookExecutor.run` splits play hosts into batches with `_get_serialized_batches`:
- `serial` is a list of ints or percentages, converted by `pct_to_int`.
- The last element repeats until all hosts are used.
- An empty `serial` gives one batch of all hosts.

For each batch it calls `TaskQueueManager.run(play)`. The play stops early in two cases: every host in the batch failed, or the strategy set `RUN_FAILED_BREAK_PLAY` (8).

**Linear strategy** (`plugins/strategy/linear.py`, `StrategyModule.run`):
1. `_get_next_task_lockstep` picks the next task that all remaining hosts share.
2. For each host it gets vars (`VariableManager.get_vars`) and templates `task.action`. A `meta` action runs `_execute_meta` in the controller. Any other action goes to `_queue_task`, which forks a `WorkerProcess`.
3. `_wait_on_pending_results`, then `_process_pending_results`.
4. It checks `any_errors_fatal` and `max_fail_percentage` (section 1.9).

**Free strategy** (`free.py`) does not wait for other hosts. It warns that `run_once` and `any_errors_fatal` are not supported, and sets `ALLOW_BASE_THROTTLING = False` because it throttles by itself.

**Exit code.** `StrategyBase.run` returns `RUN_UNREACHABLE_HOSTS` (4) if any host is unreachable. Otherwise it returns `RUN_FAILED_HOSTS` (2) if any host failed, else `RUN_OK` (0).

### 1.3 One task on one host, end to end

**`TaskExecutor.run()`** (`executor/task_executor.py`):
- `_get_loop_items()` evaluates `loop` or `with_<lookup>`.
- With no loop, it calls `_execute()`.
- With an empty list, it records `set_skipped('No items in the list')`.
- With items, it calls `_run_loop(items)`.

**`_execute_internal()`** does the work:
1. `_calculate_delegate_to()` templates `delegate_to` and merges the vars of the delegated host.
2. It evaluates `when` before `post_validate`. False returns `UnifiedTaskResult.record_conditional_false`, which sets `skip_reason='Conditional result was False'` and `false_condition=<expr>`.
3. `task.post_validate(...)` finalizes the fields.
4. It picks connection vars (`cvars`). With delegation these come from `ansible_delegated_vars[delegate_to]`. It then calls `_get_connection` and `_set_become_plugin`.
5. `_get_action_handler_with_module_context` loads the action plugin. Most modules use `plugins/action/normal.py`.
6. The retry loop (section 1.6) calls `self._handler.run(task_vars=...)` inside `TaskTimeoutError.alarm_timeout(self._task.timeout)`.
7. After each attempt it applies facts, evaluates `changed_when` and `failed_when`, then evaluates `until`.
8. It copies `notify` into the result, plus `delegated_host` and `_ansible_delegated_vars` when delegated.

**Action plugin.** `normal.ActionModule.run` calls `ActionBase.run`, which enforces `_supports_check_mode` and `_VALID_ARGS`. It then calls `self._execute_module(task_vars=..., wrap_async=...)`. That method is in `plugins/action/__init__.py`:
1. **Tmpdir.** If pipelining is off, `_make_tmp_path()` runs the shell `mkdtemp` under `remote_tmp` (default `~/.ansible/tmp`). The name comes from `_generate_temp_dir_name()`.
2. **Internal args.** `_update_module_args()` adds the `_ansible_*` keys (section 1.4).
3. **Build.** `_configure_module()` calls `module_common.modify_module`, which builds the AnsiballZ payload.
4. **Transfer.** Without pipelining it writes `AnsiballZ_<name>.py` into the tmpdir with `_transfer_data`, then fixes permissions with `_fixup_perms2`. With pipelining, the payload goes to the interpreter's stdin (`module_in_data = module_data`).
5. **Run.** `_low_level_execute_command(cmd, sudoable=..., in_data=...)` wraps the command in `become.build_become_command` when become applies, then calls `connection.exec_command`.
6. **Parse.** `_parse_returned_data()` runs `_filter_non_json_lines(stdout, objects_only=True)` and `json.loads`, then `UnifiedTaskResult.from_module_result_dict`. If parsing fails, the result is failed with `module_stdout`, `module_stderr`, `rc` and the message "Module result deserialization failed.". A missing interpreter gets its own message.
7. **Clean up.** `remove_internal_keys()`, then `normal.py` calls `_remove_tmp_path`.

**AnsiballZ payload.** `module_common.py`, near line 1358, builds:
```python
params = dict(ANSIBLE_MODULE_ARGS=module_args,)
encoded_params = json.dumps(params, cls=encoder)
wrapper = f"""{code}
if __name__ == "__main__":
    _ansiballz_main(
ansible_module=..., module_fqn=..., profile=..., date_time=..., rlimit_nofile=...,
params=encoded_params, extensions=..., secrets=..., zip_data=<base64 zip of module + module_utils>,
)"""
```
`_internal/_ansiballz/_wrapper.py` `_ansiballz_main` works as follows:
- It writes the zip to a temp dir named `ansible_<module>_payload_*`.
- It puts the zip first on `sys.path` and adds a `sitecustomize.py`.
- It calls `_loader.run_module(json_params=...)`. That sets `basic._ANSIBLE_ARGS` and runs the module as `__main__` with `runpy.run_module`.
- A module must exit by itself. Falling off the end raises `'New-style module did not handle its own exit.'`.
- An uncaught exception prints `{"failed": true, "exception": {...}}` and exits 1.

**Debug modes.** Run with `ANSIBLE_KEEP_REMOTE_FILES=1 ... -vvv` to find the kept file. Then:
- `python AnsiballZ_x.py explode` writes `debug_dir/` with the sources and an `args` file.
- `python AnsiballZ_x.py execute` runs the exploded code with that `args` file.

The code is in `debug()` in `_wrapper.py`.

**SSH connection** (`plugins/connection/ssh.py`):
- `ssh_args` defaults to `-C -o ControlMaster=auto -o ControlPersist=60s`.
- `control_path_dir` defaults to `~/.ansible/cp`.
- `_create_control_path` hashes `host-port-user[-connection][-pid]` with SHA-1 and keeps 10 hex characters.
- `pipelining` defaults to `False` (`ANSIBLE_PIPELINING`, `ansible_pipelining`). `ConnectionBase.is_pipelining_enabled` needs `has_pipelining`, and the become plugin must allow pipelining.
- `local.py` also sets `has_pipelining = True`.

### 1.4 Module input JSON

The module reads `{"ANSIBLE_MODULE_ARGS": {...}}`. `basic._load_params` raises `"ANSIBLE_MODULE_ARGS not provided."` when the key is missing.

`_update_module_args` adds these keys: `_ansible_check_mode`, `_ansible_no_log`, `_ansible_debug`, `_ansible_diff`, `_ansible_verbosity`, `_ansible_version`, `_ansible_module_name`, `_ansible_syslog_facility`, `_ansible_selinux_special_fs`, `_ansible_socket`, `_ansible_shell_executable`, `_ansible_keep_remote_files`, `_ansible_tmpdir`, `_ansible_remote_tmp`, `_ansible_ignore_unknown_opts`, `_ansible_target_log_info`, `_ansible_tracebacks_for`, `_ansible_inject_invocation`.

The module side allowlists them in `module_utils/common/parameters.py` `PASS_VARS`. Any other `_ansible_*` key fails like an unknown parameter:
```
{"failed": true, "msg": "Unsupported parameters for (basic.py) module: _ansible_bogus. Supported parameters include: ..."}
```

Example input:
```json
{"ANSIBLE_MODULE_ARGS": {"path": "/tmp/x.txt", "line": "hello", "create": true,
                         "_ansible_diff": true, "_ansible_check_mode": true}}
```

### 1.5 Module output JSON

`AnsibleModule.exit_json(**kw)` and `fail_json(msg, *, exception=..., **kw)` call `_return_formatted`:
- `add_path_info` adds `uid`, `gid`, `owner`, `group`, `mode`, `state`, `size` when the result has `path` or `dest`.
- It adds `invocation: {"module_args": params}` only if `_ansible_inject_invocation` is true. Otherwise it removes `invocation`.
- It adds `warnings` and `deprecations` from the global lists.
- It adds `_ansible_new_secrets` when a `no_log` parameter holds a value.

`_record_module_result` prints `'\n' + json`. Exit code is 0 for `exit_json` and 1 for `fail_json`. `fail_json` sets `failed=True` and `msg`.

Real outputs:

```
lineinfile, check+diff, file absent:
{"changed": true, "msg": "line added", "backup": "", "diff": {"before": "", "after": "hello\n",
 "before_header": "/tmp/x.txt (content)", "after_header": "/tmp/x.txt (content)"}}

lineinfile, real run on existing file (diff is a LIST: content + attributes):
{"changed": true, "msg": "line added", "backup": "", "diff": [{"before": "", "after": "a=1\n",
 "before_header": "/tmp/mt/z (content)", "after_header": "/tmp/mt/z (content)"},
 {"before_header": "/tmp/mt/z (file attributes)", "after_header": "/tmp/mt/z (file attributes)"}]}

file state=directory mode=0750 (diff before/after are DICTS):
{"path": "/tmp/mt/d", "changed": true, "diff": {"before": {"path": "/tmp/mt/d", "state": "absent", "mode": "0755"},
 "after": {"path": "/tmp/mt/d", "state": "directory", "mode": "0750"}}, "uid": 1005, "gid": 1005,
 "owner": "...", "group": "...", "mode": "0750", "state": "directory", "size": 4096}

cron, check+diff:
{"jobs": ["monk"], "envs": [], "changed": true, "diff": {"before": "", "before_header": "crontab",
 "after": "#Ansible: monk\n5 * * * * true\n", "after_header": "crontab"}}

command, real:
{"changed": true, "stdout": "hi", "stderr": "", "rc": 0, "cmd": ["echo", "hi"], "start": "...", "end": "...", "delta": "0:00:00.001139", "msg": ""}

command, check mode (partial support):
{"changed": false, ..., "msg": "Command would have run if not in check mode", "skipped": true}

command, rc=1:
{"changed": true, ..., "rc": 1, ..., "failed": true, "msg": "The command exited with a non-zero return code."}

module without supports_check_mode, in check mode:
{"skipped": true, "msg": "remote module (basic.py) does not support check mode"}

lineinfile warning (legacy profile serializes typed objects):
"warnings": [{"event": {"msg": "The regular expression is an empty string, ...", "__ansible_type": "Event"}, "__ansible_type": "WarningSummary"}]

no_log parameter present:
{"changed": false, "echo": "hunter2secret", "msg": "pw is hunter2secret", "_ansible_new_secrets": ["hunter2secret"]}
```

**Controller normalization** (`_internal/_task.py`, `UnifiedTaskResult`):
- **`failed`**: true if `_failed` is unset and `rc` is not 0. Then `failed_when_result` wins. Then any loop item's `failed_when_result`. Then `_failed`. Then any loop item failed.
- **`changed`**: true if any loop item changed.
- **`skipped`**: for a loop, true only if all items were skipped.
- **Reserved keys.** A module cannot set keys from the `Source.ACTION` fields: `_ansible_no_log`, `_ansible_verbose_always`, `_ansible_verbose_override`. Unknown `_ansible_*` keys also get "Removed reserved key ... from module result." Action plugins can set the `Source.ACTION` keys.
- **`as_result_dict()`** adds `stdout_lines` and `stderr_lines`.
- **`no_log`.** The callback result keeps only `PRESERVE = {'_ansible_no_log', 'attempts', 'changed', 'deprecations', 'exception', 'retries', 'warnings'}` and adds `censored="the output has been hidden due to the fact that 'no_log: true' was specified for this result"`.

### 1.6 Conditions and results: when, changed_when, failed_when, until, register, loops

**Order after each attempt** (`_execute_internal`):
1. It applies `ansible_facts` to task vars through `VariableLayer.CACHEABLE_FACT`, then `INCLUDE_VARS`, `EPHEMERAL_FACT`, `REGISTER_VARS`.
2. If `retries > 1`, it sets `utr.attempts = attempt`.
3. If the result is not skipped, it evaluates `changed_when` (`set_changed_when_result`), then `failed_when` (`set_failed_when_result`).
4. Any `failed_when` moves the existing exception into `failed_when_suppressed_exception`. A true `failed_when` sets the message "A 'failed_when' expression evaluated to 'True'.". An error inside either expression marks the task failed.
5. `until`:
   - `retries = 1 + task.retries`. If only `until` is set, it adds 3.
   - `delay < 0` becomes 1.
   - The loop stops when `until`, or `[not utr.failed]` when `until` is unset, is true. Otherwise it sends `v2_runner_retry` and sleeps.
   - When attempts run out, `failed=True` and `attempts = retries - 1`.

**`register`.** `Task._validate_register` turns `register: x` into a projection, `{x: _task.polymorphic_result}`. This tree also accepts a dict of named projections such as `register: {blar: _task.result}`. Registered values apply even when the task fails or is skipped (`registered_values` docstring). Tests in `plugins/test/core.py` read the result dict:
- `failed`, `success`/`succeeded`, `changed`, `skipped`, `unreachable`/`reachable`, `finished`, `started`, `timedout`.
- `changed` also scans `results[*].changed` when the top-level key is missing.

**Loops.**
- `loop_control` has `loop_var` (default `item`), `index_var`, `label`, `pause`, `extended`, `extended_allitems`, `break_when`.
- `_run_loop` sends `v2_runner_item_on_ok`, `_failed` or `_skipped` for each item.
- The final result has a `results` list. Each item carries its own `item` or `loop_var` key, `ansible_loop_var`, and `_ansible_item_label`.

### 1.7 Check mode and diff mode

- **Selection.** `--check` or task `check_mode: true` sets `_ansible_check_mode`. Task `check_mode: false` forces a real run inside a `--check` run.
- **Controller gate.** If an action plugin has `_supports_check_mode = False`:
  - `ActionBase.run` raises `AnsibleActionSkip` "This action (...) does not support check mode." That exception is deprecated for 2.25.
  - `_update_module_args` raises "check mode is not supported for this operation".
- **Module gate.** `AnsibleModule(supports_check_mode=False)` exits with `skipped=True` in check mode. The module has already validated its arguments, so it still fails on bad arguments. The comment reads "Fail for validation errors, even in check mode".
- **Modules without check-mode support in core** (checked with a scan): `dnf`, `dnf5`, `expect`, `tempfile`, `uri`, `wait_for`. `command`/`shell` have partial support: they skip unless `creates`/`removes` decide.
- **Diff.** `--diff` or task `diff: true` sets `_ansible_diff`. Modules build `diff` only when `module._diff` is true. `CallbackBase._get_diff` renders:
  - `before`/`after`. Mappings are serialized as JSON or YAML. `None` becomes `""`.
  - `before_header`/`after_header`, printed as `before: <header>` and `after: <header>`.
  - `dst_binary`, `src_binary`, `dst_larger`, `src_larger` (skip notices).
  - `prepared` (pre-rendered text).
  - A list of such dicts.

  The context size is `C.DIFF_CONTEXT`. The unified diff comes from `difflib.unified_diff`.

### 1.8 Handlers

- **Notify only on change.** In `StrategyBase._process_pending_results`, a notification counts only `if result_utr.notify and task_result.utr.changed`.
- **Deferred.** `iterator.add_notification(host, name)` stores the name. Handlers run at the next `flush_handlers`, implicit or `meta: flush_handlers`.
- **Lookup.** `search_handlers_by_notification` scans handlers in reverse, so the last definition with that name wins. It then yields every handler whose `listen` contains the name. A name that finds nothing warns or errors (`ERROR_ON_MISSING_HANDLER`).
- **Once per host.** `Handler.notify_host` deduplicates per host.
- **After failure.** A failed host goes to `COMPLETE` (`_set_failed_state`). Its pending handlers do not run unless `force_handlers` is set.

### 1.9 Failure control

- **`ignore_errors`.** The result is still `failed`. Stats count `ok` and `ignored`, plus `changed` if the task changed. `v2_runner_on_failed(..., ignore_errors=True)` fires. It does not cover unreachable hosts (`ignore_unreachable` is separate), undefined variables or syntax errors.
- **Unreachable.** `AnsibleConnectionFailure.as_task_result` sets `utr.unreachable = True`. The host goes into `_unreachable_hosts` and `play._removed_hosts`. Stats count `dark`.
- **`rescue`/`always`.** `PlayIterator._set_failed_state` moves the host to `RESCUE` if the block has one, else `ALWAYS`, else `COMPLETE`. On failure inside a block with a rescue:
  - Stats count `rescued`, not `failures`. The count happens at the moment of failure.
  - The host is taken out of `_removed_hosts`.
  - `ansible_failed_task` and `ansible_failed_result` become non-persistent facts.
- **`any_errors_fatal`** (linear only). After a task finishes on all hosts: if any result failed or is unreachable, and `any_errors_fatal or run_once`, and the task has no `ignore_errors`, every remaining host is marked failed.
- **`max_fail_percentage`.** The check is `len(self._tqm._failed_hosts) / iterator.batch_size > percentage`. The comparison is strictly greater. `_failed_hosts` lives on the TQM and survives across batches, so earlier failures count against a later batch. This is an observed quirk of the code, not documented behavior. On a trip it marks all hosts failed, sends `v2_playbook_on_no_hosts_remaining`, and sets `RUN_FAILED_BREAK_PLAY`.
- **`throttle`.** `_queue_task` lowers the worker rewind point to `throttle`. It is ignored when `run_once` is set.
- **`run_once`.** `get_task_hosts` returns all hosts, so registered vars and facts go to every host. A failure marks all hosts failed.
- **`delegate_to`.** The connection uses the vars of the delegated host. Task vars stay those of the original host. Facts go to the original host unless `delegate_facts: true`.
- **Become.** Become wraps the command when it is sudoable, become is on, and the users differ or `BECOME_ALLOW_SAME_USER` is set. `_fixup_perms2` handles tmpdir access for an unprivileged become user. It tries, in order:
  1. setfacl
  2. chown
  3. macOS `chmod +a`
  4. chgrp to `ansible_common_remote_group`
  5. world-readable, only with `allow_world_readable_tmpfiles`

### 1.10 Facts and caching

- **Implicit gather.** `PlayIterator.__init__` adds a setup block. Its task is `action='gather_facts'`, `name='Gathering Facts'`, `tags=['always']`, with args from `gather_subset`, `gather_timeout` and `fact_path`.
- **Policy.** `DEFAULT_GATHERING` is one of:
  - `implicit` (default): gather each play and ignore the cache.
  - `explicit`: gather only when asked.
  - `smart`: gather once per host. It checks `_ansible_facts_gathered`, which the `gather_facts` action sets.
- **Storage.** Facts go to `VariableManager.set_host_facts`, which merges into `_fact_cache` (`host_cache |= facts`). The default plugin is `memory`. `jsonfile` writes one file per host into `fact_caching_connection`. The file is written to a temp file and renamed.
  - Expiry: `has_expired` compares `time.time() - st_mtime` with `_timeout` (default `86400`). 0 means never.
  - Wrapping: `PluginInterposer` (`_internal/_plugins/_cache.py`) prefixes keys with `s1_` and stores `{"__payload__": "<json string>"}`.
- **Names.** `namespace_facts` removes the `ansible_` prefix inside `ansible_facts`, except for `ansible_local`. So `ansible_facts.os_family` is the modern name. `INJECT_FACTS_AS_VARS` (default True, deprecated) also puts `ansible_os_family` at top level.
- **Real `setup` output**, `gather_subset: ["min"]`, 0.5 s:
  - Top level: `{"ansible_facts": {...55 keys...}}`.
  - Values: `ansible_os_family='Debian'`, `ansible_distribution='Ubuntu'`, `ansible_distribution_release='jammy'`, `ansible_distribution_version='22.04'`, `ansible_architecture='aarch64'`, `ansible_hostname`, `ansible_nodename`, `ansible_fqdn`, `ansible_system='Linux'`, `ansible_kernel`, `ansible_user_id`, `ansible_user_dir`, `ansible_pkg_mgr='apt'`, `ansible_service_mgr='bwrap'`, `ansible_env.HOME`, `ansible_local={}`, `gather_subset`, `module_setup=true`.
- **`facts.d`** (`module_utils/facts/system/local.py`, `LocalFactCollector`):
  - It reads `fact_path/*.fact`, default `/etc/ansible/facts.d`.
  - An executable file runs, and a non-zero rc becomes an error string. Any other file is read.
  - Parsing tries JSON, then INI.
  - The result lands at `ansible_local.<basename>`.
  - Real run: `{"ansible_facts": {"ansible_local": {"ini": {"general": {"owner": "tzz"}}, "monk": {"role": "workstation", "classes": ["darwin", "emacs"]}}}}`.
- **`pkg_mgr`** (`system/pkg_mgr.py`). It walks the `PKG_MGRS` path list; the last existing path wins. Examples: `/usr/bin/apt-get` gives `apt`, `/opt/homebrew/bin/brew` gives `homebrew`, `/usr/bin/pacman` gives `pacman`. Then OS-family overrides apply: RedHat goes through `_check_rh_versions`, Debian is forced to `apt`. OpenBSD has its own collector.
- **`service_mgr`.** It reads `/proc/1/comm` or `ps -p 1`, maps the name through `proc_1_map`, and falls back to OS checks such as Darwin `launchd`. An unknown PID 1 passes through verbatim. That is why the sandbox reports `bwrap`.
- **`os_family`.** `Distribution.OS_FAMILY_MAP` includes `'Darwin': ['MacOSX']` and `'Archlinux': ['Archlinux','Antergos','Manjaro']`. `get_distribution_Darwin` sets `distribution='MacOSX'` and the version from `sw_vers`. `distribution_release` stays `platform.release()`, the Darwin kernel version.

### 1.11 Variable precedence

This comes from the `VariableManager.get_vars` docstring and code, lowest first:
1. role defaults
2. group vars, in the order of `VARIABLE_PRECEDENCE`: `all_inventory, groups_inventory, all_plugins_inventory, all_plugins_play, groups_plugins_inventory, groups_plugins_play`
3. host vars
4. facts
5. play vars
6. `vars_files`
7. role vars and task vars
8. include_vars cache
9. set_fact and register (non-persistent)
10. role params and include params
11. extra vars
12. magic vars

`sort_groups` orders groups by `(depth, priority, name)`, so deeper child groups win. `ansible_group_priority` calls `Group.set_priority`; the default is 1. `combine_vars` replaces keys unless `hash_behaviour=merge`.

`Host.get_magic_vars` gives `inventory_hostname`, `inventory_hostname_short` and `group_names` (sorted, without `all`). `_get_magic_variables` adds `groups`, `ansible_play_hosts_all`, `ansible_play_hosts`, `ansible_play_batch` and `hostvars`.

### 1.12 Callbacks and stats

- **Methods** (`plugins/callback/__init__.py`): `v2_runner_on_ok/failed(ignore_errors)/skipped/unreachable`, `v2_runner_item_on_ok/failed/skipped`, `v2_runner_retry`, `v2_runner_on_start(host, task)`, `v2_playbook_on_start`, `v2_playbook_on_play_start`, `v2_playbook_on_task_start(task, is_conditional)`, `v2_playbook_on_handler_task_start`, `v2_playbook_on_notify(handler, host)`, `v2_playbook_on_include`, `v2_playbook_on_no_hosts_remaining`, `v2_on_file_diff(result)`, `v2_playbook_on_stats(stats)`.
- **`CallbackTaskResult`** (`executor/task_result.py`):
  - `host`, `task`, `task_name`, `task_fields` (`task.dump_attrs()`).
  - `is_changed()`, `is_skipped()`, `is_failed()`, `is_unreachable()`, `exception`, `warnings`.
  - `result`: a masked copy built with `as_result_dict(for_callback=True, censor_callback_result=task.no_log or utr.no_log)`.
- **Internal keys.** `_dump_results` strips `_ansible_*`. At verbosity below 3 it also drops `invocation` and `diff`, and it always drops `exception`, `warnings` and `deprecations`.
- **`AggregateStats.summarize(host)`** returns `{ok, failures, unreachable (=dark), changed, skipped, rescued, ignored}`. `custom` holds `set_stats` data, keyed `'_run'` for global values.

---

## 2. What Monk should copy (priority order)

**1. An `unreachable` status.** Ansible separates `dark` from `failures`. It also stops sending tasks to an unreachable host in the same play. Monk has no such status. A TRAMP connect failure today shows up as N failed resources with N copies of the same error.

Recommendation:
- Add `monk-status-unreachable` at the machine level.
- On a connection error, mark that machine's remaining resources `unreachable` without trying them.
- Dependents on other machines skip with reason `(unreachable MACHINE)`.
- Keep it out of `degree` math. It is "unknown", not "0".
- Add `:ignore-unreachable` for laptops that come and go.

**2. `changed_when` / `failed_when` as Lisp predicates.** This is the most used idiom in site.yml for `command`/`shell`.

Recommendation:
- Add `:changed-when` and `:failed-when` on `monk-command`. Each takes a function `(lambda (result) ...)` or a CELL form over `(result :rc)`, `(result :stdout)`, `(result :stderr)`.
- Copy Ansible's order: rc!=0 gives failed by default, then `:changed-when`, then `:failed-when`.
- An error inside the predicate fails the resource and keeps the original error.

**3. `until` / `retries` / `delay` inside one pass.** This is not the same as multi-pass convergence. It covers "wait for port", "wait for apt lock", "wait for a service to answer".

Recommendation:
- Add `:until PRED :retries N :delay SECONDS`.
- Use Ansible's defaults: 3 extra retries when only `:until` is set, delay 5.
- Record `attempts` in the result.
- Do not block Emacs. Run the retry on a timer inside the async runner.

**4. Declared parameters, the `argument_spec` model.**
- Spec keys (`common/parameters.py`): `type`, `required`, `default`, `choices`, `aliases`, `elements`, `options` (nested), `no_log`, `fallback`, `deprecated_aliases`, `removed_in_version`. Cross-field keys: `mutually_exclusive`, `required_together`, `required_one_of`, `required_if`, `required_by`.
- Types: `str list dict bool int float path raw json bytes bits`.
- Recommendation: `monk-defresource` declares slots with the same vocabulary. Validation happens at policy load, not at check time.
- Copy the error text: "Unsupported parameters for (X): foo. Supported parameters include: ...".
- Generate `describe-function` docs and the section 3.7 importer from the same spec.

**5. `no_log` and secret masking.** Ansible collects `no_log` values (`_list_no_log_values`) and masks them everywhere. A `no_log` result keeps only the `PRESERVE` keys plus `censored`. Monk writes results to an event ledger, sqlite, etcd and a socket. One leak goes to all four.

Recommendation: `:no-log t` on a resource, and a `no-log` flag on spec slots. The event writer replaces `changes`, `diff` and `comment` with a censored marker and keeps `status`, `degree` and `attempts`.

**6. Structured diff.** Keep `diff` as a list of plists: `(:before :after :before-header :after-header)`. `:before`/`:after` can also be plists for attribute changes, as the `file` module does. Render them with `diff-no-select` (task #29), with headers `before: <hdr>` and `after: <hdr>`. One resource can carry a content diff and an attribute diff, as `lineinfile` does.

**7. Run stats and exit codes.** Emit a `summarize`-style plist per machine: `ok changed failures unreachable skipped rescued ignored`, plus Monk's `partial` and `would-repair`. For batch mode (`emacs --batch -f monk-apply-batch`) use Ansible's bit-safe codes: 0 ok, 2 some failed, 4 some unreachable. Cron jobs and CI already understand them.

**8. Local facts (`facts.d`).** Machines can publish their own facts. Recommendation:
- Add a `local` probe that reads `/etc/monk/facts.d/*.fact` and `~/.config/monk/facts.d/*.fact` with Ansible's rules: executable runs, then JSON, then INI.
- Put the result under `(fact local)`. CELL then works: `(= (get (fact local) 'monk 'role) "workstation")`.
- Also read `/etc/ansible/facts.d` for migration.

**9. Fact cache with TTL.** Ansible uses `jsonfile` with a mtime-based TTL of 86400 s, plus `gathering=smart`. Monk has sqlite. Add a `facts` table with `(machine, key, value, gathered-at)`. Treat facts newer than `monk-fact-ttl` as fresh. Add `--flush-cache` as `monk-facts-flush`. This makes CELL queries and `monk-plan` work with no connection.

**10. `pkg_mgr` / `service_mgr` auto-detection with generic resources.** Ansible's `package` and `service` action plugins pick the backend from the fact. They run a narrowed `setup` (`filter='ansible_pkg_mgr', gather_subset='!all'`) when the fact is missing. Recommendation:
- Add probes `pkg-mgr` (`homebrew`/`apt`/`dnf`/`pacman`, same path table) and `service-mgr` (`launchd`/`systemd`).
- Add `monk-package` and `monk-service`, which dispatch to `brew`/`apt`/`systemd-unit`.
- Clean up `service-mgr`: an unknown PID 1 comm (`bwrap`, `tini`, `sh`) becomes `unknown`.

**11. Rolling change: `serial`, `throttle`, and run-level thresholds.** Monk's rule that a failure never stops the pass is right for one machine. It is wrong for fleet-wide changes, such as a bad sshd config on every host at once. Recommendation:
- Add `:serial (1 "25%" "100%")` on an orchestrate group, with batches as in `_get_serialized_batches`.
- Add `:throttle N` on a resource for "at most N machines do this at once".
- Add `:max-fail-percentage P` per batch as a circuit breaker. Do not copy the cumulative quirk. Count failures in the current batch only.
- Add `:any-errors-fatal` on a resource: if it fails anywhere, stop starting new resources on all machines in that batch. This is the only place Monk needs to leave the Salt model, and it must be opt-in.

**12. Selectors: tags and `--limit`.** Monk has `:only` with a dependents closure and CELL queries. Recommendation:
- Add `:tags` on resources, plus `monk-apply :tags '(ssh) :skip-tags '(slow)`.
- Copy `Taggable.evaluate_tags`: `always` runs unless skipped by name, `never` runs only when named, and `tagged`/`untagged`/`all` are reserved.
- Feed the tag selection into `:only`, so requisites still pull in what they need.
- `--limit` becomes a CELL query. Copy `@file` limits: a file of machine names.

**13. `rescued` and `ignored` in the ledger.** Monk's `:onfail` is the rescue. When an `:onfail` resource succeeds, count the original failure as `rescued`, as Ansible does. Leave out `ignore_errors`; see section 6.

**14. Listing modes.** Map `--list-hosts`, `--list-tasks`, `--list-tags` and `--syntax-check` to Monk commands that compile the policy with no connection. `monk-plan` covers most of this. A cheap "compile only" command catches bad CELL forms and bad slots in CI.

**15. `--start-at-task` / `--step`.** `--start-at-task` matches by `fnmatch` on the task name (`play_iterator.py`). Monk can offer it in the UI as "run from here". `--step` prompts `(N)o/(y)es/(c)ontinue` for each task. In Monk this must be a UI pause before each repair. Per your testdrive rule, it must never be a prompt inside a wait loop. Low priority.

**16. `ansible-pull` ideas for `monk-agent`.**
- `-o/--only-if-changed`: skip the run when the VCS update changed nothing.
- `-s/--sleep`: random delay against thundering herds.
- Per-host policy file selection: `<fqdn>.yml`, then `<short>.yml`, then `local.yml` (`PullCLI.select_playbook`).
- Limit to local aliases (`localhost,<fqdn>,<node>,<short>,127.0.0.1`).

**17. Computed groups (the `constructed` plugin) instead of more CELL.** `constructed` has `compose` (computed vars), `groups` (name to condition) and `keyed_groups` (group per value of a key, with `prefix`, `separator`, `parent_group`, `default_value`, `trailing_separator`). CELL already evaluates conditions. Add inventory forms:
- `(monk-group 'debian :when (= (fact os-family) "Debian"))`
- `(monk-keyed-groups :key '(fact distribution) :prefix "distro")`

Groups then become facts-derived and cached. That is the point of `constructed`: groups from the fact cache without a connection.

**18. Cross-host reads (`hostvars`).** Monk has refs across machines for requisites. It lacks reads such as `hostvars['db1'].ansible_default_ipv4.address`. Add `(machine-fact NAME KEY)` and `(machine-var NAME KEY)` to CELL. Evaluate them from the fact cache, so a guard never opens a connection.

**Skip or leave at low priority:**
- `ansible-console`: an `ielm`-style REPL over CELL-selected machines is cute but low value. `M-x monk-run-resource` on a query does the same job.
- `run_once`: a resource on one machine that others `:require` covers it.
- `vault`: use auth-source (section 6).

---

## 3. Ways to call Ansible from Monk

### 3.1 Fix the current ad hoc bridge first

**What `-o` prints** (`plugins/callback/oneline.py`):
```
HOST | CHANGED => {"changed": true,...}          # v2_runner_on_ok, changed
HOST | SUCCESS => {...}                          # v2_runner_on_ok, not changed
HOST | FAILED! => {...}                          # v2_runner_on_failed
HOST | UNREACHABLE!: <msg text, not JSON>        # v2_runner_on_unreachable
HOST | SKIPPED                                   # v2_runner_on_skipped
HOST | CHANGED | rc=0 | (stdout) ... (stderr) ...   # command/shell/raw (C.MODULE_NO_JSON)
```

Problems for `monk-ansible`:
- **Unreachable** uses `!:` and plain text. The regexp matches the status, then JSON parsing fails, so `msg` is lost.
- **`command`/`shell`/`raw`** print no JSON at all.
- **Diff** is removed at verbosity below 3, and `oneline` has no `v2_on_file_diff`. `--check --diff` gives no diff.
- **Exceptions.** `FAILED!` with an exception prints an extra line first: "An exception occurred during task execution. ...".
- **Removal.** The flag and the callback are deprecated for removal in 2.23.
- **JSON newlines.** The JSON uses `indent=0` with `'\n'` removed. It stays valid because JSON escapes newlines inside strings.

**Arguments.** `-a` accepts JSON. `AdHocCLI._play_ds` parses JSON when the string starts with `{` and ends with `}`. Send `(json-encode args)` instead of `key=value` pairs. That removes `parse_kv` quoting trouble and keeps booleans, ints and lists typed.

### 3.2 Recommended: a small Monk callback plugin that writes JSON lines

Ship `monk/etc/ansible/callback/monk_events.py`. It works for `ansible` and for `ansible-playbook`, and it does not depend on `ansible.posix`. It writes to a file named by an env var, so stderr warnings never mix into the data. `monk-exec` merges stderr today. Sketch against the API in this tree:

```python
from __future__ import annotations
import json, os, time
from ansible.plugins.callback import CallbackBase
from ansible.vars.clean import strip_internal_keys

class CallbackModule(CallbackBase):
    CALLBACK_VERSION = 2.0
    CALLBACK_TYPE = 'stdout'
    CALLBACK_NAME = 'monk_events'

    def __init__(self, *a, **kw):
        super().__init__(*a, **kw)
        self._out = open(os.environ['MONK_ANSIBLE_EVENTS'], 'a', buffering=1)

    def _emit(self, kind, result=None, **extra):
        rec = dict(kind=kind, time=time.time(), **extra)
        if result is not None:
            r = strip_internal_keys(dict(result.result))   # already masked for no_log/secrets
            rec.update(host=result.host.get_name(), task=result.task_name,
                       action=result.task_fields.get('action'),
                       delegated=result.result.get('_ansible_delegated_vars', {}).get('ansible_delegated_host'),
                       item=result.result.get('_ansible_item_label'), result=r)
        self._out.write(json.dumps(rec, default=str) + '\n')

    def v2_runner_on_ok(self, result):          self._emit('changed' if result.is_changed() else 'ok', result)
    def v2_runner_on_failed(self, result, ignore_errors=False): self._emit('failed', result, ignored=ignore_errors)
    def v2_runner_on_skipped(self, result):     self._emit('skipped', result)
    def v2_runner_on_unreachable(self, result): self._emit('unreachable', result)
    def v2_runner_item_on_ok(self, result):     self._emit('item-changed' if result.is_changed() else 'item-ok', result)
    def v2_runner_item_on_failed(self, result): self._emit('item-failed', result)
    def v2_runner_item_on_skipped(self, result):self._emit('item-skipped', result)
    def v2_runner_retry(self, result):          self._emit('retry', result)
    def v2_playbook_on_task_start(self, task, is_conditional): self._emit('task-start', name=task.get_name())
    def v2_playbook_on_stats(self, stats):
        self._emit('stats', hosts={h: stats.summarize(h) for h in sorted(stats.processed)})
```

**Call it** with a fixed environment:
```
ANSIBLE_CALLBACK_PLUGINS=<monk>/etc/ansible/callback
ANSIBLE_STDOUT_CALLBACK=monk_events
ANSIBLE_LOAD_CALLBACK_PLUGINS=1     # required for ad hoc to honor stdout_callback
MONK_ANSIBLE_EVENTS=/tmp/monk-ansible-XXXX.jsonl
ANSIBLE_NOCOLOR=1 ANSIBLE_DEVEL_WARNING=0 ANSIBLE_DEPRECATION_WARNINGS=0 ANSIBLE_LOCALHOST_WARNING=0
```

`ANSIBLE_LOAD_CALLBACK_PLUGINS=1` is required. `adhoc.py` uses `DEFAULT_STDOUT_CALLBACK` only when `C.DEFAULT_LOAD_CALLBACK_PLUGINS and C.DEFAULT_STDOUT_CALLBACK != 'default'`. Otherwise ad hoc uses `minimal`.

Keep `diff` out of the `_dump_results` path. The sketch reads `result.result` directly, so `diff` survives. Full test: `ansible localhost -c local -m lineinfile -a '{"path":"/tmp/x","line":"a"}' --check --diff`, then read the JSONL file.

**Fallback without a plugin:** `minimal` with `ANSIBLE_CALLBACK_FORMAT_PRETTY=false`. `_dump_results` sets `indent=None` when `pretty_results is False`, so each result is `HOST | STATUS => {one-line json}`. The diff still prints separately as text through `v2_on_file_diff`. `command`/`shell` still use the multi-line `rc=N >>` form. Treat this only as a stopgap.

### 3.3 Facts

**Through Ansible:**
```
ansible HOST -i HOST, -m setup -a '{"gather_subset":["min"]}'
```
Use it with the callback above. The result has `result.ansible_facts.ansible_os_family`, and the keys keep the `ansible_` prefix there. In playbooks the same value is `ansible_facts.os_family`, with the prefix removed. Map both names.

**Without the controller:** see 3.6. `python3 -m ansible.modules.setup` with `{"ANSIBLE_MODULE_ARGS": {"gather_subset": ["min"], "filter": ["ansible_*"]}}` took 0.5 s here. `filter` is an fnmatch list on the first-level keys.

`--tree DIR` writes one JSON file per host (`tree.py`, `write_tree_file`). It is deprecated for removal in 2.23. Do not build on it.

### 3.4 Inventory import and export: `ansible-inventory`

This comes from `cli/inventory.py`:
- `ansible-inventory -i INV --list [--export] [--limit PAT] [--output FILE]` prints JSON, `sort_keys=True`, `indent=4`, with `_inventory_legacy.Encoder`:
  ```json
  {"all": {"children": ["ungrouped", "web"]},
   "web": {"hosts": ["h1"], "vars": {...only with --export...}, "children": [...]},
   "_meta": {"hostvars": {"h1": {...}}, "profile": "inventory_legacy"}}
  ```
  - Empty `hosts`, `vars` and `children` keys are removed. `all` never has `hosts`.
  - Without `--export`, `hostvars` holds fully flattened vars (`vm.get_vars(host, include_hostvars=False, stage='all')`), with group vars merged into every host. Use `--export` for import, so group vars stay on groups. `ansible_group_priority` appears when it is not 1.
  - Vaulted values serialize as `{"__ansible_vault": "<ciphertext>"}`. Untrusted strings serialize as `{"__ansible_unsafe": "..."}` (`_json/_profiles/_legacy.py`). The Monk importer must unwrap both.
- `--host NAME` prints one host's flattened vars. It errors unless exactly one host matches.
- `--graph [GROUP] [--vars]` prints text: `@all:`, then `  |--@web:`, then `  |  |--h1`.
- `--yaml` and `--toml` are available.
- Exit code is 0 when there was output, else 1 (`sys.exit(1)`).

### 3.5 Run the existing site.yml during the migration

```
ANSIBLE_...(env above) ansible-playbook -i INV site.yml --check --diff [--limit HOST] [--tags T]
```
Read the JSONL file. Each `changed` event in check mode is Ansible's "would change".

The high-value tool is a **parity report**. Run Ansible `--check --diff` and `monk-plan` on the same machine, then list the drift each one sees and the other misses. That shows when the Monk port is complete.

Exit codes (`errors/__init__.py` `ExitCode`, `cli/__init__.py` `cli_executor`):
- 0 success
- 1 generic error
- 2 host failed
- 4 host unreachable. `PARSER_ERROR` is also 4; the source marks the clash `FIXME`.
- 5 invalid CLI option. An import failure before `Display` exists also gives 5.
- 99 keyboard interrupt
- 250 unknown error

`RUN_FAILED_BREAK_PLAY` (8) becomes 2 before the CLI returns.

### 3.6 Run modules directly, without the controller

This is the most useful path for Monk. It works on any target with Python 3.9 or later (`basic.py` `_PY_MIN = (3, 9)`). It needs no controller Python 3.13, no inventory, no tmpdir handling and no CLI startup. **Tested:**

```
$ echo '{"ANSIBLE_MODULE_ARGS": {"path": "/tmp/mt/zz", "line": "b=2", "create": true,
         "_ansible_check_mode": true, "_ansible_diff": true}}' \
  | PYTHONPATH=/tmp/mt/payload.zip python3 -m ansible.modules.lineinfile

{"changed": true, "msg": "line added", "backup": "", "diff": {"before": "", "after": "b=2\n", ...}}
rc=0
```

- **Payload.** `payload.zip` holds `ansible/__init__.py`, `ansible/module_utils/**` and the wanted `ansible/modules/*.py`. It is 700 KB and loads through zipimport.
- **Speed.** Runtime is 0.1 s for `lineinfile` and 0.5 s for `setup`, against 1 to 3 s for the CLI.
- **Input.** The module reads stdin. It also accepts `argv[1]` as a JSON file path or an inline JSON string, plus `--profile NAME` (default `legacy`) (`module_utils/_internal/_debugging.py` `load_params`).
- **Output.** Stdout is `"\n" + one JSON object`. Exit is 0 on success and 1 on failure.
- **Normalization.** Monk must copy the controller rules itself:
  - `rc != 0` with no `failed` key means failed.
  - A missing `changed` means false.
  - Strip `_ansible_new_secrets`, and use its values to mask the rest of the result.
  - `skipped: true` in check mode means "cannot dry-run".
  - `warnings` holds `{"event": {"msg": ...}, "__ansible_type": "WarningSummary"}` objects.
  - `diff` can be a dict, a list, or dict-valued before/after.
- **What does not work this way:** modules that are only action plugins, or that depend on one.
  - `template` renders in the controller.
  - `copy` with a local `src` expects the file already on the target plus a `checksum`.
  - `fetch` and `raw` are virtual.
  - `package` and `service` dispatch on facts in the action plugin; call `apt`, `systemd` and the others directly.
  - `command` is fine. Its action plugin only forwards to `ansible.legacy.command`.
- **Over TRAMP:**
  1. Copy the zip once to `~/.cache/monk/ansible-modlib-<sha>.zip` on the target, with a checksum guard.
  2. Run `python3 -m ansible.modules.X` with `PYTHONPATH` set and the args JSON on stdin.

  Not verified here: whether `process-file` with TRAMP copies a local INFILE for you. A `make-process` with `:file-handler t`, then `process-send-string` and `process-send-eof`, avoids that question.
- **AnsiballZ `explode`/`execute`** is a debugging aid only. The params are baked into each payload, so a payload cannot be reused. Build Monk's own zip.

### 3.7 Generate Monk wrappers with `ansible-doc -j`

`cli/doc.py`:
- `ansible-doc -t module -j lineinfile` prints `{"lineinfile": {"doc": {...}, "examples": "...", "return": {...}, "metadata": ...}}`.
- `doc` holds `options.<name>.{description, type, required, default, choices, aliases, elements, suboptions}`, `attributes.check_mode.support` (`full`/`partial`/`none`), and `has_action` (True when an action plugin exists).
- `-l -j` lists modules with short descriptions. `-F` lists files. `--metadata-dump` is for internal use only.

Use this to generate `(monk-ansible-lineinfile :path ... :line ...)` with validated keywords, docstrings, and a `:check-mode` flag from `attributes.check_mode.support`. Only generate wrappers where `has_action` is false, since section 3.6 covers only those modules.

### 3.8 Other entry points

- **`ansible-config dump --only-changed -f json`** (`cli/config.py` `execute_dump`) records the effective config next to a run. I did not check the exact JSON shape.
- **Python API** (`TaskQueueManager`, `PlaybookExecutor`): **do not use it.**
  - It needs a Python 3.13 controller.
  - The result objects in this tree are brand new and marked unstable. `UnifiedTaskResult` carries many `RPFIX-*` markers, and `CallbackTaskResult._result` is deprecated.
  - Monk would need a long-lived Python process.
- **`ansible-runner`** is not in this tree, so I did not check it. It wraps the same CLI and callbacks. The section 3.2 plugin gives Monk the same events with less machinery.

### 3.9 Gotchas

- **Color.** `ANSIBLE_NOCOLOR` or `NO_COLOR` turns it off. `ANSIBLE_FORCE_COLOR` overrides both.
- **Host keys.** `ANSIBLE_HOST_KEY_CHECKING` defaults to True.
- **Extra vars.** `-e` treats a leading `{` or `[` as YAML/JSON, `@file` as a file, and anything else as k=v. A value that starts with `/` or `.` without `@` is an error (`load_extra_vars`).
- **Host lists.** `-i host,` needs the trailing comma. The `host_list` plugin needs a path that does not exist and contains `,`.
- **Implicit localhost.** Without `-i`, `localhost` uses implicit settings: `ansible_connection=local` and the controller's Python. It does not match `all` (`_create_implicit_localhost`).
- **Interpreter.** `INTERPRETER_PYTHON` defaults to `auto`, which prints discovery warnings. Set `ANSIBLE_PYTHON_INTERPRETER` or `ansible_python_interpreter`.
- **Development builds.** `ANSIBLE_DEVEL_WARNING=0` silences the `dev0` warning.
- **`msg` on failure.** `msg` is set by `fail_json`. Action errors fill `msg` from the exception chain (`_create_from_exception`). A parse failure has `msg` "Module result deserialization failed." with `module_stdout`/`module_stderr`.
- **Output streams.** Warnings and deprecations go to stderr (`Display.warning`, `stderr=True`).
- **`ansible-pull`'s own check** for `--only-if-changed` is a grep for `b'"changed": true'`, `b"changed: True"` or `b"| CHANGED =>"` in the ad hoc output (`cli/pull.py`). Do not copy it.

---

## 4. Interop the other way

**Monk as an inventory source: worth doing, cheap.** Two forms:
- **Static file.** Write `inventory.json` in YAML-plugin format. That plugin's `yaml_extensions` default includes `.json`:
  ```json
  {"all": {"children": {"darwin": {"hosts": {"mac1": {"ansible_host": "..."}}, "vars": {...}}}}}
  ```
  It needs no executable and starts with no delay. Regenerate it when the inventory changes.
- **Live script.** An executable that answers `--list` with `{GROUP: {"hosts": [...], "vars": {...}, "children": [...]}, "_meta": {"hostvars": {...}}}`. Always include `_meta.hostvars`. Without it, `plugins/inventory/script.py` warns and runs `--host` once per host. `_meta.profile`, if present, must start with `inventory_`. `verify_file` requires `os.access(path, os.X_OK)`. `emacs --batch` startup is fine for this.

With either form, Ansible ad hoc commands and old playbooks keep working against Monk's inventory during the migration.

**Monk facts as `facts.d`: worth doing only while Ansible still runs on a machine.** A Monk resource writes `/etc/ansible/facts.d/monk.fact`:
```json
{"classes": [...], "groups": [...], "role": "..."}
```
Playbooks then read `ansible_local.monk.classes`. Verified above with a real `setup` run. It is small and one-way.

**Monk as a callback target: yes. It is the same artifact as section 3.2.** The callback shape converts to Monk's event record:

| Callback | Monk result |
|---|---|
| `ok` + `is_changed()` (normal run) | `repaired`, degree 1 |
| `ok` + `is_changed()` (`--check`) | `would-repair`, degree 0 |
| `ok`, not changed | `kept`, degree 1 |
| `failed` | `failed`, degree 0. Keep `ignored` and `rc` in `changes` |
| `skipped` | `skipped`, comment from `skip_reason` / `false_condition` / `msg` |
| `unreachable` | `unreachable` (new, see section 2) |
| `result.msg` | comment |
| `result.diff` | diff list |
| remaining keys | `changes` plist, with `stdout_lines`, `stderr_lines` and `invocation` dropped |
| task name, `action` | resource name, resource type `ansible:<action>` |
| loop `item-*` events with `_ansible_item_label` | one event per item |

What does not map: Ansible has no degree and no partial status. It also has no per-attribute kept-versus-repaired, and handlers appear as ordinary tasks. Write to the JSONL file, then have Monk import it into the ledger and sqlite. Posting straight to the agent socket from Python also works. The file is simpler, and a crash cannot lose events.

**Monk resources as an Ansible module: no.**
- A module is a single-shot JSON-in/JSON-out process per task. Monk's value is the graph: multi-pass convergence, requisites across machines, flap detection, degree. A module exposes none of that.
- It would need Emacs on every target and an Emacs startup per task.
- The one exception is a bootstrap `monk_apply` module that runs a whole Monk policy with `emacs --batch` and returns the summary as `changed`/`failed`. That is at most a transition tool.

**Monk history as a `fact_caching` backend: no. Read Ansible's cache instead.**
- A cache plugin implements `get/set/keys/contains/delete/flush`. Core wraps it in `PluginInterposer`: keys become `s1_<host>` and values become `{"__payload__": "<json string>"}` in the `cache_persistence` profile, which carries type tags. Tying Monk's sqlite schema to that is poor value.
- The reverse direction costs little. Point Monk at `fact_caching_connection`, read `s1_<host>` files, and `json-parse-string` the `__payload__`. Monk then gets Ansible-gathered facts for free during the migration. I did not check the full tag vocabulary of `cache_persistence`. Test it on a real cache dir.

---

## 5. Mapping table

### 5.1 Concepts

| Ansible | Monk | Note |
|---|---|---|
| playbook / play / block | policy function / machine selector / `progn` of resources | Monk has no play boundary. Keep it that way. |
| task `name` | resource name | Monk names must be unique. Ansible names need not be. |
| module / FQCN | resource type (`monk-file`, ...) or `monk-ansible` | |
| `args` / `argument_spec` | constructor keywords / slots | Add spec validation (section 2, item 4). |
| `--check` | `:dry-run` / `monk-plan` | Ansible: `changed` in check mode. Monk: `would-repair`. |
| `--diff` | `diff` in result | Keep `before/after/*_header`. |
| ok / changed | kept / repaired | |
| changed in check mode | would-repair | |
| skipped | skipped | Ansible gives `skip_reason`, `false_condition`. Monk gives the CELL explain. |
| failed | failed / partial | Ansible has no partial. |
| unreachable (`dark`) | **missing** | Add `unreachable`. |
| rescued / ignored | missing | `rescued` = failure with a successful `:onfail`. |
| `when` | CELL `:when` | Table 5.3. |
| `changed_when` / `failed_when` | missing | Add as predicates. |
| `until/retries/delay` | missing (Monk has multi-pass) | Add, within a pass. |
| `register` + `when: r.changed` | `:onchanges` | |
| `register` + `when: r.failed` | `:onfail` | |
| `register` + later use of `r.stdout` | missing | Add `(result REF KEY)`. |
| `notify` / handlers / `listen` | `:watch` | In the graph, not at play end. |
| `flush_handlers` | not needed | `:watch` fires in the same pass. |
| `ignore_errors` | Salt model (default) | Do not add a flag. |
| `block/rescue/always` | `:onfail` + unconditional resource | |
| `any_errors_fatal` | missing | Add opt-in, per resource. |
| `max_fail_percentage` | missing | Add per batch. |
| `serial` / `throttle` | missing | Add for orchestrate. |
| single pass + handlers | multi-pass until converged | Monk is stronger here. |
| `--limit`, host patterns (`:`, `&`, `!`, `~regex`, `@file`) | CELL query / `:only` | |
| `tags` / `--skip-tags` | **missing** | Add as selectors (section 2, item 12). |
| `delegate_to` / `run_once` | orchestrate refs / one resource + `:require` | |
| `hostvars[h].x` | missing | Add `(machine-fact h k)`. |
| `group_vars/`, `host_vars/` | inventory vars | |
| `ansible_group_priority` + group depth | explicit order in inventory | Do not copy the implicit depth rule. |
| `set_fact` | Lisp `let` at policy time | |
| facts, `gather_facts`, `gathering=smart` | fact probes + sqlite TTL | Add TTL. |
| `facts.d` → `ansible_local` | missing | Add `local` probe. |
| `constructed` inventory | missing | Add computed/keyed groups. |
| `ansible-pull` | `monk-agent` | Copy only-if-changed and sleep jitter. |
| `ansible-console` | `M-x` on a CELL query | |
| vault | **missing** | Use auth-source (section 6). |
| `ansible-doc` | generated docs from specs | |
| callback plugins | event record + agent socket | |
| `AggregateStats` / exit codes | run summary | Add bit-safe exit codes. |
| become | TRAMP `sudo` hop / `:sudo` | |
| ControlPersist | TRAMP `tramp-use-connection-share` / ssh ControlMaster | |

### 5.2 Facts

"setup" means the `ansible_facts` key as returned by the module. "Playbook" means the modern playbook name.

| setup | Playbook | Monk | Difference |
|---|---|---|---|
| `ansible_system` | `ansible_facts.system` | `kernel` | Same source (`platform.system()` vs `uname -s`). |
| `ansible_os_family` | `ansible_facts.os_family` | `os-family` | Monk knows only Debian and RedHat. Otherwise it capitalizes `ID`. Ansible uses `OS_FAMILY_MAP`: Arch is `Archlinux`, and there are Suse, Alpine, Gentoo entries. |
| `ansible_distribution` | `.distribution` | `distribution` | macOS: Ansible says **`MacOSX`**, Monk says `MacOS`. Arch: `Archlinux` vs `Arch`. openSUSE naming differs. Pick one set on purpose. |
| `ansible_distribution_release` | `.distribution_release` | `distribution-release` | Linux: codename (`jammy`) in both. macOS: Ansible gives `platform.release()` (Darwin kernel version). Monk gives nothing. |
| `ansible_distribution_version` | `.distribution_version` | `distribution-version` | Same on Linux (`22.04`) and macOS (`sw_vers`). |
| `ansible_distribution_major_version` | `.distribution_major_version` | missing | Add it. site.yml often compares it. |
| `ansible_architecture` | `.architecture` | `arch` | Both come from `uname -m`/`platform.machine()`: `arm64` on macOS, `aarch64` on Linux. |
| `ansible_hostname` | `.hostname` | `hostname` | Same rule (node name up to the first dot). |
| `ansible_nodename` | `.nodename` | `nodename` | Same. |
| `ansible_fqdn` | `.fqdn` | missing | `socket.getfqdn()`. |
| `ansible_env.HOME` | `.env.HOME` | `home` | Also `ansible_user_dir` (from pwd). Prefer that over `$HOME`. |
| `ansible_user_id` | `.user_id` | `user` | Ansible uses `getpass.getuser()`, which reads `LOGNAME`/`USER` first. Monk uses `id -un` (effective uid). They differ under sudo. |
| `ansible_pkg_mgr` | `.pkg_mgr` | missing | Add (`homebrew`, `apt`, ...). |
| `ansible_service_mgr` | `.service_mgr` | missing | Add, and clean up unknown PID 1 names. |
| `ansible_local.X` | `ansible_local.X` | missing | Add `(fact local)`. |

### 5.3 Common `when` idioms in CELL

| Ansible `when` | CELL | CEL string |
|---|---|---|
| `ansible_os_family == "Darwin"` / `ansible_facts['os_family'] == 'Darwin'` | `(= (fact os-family) "Darwin")` | `facts['os-family'] == 'Darwin'` |
| `'web' in group_names` | `(group web)` | `'web' in groups` |
| `inventory_hostname in groups['web']` | `(group web)` | same |
| `foo is defined` | `(has (var foo))` | `has(vars.foo)` |
| `foo is not defined` | `(not (has (var foo)))` | `!has(vars.foo)` |
| `foo \| default(false)` / `foo is defined and foo` | `(var foo)` | `vars.foo` (nil when absent) |
| `not foo` | `(not (var foo))` | `!vars.foo` |
| `ansible_distribution in ['Ubuntu', 'Debian']` | `(in (fact distribution) (list "Ubuntu" "Debian"))` | `facts.distribution in ['Ubuntu','Debian']` |
| `ansible_architecture == 'arm64'` | `(= (fact arch) "arm64")` | |
| `ansible_hostname.startswith('mac')` | `(starts-with (fact hostname) "mac")` | `facts.hostname.startsWith('mac')` |
| `a and (b or c)` (list form means AND) | `(and A (or B C))` | |
| `ansible_distribution_major_version \| int >= 22` | needs `(>= (int (fact distribution-major-version)) 22)` | CELL lacks string→number. Add `int`, or store the fact as a number. |
| `ansible_distribution_version is version('22.04', '>=')` | needs a `version>=` op | Add; `version-to-list` exists in Emacs core. |
| `hostvars['db1'].x == 'y'` | needs `(machine-var db1 x)` | Section 2, item 18. |
| `r.changed` / `r is changed` | `:onchanges REF` | Not a guard. It is a requisite. |
| `r.rc != 0` / `r is failed` | `:onfail REF` | Same. |
| `r.stdout.find('x') != -1` | needs `(contains (result REF :stdout) "x")` | Add `result` accessor. |

YAML truthiness is a trap during translation. `foo: "false"` (a string) is true in Jinja. `foo: no` is a YAML bool. Import vars with explicit types and flag string-typed booleans.

---

## 6. Gotchas and things not to copy

**Jinja in YAML, and string conditions.** `when` is a string compiled at run time. Errors show up late. Type coercion is implicit (`yes`/`no`/`"False"`). CELL as data is better: it is checkable at load time, explainable, and diffable. Keep CEL as the string syntax only.

**The variable precedence ladder.** Section 1.11 has 12 or more layers. `VARIABLE_PRECEDENCE` makes the group part configurable. Group order uses depth, then priority, then name. `hash_behaviour=merge` changes everything at once. People debug this with `-vvv` for hours. Monk needs defaults < group vars in explicit order < host vars < run overrides, and nothing more. Use no implicit depth rule, and let the order of groups in the inventory decide.

**Handlers as named strings at play end.**
- They run only at `flush_handlers`.
- The last definition with a given name wins silently (reverse search).
- A failure between notify and flush loses the handler unless `force_handlers` is set.
- A missing name is only a warning by default.

Monk's `:watch` in the graph, with multi-pass, is strictly better. Split "refresh owed" from "state wrong" as the mgmt review suggests. Do not add a handler concept.

**`ignore_errors`.** It turns a failure into `ok` + `ignored` and hides it from `failures`. Monk's model already continues and shows the failure. Offer `:failed-when (lambda (_) nil)` for the rare real case, so the intent is visible.

**Linear lockstep and per-task Python startup.** Every task on every host pays for AnsiballZ build, transfer and interpreter start, plus a tmpdir unless pipelining is on (default off). Monk's TRAMP-native resources avoid this. Use Ansible modules (section 3.6) only for gaps.

**The tmpdir permission chain.** `_fixup_perms2` ends with world-readable tmpfiles when become goes to an unprivileged user. Monk with TRAMP `sudo` hops does not need a shared tmpdir. Keep it that way.

**Facts injected as top-level vars.** They are deprecated (2.24). Do not flatten facts into vars in Monk. Keep `(fact k)` and `(var k)` separate, as CELL does now.

**Deprecated output: `-o`, `--tree`, `oneline`, `tree`.** All are deprecated for removal in 2.23. Do not build new parsing on them. Migrate `monk-ansible` to the section 3.2 callback, or to direct module runs (section 3.6).

**"Skipped" as the no-check-mode signal.** This is changing (2.25 deprecation, `AnsibleActionNoCheckMode` → `changed=False`). Decide check-mode support from `ansible-doc`'s `attributes.check_mode.support`, not from the result. For `none`, Monk reports `would-repair: unknown` and never `kept`. The current bridge treats `skipped` as "change on every pass", which stops convergence. `command` in check mode also returns `skipped: true` unless `creates`/`removes` apply.

**Loops with nested `results`.** A looped task produces one result with a `results` list. `changed` means any item changed and `skipped` means all items skipped, so per-item history is lost. Monk expands loops at policy time with `dolist`. Each item then becomes its own resource with its own name, history, requisites and diff. Keep that.

**`register` as the only data channel.** Implicit coupling by variable name breaks when tasks move. Monk refs are explicit. Add `(result REF KEY)` on refs, not global result variables.

**Retry files.** `RETRY_FILES_ENABLED` (default False) writes `<playbook>.retry` for `--limit @file`. Monk's sqlite history can answer "machines that failed last run" as a query. No files are needed.

**The Python API.** It needs a 3.13 controller and its internals move. See section 3.8.

**Vault.** The format is `$ANSIBLE_VAULT;1.1;AES256` (or `1.2;AES256;<vault-id>`): PBKDF2-HMAC-SHA256 with 10000 iterations, AES-256-CTR, HMAC-SHA256 (`parsing/vault/__init__.py`, `VaultAES256`). Do not reimplement it in Emacs. Use auth-source (`.authinfo.gpg`, keychain, pass) for secrets, keyed by `(:host machine :user purpose)`. For migration, run `ansible-vault view FILE` once and move the values into auth-source. Remember the auth-source cache lesson from your notes: bind `auth-source-do-cache` to nil for lookups that must be fresh.

**Fact quirks to clean up, not copy:**
- `service_mgr` reports the raw PID 1 name (`bwrap` here).
- `user_id` follows `LOGNAME` instead of the effective uid.
- macOS `distribution_release` is the Darwin kernel version.
- `distribution` is `MacOSX`.

If site.yml compares against these values, write the translation table once, in one file.

**`max_fail_percentage` counting across batches.** The code divides the cumulative `_tqm._failed_hosts` by the current `batch_size`. Count per batch in Monk.

**The `ansible-pull` change check.** `--only-if-changed` greps text for `"changed": true`. Monk's agent must decide from the VCS state: compare the commit before and after, or check that `vc-git` reported a change.
