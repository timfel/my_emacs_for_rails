---
name: emacs
description: Access and control a running GNU Emacs instance via `emacsclient`. Use when an agent needs to inspect Emacs state, evaluate Elisp, visit files or locations, or trigger editor-side actions inside the user's live Emacs session instead of only editing files on disk. On Windows, run `emacsclient` from PowerShell so the PowerShell function can resolve Emacs correctly. On Linux, run `emacsclient` from the default shell via `PATH`.
---

# Emacs

Use this skill when work must happen inside the live Emacs session instead of only on-disk files.

## Rules

- On Windows, run `emacsclient` from PowerShell.
- On Linux, run `emacsclient` from the default shell.
- Start with read-only inspection before running state-changing Elisp.
- Prefer `emacsclient --eval` for scripted actions; reserve opening files/frames for cases where visual state matters.
- Before calling local helper functions that are not commands, either `(require (quote owning-library))` or verify `(fboundp (quote helper))` and fail clearly.
- Keep inline `--eval` forms short and literal. For portability between POSIX shells and PowerShell, wrap the Elisp form in single quotes and avoid Elisp quote shorthand inside it: use `(quote symbol)` instead of `'symbol`, and `(function fn)` instead of `#'fn`.
- Do not encode text as `(string 97 94 102 ...)` just to avoid shell quoting. Put complex Elisp or user-provided text in a temporary file and have Emacs read it.

## Common Commands

- Verify the server: `emacsclient --eval '(emacs-version)'`
- Inspect the current buffer: `emacsclient --eval '(buffer-name (window-buffer (selected-window)))'`
- List buffers: `emacsclient --eval '(mapcar (function buffer-name) (buffer-list))'`
- Open a file in an existing frame: `emacsclient path/to/file`
- Jump to a location in an existing frame: `emacsclient +LINE:COLUMN path/to/file`

## Quoting and Payloads

Use inline `--eval` only for forms that contain no dynamic user text and no shell-sensitive quoting.

When running from PowerShell, prefer this pattern for complex Elisp:

```powershell
$elisp = @'
(progn
  (require (quote timfel-jira-extensions))
  (timfel/jira-periodic-python-issues-alist 30))
'@
$tmp = Join-Path $env:TEMP "codex-emacs-$PID.el"
Set-Content -LiteralPath $tmp -Encoding UTF8 -Value $elisp
$env:CODEX_EMACS_ELISP = $tmp
emacsclient --eval '(load-file (substitute-in-file-name "$CODEX_EMACS_ELISP"))'
```

For prompts, paths, JSON, or any other user-provided payload, write the payload to a second temp file and read it from Elisp:

```elisp
(let ((prompt-file (substitute-in-file-name "$CODEX_EMACS_PROMPT_FILE")))
  (with-temp-buffer
    (insert-file-contents prompt-file)
    (buffer-string)))
```

This keeps shell quoting out of the payload and leaves readable Elisp in logs.

## Agent Shell

Use this flow when the user wants Codex to inspect or drive `agent-shell` sessions from the live Emacs instance.

- `agent-shell` resolves buffers by current project, so set `default-directory` or `with-current-buffer` deliberately before calling project-sensitive functions.
- Discover installed entry points first: evaluate `locate-library`, `fboundp`, `documentation`, or small buffer lists before sending prompts.
- Prefer public commands such as `agent-shell`, `agent-shell-start`, `agent-shell-new-worktree-shell`, `agent-shell-buffers`, `agent-shell-project-buffers`, `agent-shell-insert`, and `agent-shell-queue-request`.
- When targeting a specific shell, switch to its buffer with `with-current-buffer`; do not rely on the currently selected window.

### Agent Shell Inspection

- Find the library: `emacsclient --eval '(locate-library "agent-shell")'`
- Check availability: `emacsclient --eval '(list (fboundp (quote agent-shell)) (fboundp (quote agent-shell-queue-request)) (fboundp (quote agent-shell-new-worktree-shell)))'`
- List shell buffers and directories: `emacsclient --eval '(mapcar (lambda (buf) (with-current-buffer buf (list (buffer-name buf) default-directory))) (agent-shell-buffers))'`
- Read a docstring: `emacsclient --eval '(documentation (quote agent-shell-queue-request))'`

### Agent Shell Fan-Out

Use this when the user wants multiple parallel agent shells, especially one per git worktree.

- `timfel/agent-shell-fan-out-worktrees` is the preferred local helper for creating one worktree-backed shell per task, renaming each shell buffer to a title, and queueing each task automatically.
- `agent-shell-queue-request` only works inside an `agent-shell-mode` buffer. It submits immediately when idle and enqueues when busy.
- For reliable automation, first find the target shell buffer by matching its `default-directory`, then call `agent-shell-queue-request` inside that buffer.
- After loading `timfel-jira-extensions`, this pairs well with `timfel/jira-periodic-python-issues-alist`: fetch issue summaries, convert them into `(TITLE . TASK)` pairs, then fan them out into separate worktrees.

### Agent Shell Examples

- Use the local fan-out helper with titles and tasks:
  `emacsclient --eval '(timfel/agent-shell-fan-out-worktrees (list (cons "Task A" "Implement feature A") (cons "Task B" "Fix bug B")))'`
- Strings still work and are treated as `(TASK . TASK)`:
  `emacsclient --eval '(timfel/agent-shell-fan-out-worktrees (list "Task A" "Task B"))'`
- Combine Jira search with worktree fan-out, loading the non-command helper first:
  `emacsclient --eval '(progn (require (quote timfel-jira-extensions)) (timfel/agent-shell-fan-out-worktrees (mapcar (lambda (issue) (cons (car issue) (concat "Investigate and propose a fix for " (car issue) ": " (cdr issue)))) (timfel/jira-periodic-python-issues-alist 90))))'`
- Start a shell in a specific worktree:
  `emacsclient --eval '(let ((default-directory "/path/to/worktree/")) (agent-shell (list 4)))'`
- Start a shell programmatically with the preferred config:
  `emacsclient --eval '(let ((default-directory "/path/to/worktree/")) (agent-shell-start :config (agent-shell--resolve-preferred-config)))'`
- Queue work on the shell rooted at a directory:
  `emacsclient --eval '(let* ((root (file-name-as-directory "/path/to/worktree/")) (buf (seq-find (lambda (buf) (with-current-buffer buf (string= (file-name-as-directory default-directory) root))) (agent-shell-buffers)))) (unless buf (error "No agent-shell buffer for %s" root)) (with-current-buffer buf (agent-shell-queue-request "Implement task A, then summarize the diff.")))'`
- Fan out several requests at once:
  `emacsclient --eval '(dolist (spec (list (cons "/path/to/wt-a/" "Task A") (cons "/path/to/wt-b/" "Task B"))) (let* ((root (car spec)) (prompt (cdr spec)) (buf (seq-find (lambda (buf) (with-current-buffer buf (string= (file-name-as-directory default-directory) (file-name-as-directory root)))) (agent-shell-buffers)))) (unless buf (error "No agent-shell buffer for %s" root)) (with-current-buffer buf (agent-shell-queue-request prompt))))'`

## Jira and CI Helpers

Use this when the user wants quick Jira or CI info directly from their Emacs setup instead of reconstructing REST plumbing each time.

- `timfel/jira-periodic-python-issues-alist` returns `((KEY . SUMMARY) ...)` pairs.
   - It queries Jira for issues with label `periodic-job-failures`, component `Python`, status not in `Closed` or `In Progress`, and created within the last 90 days by default.
   - Pass a numeric argument to override the day window.
   - Load before calling directly: `emacsclient --eval '(progn (require (quote timfel-jira-extensions)) (timfel/jira-periodic-python-issues-alist))'`
   - Example with a 30-day window: `emacsclient --eval '(progn (require (quote timfel-jira-extensions)) (timfel/jira-periodic-python-issues-alist 30))'`
- The `timfel/jira` command is available to open a Jira issue buffer asynchronously. Once loaded, from that buffer you can query and inspect Jira issues
   - Run this command, then wait for a short while for the `*Jira Issues*` buffer to be populated
- The `ci-dashboard` command is available to open a CI buffer asynchronously. Once loaded, it allows navigation to all pull requests and CI statuses that the user has either created or is a reviewer of
   - Run this command, then wait for a short while for the `*ci-dashboard*` buffer to be populated

## Notes

- Keep Elisp forms short and explicit.
- If `emacsclient` cannot find the server, just inform the user that the Emacs server is not running and stop.
- Prefer commands that return simple strings, numbers, or small lists.
- For scripted edits or shell orchestration, prefer a single `-r --eval` form that validates preconditions and raises a clear error when a target buffer is missing.
