---
name: emacsclient-access
description: Access and control a running GNU Emacs instance via `emacsclient`. Use when an agent needs to inspect Emacs state, evaluate Elisp, visit files or locations, or trigger editor-side actions inside the user's live Emacs session instead of only editing files on disk. On Windows, run `emacsclient` from PowerShell so the PowerShell function can resolve Emacs correctly. On Linux, run `emacsclient` from the default shell via `PATH`.
---

# Emacsclient Access

Use this skill when work must happen inside the live Emacs session instead of only on-disk files.

## Rules

- On Windows, run `emacsclient` from PowerShell.
- On Linux, run `emacsclient` from the default shell.
- Prefer `-r` to reuse an existing frame.
- Only use `-c` when the user explicitly asks for a new frame.
- Start with read-only inspection before running state-changing Elisp.

## Common Commands

- Verify the server: `emacsclient -r --eval "(emacs-version)"`
- Inspect the current buffer: `emacsclient -r --eval "(buffer-name (window-buffer (selected-window)))"`
- List buffers: `emacsclient -r --eval "(mapcar #'buffer-name (buffer-list))"`
- Open a file in an existing frame: `emacsclient -r path/to/file`
- Jump to a location in an existing frame: `emacsclient -r +LINE:COLUMN path/to/file`

## Notes

- Keep Elisp forms short and explicit.
- If `emacsclient` cannot find the server, just inform the user that the Emacs server is not running.
- Prefer commands that return simple strings, numbers, or small lists.
