;;; hook-compile.el --- tolerant batch byte-compilation for git hooks -*- lexical-binding: t; -*-

;; Keep this helper focused on git-hook compilation.  It compiles each file
;; independently, removes any stale .elc on failure, and never exits non-zero
;; just because one file does not compile.

(require 'bytecomp)

(defun hook-compile--delete-elc (file)
  "Delete the compiled output for FILE if it exists."
  (let ((elc-file (byte-compile-dest-file file)))
    (when (file-exists-p elc-file)
      (delete-file elc-file))))

(defun hook-compile--compile-file (file)
  "Byte-compile FILE.
Return non-nil on success.  On failure, delete FILE's `.elc' and report it."
  (condition-case err
      (progn
        (byte-compile-file file)
        t)
    (error
     (hook-compile--delete-elc file)
     (princ (format "Hook compile skipped `%s': %s\n"
                    file
                    (error-message-string err)))
     nil)))

(defun hook-compile-batch ()
  "Batch-compile remaining command-line files for git hooks.
Compilation errors are reported, but this command always exits successfully."
  (let ((files command-line-args-left)
        (failed 0))
    (dolist (file files)
      (unless (hook-compile--compile-file file)
        (setq failed (1+ failed))))
    (when (> failed 0)
      (princ (format "Hook compile finished with %d failure(s); removed stale .elc files.\n"
                     failed)))
    (kill-emacs 0)))

(provide 'hook-compile)
;;; hook-compile.el ends here
