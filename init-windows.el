;;;;
;;;; cygwin support
;;;;
;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).
;;
(let* ((cygwin-root "c:/cygwin")
       (cygwin-bin (concat cygwin-root "/bin")))
  (when (and (eq 'windows-nt system-type)
	     (not (eq 'x window-system))
  	     (file-readable-p cygwin-root))

    (load (expand-file-name "~/.emacs.d/cygwin-mount.el"))
    (setq exec-path (cons cygwin-bin exec-path))
    (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
    (cygwin-mount-activate)
    
    ;; By default use the Windows HOME.
    ;; Otherwise, uncomment below to set a HOME
    (setenv "HOME" (concat cygwin-root "/home/tim"))
    
    ;; NT-emacs assumes a Windows shell. Change to baash.
    (setq shell-file-name "bash")
    (setenv "SHELL" shell-file-name)
    (setq explicit-shell-file-name shell-file-name)
    (setq ediff-shell shell-file-name)
    (setq explicit-shell-args '("--login" "-i"))

    ;; This removes unsightly ^M characters that would otherwise
    ;; appear in the output of java applications.
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

    ;; Make tramp ssh work
    (setq tramp-default-method "sshx")

    ;;; Follow Cygwin symlinks.
    ;;; Handles old-style (text file) symlinks and new-style (.lnk file) symlinks.
    ;;; (Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still loaded as such.)
    (defun follow-cygwin-symlink ()
      "Follow Cygwin symlinks.
       Handles old-style (text file) and new-style (.lnk file) symlinks.
       \(Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still
       loaded as such.)"
      (save-excursion
	(goto-char 0)
	(if (looking-at
	     "L\x000\x000\x000\x001\x014\x002\x000\x000\x000\x000\x000\x0C0\x000\x000\x000\x000\x000\x000\x046\x00C")
	    (progn
	      (re-search-forward
	       "\x000\\([-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`][-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`]+\\)")
	      (find-alternate-file (match-string 1)))
	  (if (looking-at "!<symlink>")
	      (progn
		(re-search-forward "!<symlink>\\(.*\\)\0")
		(find-alternate-file (match-string 1))))
	  )))
    (add-hook 'find-file-hooks 'follow-cygwin-symlink)

    ;;; Use Unix-style line endings.
    (setq-default buffer-file-coding-system 'undecided-unix)
))
