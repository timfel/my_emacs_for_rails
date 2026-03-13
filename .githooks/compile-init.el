;;; compile-init.el --- minimal init for batch byte-compilation  -*- lexical-binding: t; -*-

;; Keep this file small and side-effect free: it exists to make `require' work
;; during `emacs -Q --batch' byte-compilation.

(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
(setq load-prefer-newer t)

;; Activate installed package.el packages so their directories are on `load-path'.
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; Add local lisp directory.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(provide 'compile-init)
;;; compile-init.el ends here
