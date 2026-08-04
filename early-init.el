;;; -*- lexical-binding: t -*-

(setq load-prefer-newer t)       ;; do not trip up when .el is newer than .elc

(setenv "LSP_USE_PLISTS" "true") ;; if we load lsp-mode, this makes it faster
(defvar lsp-use-plists)
(setq lsp-use-plists t)

(when (eq system-type 'android)  ;; the android version works best with termux
  (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":"  termuxpath))
    (add-to-list 'exec-path termuxpath t)))
