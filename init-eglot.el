;;; -*- lexical-binding: t -*-
(require 'use-package)

(use-package eglot-jdtls
  :after eglot
  :demand t
  :functions (eglot-jdtls)
  :config
  (let ((jdtls (expand-file-name (locate-user-emacs-file "lsp-servers/jdtls/bin/"))))
    (add-to-list 'exec-path jdtls)
    (setenv "PATH" (string-join exec-path path-separator)))
  (add-to-list 'eglot-server-programs
               (cons '(java-mode java-ts-mode) #'eglot-jdtls)))

(use-package eglot-jdb
  :after eglot
  :demand t
  :commands (eglot-jdb))

(use-package eglot-booster
  :after eglot
  :demand t
  :if (memq system-type '(windows-nt gnu/linux))
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :branch "main" :rev :newest)
  :ensure t
  :custom (eglot-booster-io-only t)
  :commands (eglot-booster-mode)
  :config
  (unless (executable-find "emacs-lsp-booster")
    (let ((lsp-booster-exe (locate-user-emacs-file "lsp-servers/emacs-lsp-booster")))
      (when (not (file-exists-p lsp-booster-exe))
        (mkdir (file-name-directory lsp-booster-exe) t)
        (let ((zip (concat lsp-booster-exe ".zip")))
          (url-copy-file
           (pcase system-type
             ('windows-nt "https://github.com/blahgeek/emacs-lsp-booster/releases/download/v0.2.1/emacs-lsp-booster_v0.2.1_x86_64-pc-windows-gnu.zip")
             ('gnu/linux "https://github.com/blahgeek/emacs-lsp-booster/releases/download/v0.2.1/emacs-lsp-booster_v0.2.1_x86_64-unknown-linux-musl.zip"))
           zip)
          (let ((default-directory (file-name-directory zip)))
            (shell-command (concat "unzip " zip)))
          (delete-file zip)))
      (add-to-list 'exec-path (file-name-directory lsp-booster-exe))))
  (when (executable-find "emacs-lsp-booster")
    (eglot-booster-mode)))
