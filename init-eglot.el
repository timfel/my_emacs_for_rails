;;; -*- lexical-binding: t -*-
(require 'use-package)

(use-package eglot-jdtls
  :after eglot
  :demand t
  :functions (eglot-jdtls)
  :config
  (add-to-list 'eglot-server-programs
               (cons '(java-mode java-ts-mode) #'eglot-jdtls)))

(use-package eglot-jdb
  :after eglot
  :demand t
  :commands (eglot-jdb))
