;;; -*- lexical-binding: t -*-
(require 'use-package)

(use-package term-keys
  :ensure t
  :unless (or (daemonp) (display-graphic-p))
  :config
  (global-set-key (kbd "M-[ 1 ; 2 a") (kbd "S-<up>"))
  (global-set-key (kbd "M-[ 1 ; 2 b") (kbd "S-<down>"))
  (global-set-key (kbd "M-[ 1 ; 5 a") (kbd "C-<up>"))
  (global-set-key (kbd "M-[ 1 ; 5 b") (kbd "C-<down>"))
  (global-set-key (kbd "M-[ 1 ; 5 d") (kbd "C-<left>"))
  (global-set-key (kbd "M-[ 1 ; 5 c") (kbd "C-<right>"))
  (term-keys-mode t))

(use-package kitty-graphics
  :ensure t
  :unless (or (daemonp) (display-graphic-p))
  :vc (:url "https://github.com/cashmeredev/kitty-graphics.el" :branch "master" :rev :newest)
  :config
  (kitty-graphics-mode 1))

(use-package clipetty
  :ensure t
  :unless (or (daemonp) (display-graphic-p)))
