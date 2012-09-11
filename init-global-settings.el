(recentf-mode t)
;; stops me killing emacs by accident!
(setq confirm-kill-emacs 'yes-or-no-p)
;; move deleted files to trash
(setq delete-by-moving-to-trash t)
(setq make-backup-files nil)
(setq query-replace-highlight t)
(setq search-highlight t)
(setq font-lock-maximum-decoration t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq require-final-newline t)
(setq major-mode 'text-mode)
;; turn on paren matching
(show-paren-mode t)
(setq show-paren-style 'mixed)
;; Get rid of the startup screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
;; Get back font antialiasing
(push '(font-backend xft x) default-frame-alist)
(setq font-lock-maximum-decoration t)
(setq default-directory "~/")
;; Get rid of toolbar and scrollbar
(tool-bar-mode nil)
;; (menu-bar-mode)
(scroll-bar-mode nil)
;; wheel mouse
(mouse-wheel-mode t)
;; Standard copy'n'paste
;; (cua-mode 1)
;; fix clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(column-number-mode t)

;; Enable fullscreen on first load
(condition-case nil (toggle-fullscreen) (error nil))

;; Use the default browser on linux
(if (eq system-type 'gnu/linux)
    (setq browse-url-generic-program
          "xdg-open"
          browse-url-browser-function 'browse-url-generic))

;; Hippie expand
(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))

;; C/C++/Java Options
(setq-default c-basic-offset 4)

;; CSS mode tweak
(add-hook 'css-mode-hook (lambda()
                           (local-set-key (kbd "<return>") 'newline-and-indent)))

(global-set-key (kbd "C->") 'forward-list)
(global-set-key (kbd "C-<") 'backward-list)

(setq evernote-username "timfelgentreff") ; optional: you can use this username as default.
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; optional
(add-to-list 'helm-sources anything-c-source-evernote-title)
(defalias 'evernote-find 'anything-evernote-title)

(setq lpr-command "xpp")
(setq ps-lpr-command "xpp")

(global-set-key (kbd "s-c") 'capitalize-word)
(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

(global-set-key [mouse-4] 'scroll-down-in-place)
(global-set-key [mouse-5] 'scroll-up-in-place)
(global-set-key [C-up] 'scroll-down-in-place)
(global-set-key [C-down] 'scroll-up-in-place)

;; Better C multiline comments
(setq comment-multi-line t)
(setq comment-style 'extra-line)
