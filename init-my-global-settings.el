;;
(windmove-default-keybindings)
;; don't ding
(setq visible-bell nil
      ring-bell-function #'ignore)
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
;; (push '(font-backend xft x) default-frame-alist)

(run-with-idle-timer 0 nil (lambda ()
                        (if (memq window-system '(x pgtk))
                            (set-face-attribute 'default nil :font "DejaVu Sans Mono-10"))
                        ;; (set-face-attribute 'default nil :family "Terminus (TTF)"  :height 100))
                        ;; (set-face-attribute 'default nil :family "Noto Sans Mono" :height 110)
                        ;; (add-to-list 'default-frame-alist
                        ;;     	 '(font . "-*-terminus-medium-*-*-*-16-*-*-*-*-*-*-*")))

                        (if (eq window-system 'w32)
                            (set-face-attribute 'default nil :family "Consolas" :height 105))))

(when (eq system-type 'windows-nt)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (with-eval-after-load 'grep
    ;; findstr can handle the basic find|grep use case
    (grep-apply-setting 'grep-find-template
                        "findstr /S /N /D:. /C:<R> <F>")
    (setq find-name-arg nil)))

(setq-default fill-column 79)
(global-set-key (kbd "M-q") (lambda () (interactive) (fill-paragraph)))

(global-set-key
 [f11]
 (lambda ()
   (interactive)
   (toggle-frame-fullscreen)))

;; if run in terminal, use the mouse
(if (eq system-type 'gnu/linux)
    (xterm-mouse-mode 0))

;; Don't even blink
(blink-cursor-mode 0)

;; Narrow (C-x n n)
(put 'narrow-to-region 'disabled nil)

;; Include Texlive path
(if (file-exists-p "/usr/local/texlive/2014/bin/x86_64-linux/")
    (setenv "PATH" (concat "/usr/local/texlive/2014/bin/x86_64-linux/:" (getenv "PATH"))))

;; Always start in HOME
(setq default-directory "~/")
;; Get rid of toolbar and scrollbar
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
;; wheel mouse
;; Standard copy'n'paste
;; (cua-mode 1)
;; fix clipboard
;; (if window-system
;;     (progn
;;       (mouse-wheel-mode t)
;;       (setq x-select-enable-clipboard t)
;;       (setq interprogram-paste-function 'x-selection-value)))
(column-number-mode t)

;; Use the default browser on linux
(if (eq system-type 'gnu/linux)
    (let ((exe (or (executable-find "wslview") "xdg-open")))
      (setq browse-url-generic-program
            exe
            browse-url-browser-function 'browse-url-generic)))

;; Hippie expand
(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))

;; C/C++/Java Options
(setq-default c-basic-offset 4)

;; some navigation keys
(if (window-system)
    (progn
      (global-set-key (kbd "M-]") 'forward-list)
      (global-set-key (kbd "M-[") 'backward-list)))

(defun my/previous-position ()
  (interactive)
  ;; push onto the mark ring
  (push-mark)
  (if (xref-marker-stack-empty-p)
      (xref-pop-marker-stack)
    (previous-buffer)))
(global-set-key (kbd "C-x <left>") 'my/previous-position)

(defun my/next-position ()
  (interactive)
  (pop-global-mark))
(global-set-key (kbd "C-x <right>") 'my/next-position)

;; Use generic printer dialiog on linux
(if (eq system-type 'gnu/linux)
    (progn 
      (setq lpr-command "xpp")
      (setq ps-lpr-command "xpp")))

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

(if (and
     (fboundp 'xref-goto-xref)
     (not (fboundp 'xref-quit-and-goto-xref)))
    (defun xref-quit-and-goto-xref ()
      "Quit *xref* buffer, then jump to xref on current line."
      (interactive)
      (let* ((buffer (current-buffer))
             (xref (or (xref--item-at-point)
                       (user-error "No reference at point")))
             (xref--current-item xref))
        (quit-window nil nil)
        (xref--show-location (xref-item-location xref) t)
        (next-error-found buffer (current-buffer)))))

;; ISearch word under cursor
(defun my-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

(defun my-isearch-yank-word-hook ()
  (when (equal this-command 'my-isearch-word-at-point)
    (let ((string (concat "\\<"
                          (buffer-substring-no-properties
                           (progn (skip-syntax-backward "w_") (point))
                           (progn (skip-syntax-forward "w_") (point)))
                          "\\>")))
      (skip-syntax-backward "w_") ;; go before the current search term
      (if (and isearch-case-fold-search
               (eq 'not-yanks search-upper-case))
          (setq string (downcase string)))
      (setq string (regexp-quote (substring string 2 (- (length string) 2))))
      (setq isearch-string string
            isearch-message
            (concat isearch-message
                    (mapconcat 'isearch-text-char-description
                               string ""))
            isearch-yank-flag t)
      (isearch-search-and-update)
      )))

(add-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)
(global-set-key (kbd "C-S-s") 'my-isearch-word-at-point)
(define-key isearch-mode-map [backspace] 'isearch-edit-string)


;; (defun my-get-top-vcs-dir (previous &optional current)
;;   (interactive)
;;   (let* ((last (if current previous (file-truename previous)))
;; 	 (dir (if current current (expand-file-name ".." last))))
;;     (if (or (file-directory-p (concat (file-name-as-directory dir) ".git"))
;; 	    (file-directory-p (concat (file-name-as-directory dir) ".bzr"))
;; 	    (file-directory-p (concat (file-name-as-directory dir) ".hg")))
;; 	dir
;;       (my-get-top-vcs-dir dir (expand-file-name ".." dir)))))

;; (defun ensure-tags-file ()
;;   (interactive)
;;   (if (or (not tags-file-name) (equal this-command 'my-ensure-tags-file))
;;       (let* ((topdir (or (my-get-top-vcs-dir buffer-file-truename)
;; 			 default-directory))
;; 	     (tagsdir (file-name-as-directory
;; 		       (or (my-get-top-vcs-dir buffer-file-truename)
;; 			   (expand-file-name ".." buffer-file-truename))))
;; 	     (tag-file (concat tagsdir "TAGS")))
;; 	(shell-command (format "cd \"%s\"; find . -name \"*.%s\" -print | xargs %s -o %s"
;; 			       topdir
;; 			       (file-name-extension buffer-file-truename)
;; 			       (expand-file-name "../etags" el-get-emacs)
;; 			       tag-file))
;; 	(visit-tags-table tag-file))))

;; ;; Taggs....
;; (defadvice find-tag (before c-tag-file activate)
;;   (ensure-tags-file))

;; Python hooks
(add-hook 'python-mode-hook 'turn-on-font-lock)
(add-hook 'python-mode-hook
	  #'(lambda() (progn
		       ;; Auto completion
		       (imenu-add-to-menubar "IMENU")
		       (setq ac-sources
			     '(ac-source-python
			       ac-source-semantic
			       ac-source-words-in-same-mode-buffers
			       ac-source-yasnippet
			       ac-source-abbrev)))))


;; (defun fresh-frame()
;;   (interactive)
;;   (let* ((curbuf (current-buffer))
;; 	 (new-frame (make-frame))
;; 	 (old-frame (selected-frame)))
;;     (select-frame new-frame)
;;     (switch-to-buffer curbuf)
;;     (set-frame-parameter new-frame 'background-mode 'light)
;;     (enable-theme 'solarized)
;;     (delete-frame old-frame)))
;; (fresh-frame)

;; ;; Workaround the annoying warnings:
;; ;;    Warning (mumamo-per-buffer-local-vars):
;; ;;    Already 'permanent-local t: buffer-file-name
;; (when (or (> emacs-major-version 24)
;;       	  (and (= emacs-major-version 24)
;;                 (= emacs-minor-version 2)))
;;   (eval-after-load "mumamo"
;;     '(setq mumamo-per-buffer-local-vars
;;            (delq 'buffer-file-name mumamo-per-buffer-local-vars))))


;; (defun my-find-file-check-make-small-files-fci ()
;;   "If a file is under a given size draw a nice fci indicator."
;;   (if (and (< (buffer-size) (* 1024 1024))
;; 	   window-system)
;;       (fci-mode 1)))
;; (add-hook 'find-file-hook 'my-find-file-check-make-small-files-fci)


(defun xclip-paste ()
  (interactive)
  (insert (shell-command-to-string "timeout 0.1 xsel")))
(advice-add 'x-clipboard-yank :override #'xclip-paste)

;; (x-clipboard-yank)

(setq-default indent-tabs-mode nil)

;; mail
;; (load-library "smtpmail")
;; (setq send-mail-function 'smtpmail-send-it
;;       send-mail-function 'smtpmail-send-it
;;       gnutls-algorithm-priority "NORMAL:%COMPAT"
;;       mail-setup_with_from nil
;;       user-full-name "Tim Felgentreff"
;;       ;; user-mail-address "tim.felgentreff@hpi.de"
;;       ;; smtpmail-smtp-server "owa.hpi.de"
;;       ;; smtpmail-default-smtp-server "owa.hpi.de"
;;       ;; smtpmail-stream-type  'starttls
;;       ;; smtpmail-smtp-service 587
;;       ;; mail-host-address "hpi.de")
;;       user-mail-address "timfelgentreff@gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-stream-type  'ssl
;;       smtpmail-smtp-service 465
;;       mail-host-address "timfelgentreff.gmail.com")
;; (add-hook 'mail-mode-hook 'mail-abbrevs-setup)
;; (setq mail-yank-prefix "> "
;;       mail-signature ""
;;       mail-default-headers "")

;; (setq split-height-threshold nil
;;       split-width-threshold 160)

;; (setq split-height-threshold 120
;;       split-width-threshold 160)

;; (defun my-split-window-sensibly (&optional window)
;;     "replacement `split-window-sensibly' function which prefers vertical splits"
;;     (interactive)
;;     (let ((window (or window (selected-window))))
;;         (or (and (window-splittable-p window t)
;;                  (with-selected-window window
;;                      (split-window-right)))
;;             (and (window-splittable-p window)
;;                  (with-selected-window window
;;                      (split-window-below))))))

;; (setq split-window-preferred-function #'my-split-window-sensibly)


;; ------------------------------------------------------------------
;; display-buffer

;; The default behaviour of `display-buffer' is to always create a new
;; window. As I normally use a large display sporting a number of
;; side-by-side windows, this is a bit obnoxious.
;;
;; The code below will make Emacs reuse existing windows, with the
;; exception that if have a single window open in a large display, it
;; will be split horisontally.
;; (setq pop-up-windows nil)

;; (defun my-display-buffer-function (buf not-this-window)
;;   (if (and (not pop-up-frames)
;;            (one-window-p)
;;            (or not-this-window
;;                (not (eq (window-buffer (selected-window)) buf)))
;;            (> (frame-width) 162))
;;       (split-window-horizontally))
;;   ;; Note: Some modules sets `pop-up-windows' to t before calling
;;   ;; `display-buffer' -- Why, oh, why!
;;   (let ((display-buffer-function nil)
;;         (pop-up-windows nil))
;;     (display-buffer buf not-this-window)))

;; (setq display-buffer-function 'my-display-buffer-function)

(put 'dired-find-alternate-file 'disabled nil)

(add-to-list 'term-file-aliases
             '("st-256color" . "xterm-256color")
             '("screen-256color" . "xterm-256color"))

;; (setq display-buffer-alist
;;       '((".java" (display-buffer-same-window) ())
;;         (".py" (display-buffer-same-window) ())
;;         (".c" (display-buffer-same-window) ())
;;         (".h" (display-buffer-same-window) ())))

(if (or
     (eq window-system 'pgtk)
     (and (eq window-system nil)
          (eq system-type 'gnu/linux)
          (getenv "WAYLAND_DISPLAY")))
    (progn
      (setq wl-copy-process nil)
      (defun wl-copy (text)
        (setq wl-copy-process (make-process :name "wl-copy"
                                            :buffer nil
                                            :command '("wl-copy" "-f" "-n")
                                            :connection-type 'pipe))
        (process-send-string wl-copy-process text)
        (process-send-eof wl-copy-process))
      (defun wl-paste ()
        (if (and wl-copy-process (process-live-p wl-copy-process))
            nil ; should return nil if we're the current paste owner
          (shell-command-to-string "wl-paste -n | tr -d \r")))
      (setq interprogram-cut-function 'wl-copy)
      (setq interprogram-paste-function 'wl-paste)))

(global-set-key (kbd "C-z") 'beep)

;; add latest nvm node if we have it
(if-let* ((nvm "~/.nvm/versions/node/")
          (_ (file-exists-p nvm)))
    (and
     (add-to-list 'exec-path (f-join nvm (car (sort (directory-files nvm) #'string-greaterp)) "bin"))
     (setenv "PATH" (string-join exec-path path-separator))))

(setq frame-title-format
      '(multiple-frames "%b" ("" (:eval (if (functionp 'treemacs-current-workspace) (or (treemacs-workspace->name (treemacs-current-workspace)) "%b") "No workspace %b")))))

(if window-system
    (progn
      (global-set-key (kbd "M-<up>") 'move-line-up)
      (global-set-key (kbd "M-<down>") 'move-line-down))
  (global-set-key (kbd "ESC <up>") 'move-line-up)
  (global-set-key (kbd "ESC <down>") 'move-line-down))

(setq sentence-end-double-space nil)

(global-set-key (kbd "C-M-q") #'ospl/fill-paragraph)

(if (not (display-graphic-p))
    (progn
      (global-set-key (kbd "M-[ 1 ; 3 a") 'move-line-up)
      (global-set-key (kbd "M-[ 1 ; 3 b") 'move-line-down)
      (global-set-key (kbd "M-[ 1 ; 2 a") (kbd "S-<up>"))
      (global-set-key (kbd "M-[ 1 ; 2 b") (kbd "S-<down>"))
      (global-set-key (kbd "M-[ 1 ; 5 a") (kbd "C-<up>"))
      (global-set-key (kbd "M-[ 1 ; 5 b") (kbd "C-<down>"))
      (global-set-key (kbd "M-[ 1 ; 5 d") (kbd "C-<left>"))
      (global-set-key (kbd "M-[ 1 ; 5 c") (kbd "C-<right>"))))
