;; don't ding
(setq visible-bell t)
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
(if (eq window-system 'x)
    (set-face-attribute 'default nil :font "DejaVu Sans Mono-10"))

(setq-default fill-column 80)
(global-set-key (kbd "M-q") (lambda () (interactive) (fill-paragraph 1)))
(global-set-key (kbd "M-q") (lambda () (interactive) (fill-paragraph)))

(global-set-key [f11] 'toggle-fullscreen)

;; Don't even blink
(blink-cursor-mode 0)

;; Narrow (C-x n n)
(put 'narrow-to-region 'disabled nil)

;; Start server on windows, if not happened already
(if (eq 'windows-nt system-type)
    (progn
      (if (not (file-exists-p "~/.emacs.d/server/server"))
	  (server-start))))

;; Include Texlive path
(if (file-exists-p "/usr/local/texlive/2014/bin/x86_64-linux/")
    (setenv "PATH" (concat "/usr/local/texlive/2014/bin/x86_64-linux/:" (getenv "PATH"))))

;; Always start in HOME
(setq default-directory "~/")
;; Get rid of toolbar and scrollbar
(tool-bar-mode 0)
(menu-bar-mode t)
(scroll-bar-mode 0)
;; wheel mouse
;; Standard copy'n'paste
;; (cua-mode 1)
;; fix clipboard
(if window-system
    (progn
      (global-set-key (kbd "M-[") 'insert-pair)
      (global-set-key (kbd "M-{") 'insert-pair)
      (global-set-key (kbd "M-\"") 'insert-pair)
      (mouse-wheel-mode t)
      (setq x-select-enable-clipboard t)
      (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)))
(column-number-mode t)

;; Enable fullscreen on first load
;; (condition-case nil (toggle-fullscreen) (error nil))

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

(if window-system
    (progn
      (global-set-key (kbd "C->") 'forward-list)
      (global-set-key (kbd "C-<") 'backward-list))
  (progn
    (global-set-key (kbd "M-,") 'forward-list)
    (global-set-key (kbd "M-.") 'backward-list)))

;; Use generic printer dialiog on linux
(if (eq system-type 'gnu/linux)
    (progn 
      (setq lpr-command "xpp")
      (setq ps-lpr-command "xpp")))

(global-set-key (kbd "s-c") 'capitalize-word)

(load (expand-file-name "~/.emacs.d/fic-mode.el"))

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

;; Sessions
(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(setq desktop-buffers-not-to-save
      (concat "\\("
	      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
	      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
	      "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
(add-to-list 'desktop-modes-not-to-save 'grep-mode)
(add-to-list 'desktop-modes-not-to-save 'magit-mode)
(run-with-idle-timer
 30 ; seconds
 t  ; repeat
 'desktop-save-in-desktop-dir)


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


(defun my-get-top-vcs-dir (previous &optional current)
  (interactive)
  (let* ((last (if current previous (file-truename previous)))
	 (dir (if current current (expand-file-name ".." last))))
    (if (or (file-directory-p (concat (file-name-as-directory dir) ".git"))
	    (file-directory-p (concat (file-name-as-directory dir) ".bzr"))
	    (file-directory-p (concat (file-name-as-directory dir) ".hg")))
	dir
      (my-get-top-vcs-dir dir (expand-file-name ".." dir)))))

(defun ensure-tags-file ()
  (interactive)
  (if (or (not tags-file-name) (equal this-command 'my-ensure-tags-file))
      (let* ((topdir (or (my-get-top-vcs-dir buffer-file-truename)
			 default-directory))
	     (tagsdir (file-name-as-directory
		       (or (my-get-top-vcs-dir buffer-file-truename)
			   (expand-file-name ".." buffer-file-truename))))
	     (tag-file (concat tagsdir "TAGS")))
	(shell-command (format "cd \"%s\"; find . -name \"*.%s\" -print | xargs %s -o %s"
			       topdir
			       (file-name-extension buffer-file-truename)
			       (expand-file-name "../etags" el-get-emacs)
			       tag-file))
	(visit-tags-table tag-file))))

;; Taggs....
(defadvice find-tag (before c-tag-file activate)
  (ensure-tags-file))

;; PyPyTrace Mode
(load (expand-file-name "~/.emacs.d/pypytrace-mode.el"))

;; Yacc Mode
(load (expand-file-name "~/.emacs.d/yacc.el"))

;; Python hooks
(add-hook 'python-mode-hook 'turn-on-font-lock)
(add-hook 'python-mode-hook 'friendly-whitespace)
(add-hook 'python-mode-hook
	  '(lambda() (progn
		       ;; Auto completion
		       (imenu-add-to-menubar "IMENU")
		       (setq ac-sources
			     '(ac-source-python
			       ac-source-semantic
			       ac-source-words-in-same-mode-buffers
			       ac-source-yasnippet
			       ac-source-abbrev)))))

;; Magit mode hooks
(add-hook 'magit-mode-hook 'magit-load-config-extensions)
;; (setq with-editor-emacsclient-executable "/usr/bin/emacsclient-snapshot")

;; Latex
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'reftex-mode)
;; (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook (lambda () (auto-fill-mode -1)))
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook (lambda () (setq longlines-wrap-follows-window-size t)))
;; (add-hook 'LaTeX-mode-hook 'longlines-mode)
(add-hook 'LaTeX-mode-hook (lambda () (local-set-key "\M-i" 'ispell-word)))
(add-hook 'LaTeX-mode-hook (lambda () (local-set-key "\M-t" 'thesaurus-choose-synonym-and-replace)))
(setq reftex-plug-into-AUCTeX t)
(condition-case nil
    (when
	(add-to-list 'reftex-bibliography-commands "addbibresource"))
  (error nil))
(setq TeX-auto-save t)
(setq TeX-save-query nil)
(setq TeX-parse-self t)
;; (require 'dbus)
;; (defun un-urlify (fname-or-url)
;;   "A trivial function that replaces a prefix of file:/// with just /."
;;   (if (string= (substring fname-or-url 0 8) "file:///")
;;       (substring fname-or-url 7)
;;     fname-or-url))
;; (defun th-evince-sync (file linecol &rest ignored)
;;   (let* ((fname (url-unhex-string (un-urlify file)))
;;          (buf (find-buffer-visiting fname))
;;          (line (car linecol))
;;          (col (cadr linecol)))
;;     (if (null buf)
;;         (message "[Synctex]: %s is not opened..." fname)
;;       (switch-to-buffer buf)
;;       (goto-line (car linecol))
;;       (unless (= col -1)
;;         (move-to-column col)))))
;; (defvar *dbus-evince-signal* nil)
;; (defun enable-evince-sync ()
;;   (require 'dbus)  
;;   (when (and
;;          (eq window-system 'x)
;;          (fboundp 'dbus-register-signal))
;;     (if (not (getenv "DBUS_SESSION_BUS_ADDRESS"))
;; 	(let* ((output (shell-command-to-string "dbus-launch")))
;; 	  (string-match "DBUS_SESSION_BUS_ADDRESS=\\(.*\\)" output)
;; 	  (setenv "DBUS_SESSION_BUS_ADDRESS" (match-string 1 output))))))
;; (enable-evince-sync)
;; (unless *dbus-evince-signal*
;;   (setf *dbus-evince-signal*
;; 	(dbus-register-signal
;; 	 :session nil "/org/gnome/evince/Window/0"
;; 	 "org.gnome.evince.Window" "SyncSource"
;; 	 'th-evince-sync)))
;; (add-hook 'LaTeX-mode-hook 'enable-evince-sync)


;; Javascript/JSON
(add-hook 'javascript-mode-hook 'friendly-whitespace)
(add-hook 'js-mode-hook 'friendly-whitespace)


;; RML mode
(load (expand-file-name "~/.emacs.d/rml/rml-mode.el"))

;; Thesaurus
(setq thesaurus-bhl-api-key "b3d571e9e275682cf7830b0f9c241199")

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

;; Workaround the annoying warnings:
;;    Warning (mumamo-per-buffer-local-vars):
;;    Already 'permanent-local t: buffer-file-name
(when (or (> emacs-major-version 24)
      	  (and (= emacs-major-version 24)
                (= emacs-minor-version 2)))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))

(require 'vlf-setup)
(defun my-vlf-setup-hook ()
  "If a file is over a given size, make the buffer read only."
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (vlf-stop-follow)
  (vlf-toggle-follow)
  (fci-mode 0))
(add-hook 'vlf-mode-hook 'my-vlf-setup-hook)

(defun my-find-file-check-make-small-files-fci ()
  "If a file is under a given size draw a nice fci indicator."
  (if (and (< (buffer-size) (* 1024 1024))
	   window-system)
      (fci-mode 1)))
(add-hook 'find-file-hook 'my-find-file-check-make-small-files-fci)


(defun xclip-paste ()
  (interactive)
  (insert (shell-command-to-string "timeout 0.1 xsel")))
(advice-add 'x-clipboard-yank :override #'xclip-paste)

(x-clipboard-yank)
