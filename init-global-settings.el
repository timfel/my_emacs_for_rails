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

;; Always start in HOME
(setq default-directory "~/")
;; Get rid of toolbar and scrollbar
;; (tool-bar-mode nil)
;; (menu-bar-mode)
;; (scroll-bar-mode nil)
;; wheel mouse
(mouse-wheel-mode t)
;; Standard copy'n'paste
;; (cua-mode 1)
;; fix clipboard
(if (eq window-system 'x)
    (progn
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

(global-set-key (kbd "C->") 'forward-list)
(global-set-key (kbd "C-<") 'backward-list)

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
      (if (equal string "\\<\\>")
	  	  (setq string ""))
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
;; (add-hook 'LaTeX-mode-hook (lambda () (local-set-key "\M-i" 'ispell-word)))
(setq reftex-plug-into-AUCTeX t)
(setq TeX-auto-save t)
(setq TeX-save-query nil)
(setq TeX-parse-self t)


;; Javascript/JSON
(add-hook 'javascript-mode-hook 'friendly-whitespace)
(add-hook 'js-mode-hook 'friendly-whitespace)


;; RML mode
(load (expand-file-name "~/.emacs.d/rml/rml-mode.el"))
