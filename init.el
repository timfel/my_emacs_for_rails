(condition-case nil
    (when
        (load
         (expand-file-name "~/.emacs.d/elpa/package.el"))
      (package-initialize))
  (error (let ((buffer (url-retrieve-synchronously
                        "http://tromey.com/elpa/package-install.el")))
           (save-excursion
             (set-buffer buffer)
             (goto-char (point-min))
             (re-search-forward "^$" nil 'move)
             (eval-region (point) (point-max))
             (kill-buffer (current-buffer))
             (when
                 (load
                  (expand-file-name "~/.emacs.d/elpa/package.el"))
               (package-initialize))))))
(setq package-archives '("tromey" . "http://tromey.com/elpa/"))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(require 'compile) ;; Needed for some reason or other. el-get fails for me, otherwise

(setq el-get-sources
      '(
	
	magithub gist ruby-electric autopair haml-mode nxhtml
	rspec-mode sass-mode cssh el-get switch-window vkill
	yasnippet xcscope anything auto-complete sudo-save

	(:name redo+
	       :type http
	       :url "http://www.emacswiki.org/emacs/download/redo%2b.el"
	       :load "redo%2b.el"
	       :post-init (lambda () (global-set-key [(control -)] 'redo)))

	(:name textlint
	       :type git
	       :url "git://github.com/DamienCassou/textlint.git"
	       :load "textlint.el")

	(:name ecb
	       :load-path "."
	       :features ecb
	       :post-init (lambda () (progn
				       (global-ede-mode 1)
				       (semantic-mode t)
				       (setq semantic-load-turn-everything-on t)
				       (setq ecb-tip-of-the-day nil)
				       (setq ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
				       (if window-system
					   (if (>= (window-height) 16)
					       (progn
						 (ecb-activate)
						 (add-hook 'window-setup-hook 'ecb-redraw-layout t))
					     (message "Not activating ECB, window height to small"))
					 (message "Not activating ECB, not using a window system")))))

	;; auto-complete-clang auto-complete-etags auto-complete-extensions

	(:name color-theme
	       :load-path "."
	       :load "color-theme.el")
	(:name color-theme-solarized
	       :depends color-theme
	       :type git
	       :url "https://github.com/sellout/emacs-color-theme-solarized.git"
	       :load "color-theme-solarized.el"
	       :post-init (lambda () (color-theme-solarized-light)))

	(:name coffee-mode
	       :post-init (lambda () (add-hook 'coffee-mode-hook
					       '(lambda() (progn
							    ;; Enable compile-on-save if there is already a *.coffee & *.js file
							    (if (and (file-exists-p (buffer-file-name))
								     (file-exists-p (coffee-compiled-file-name)))
								(coffee-cos-mode t))
							    (setq coffee-args-compile '("-c" "--bare"))
							    (set (make-local-variable 'tab-width) 2)
							    (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer))))))

	(:name yaml-mode
	       :post-init (lambda () (progn
				       (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
				       (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
				       (add-to-list 'auto-mode-alist '("Gemfile.lock$" . yaml-mode)))))

	(:name maxframe
	       :features maxframe
	       :post-init (lambda () (add-hook 'window-setup-hook 'maximize-frame t)))

	;; (:name rinari
	;;       :post-init (lambda () (setq rinari-tags-file-name "TAGS")))

	(:name ri-emacs
	       :post-init (lambda () (setq ri-ruby-script (expand-file-name (concat el-get-dir "/ri-emacs/ri-emacs.rb")))))

	(:name rhtml-mode
	       :features rhtml-mode
	       :post-init (lambda () (progn
				       ;; (add-hook 'rhtml-mode-hook
				;;		 (lambda () (rinari-launch)))
				       (add-to-list 'auto-mode-alist '("\\.html.erb$" . rhtml-mode))
				       (add-to-list 'auto-mode-alist '("\\.html.rb$" . rhtml-mode))
				       (add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
				       )))

	(:name ruby-mode
	       :after (lambda () (progn
				   (add-hook 'ruby-mode-hook 'turn-on-font-lock)
				   (add-hook 'ruby-mode-hook '(lambda() ((add-hook 'write-contents-functions
										   '(lambda()
										      (save-excursion
											(untabify (point-min) (point-max))
											(delete-trailing-whitespace)
											)))
									 (ruby-electric-mode t)
									 (ruby-block-mode t)
									 ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
									 (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
									     (flymake-mode))
									 ;; Indenting options
									 (set (make-local-variable 'indent-tabs-mode) 'nil)
									 (set (make-local-variable 'tab-width) 2)
									 (local-set-key (kbd "<return>") 'newline-and-indent)
									 ;; Auto completion
									 (imenu-add-to-menubar "IMENU")
									 (setq ac-sources (append '(ac-source-rsense-method ac-source-rsense-constant) ac-sources))
									 (setq ac-sources (append ac-sources '(ac-source-words-in-same-mode-buffers)))
									 (setq ac-omni-completion-sources '(("\\.\\=" ac-source-rcodetools)))
									 (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
									 (define-key ruby-mode-map "\M-\C-o" 'rct-complete-symbol)
									 ;; Type inference auto completion
									 (if (project-current)
									     (rsense-open-project (project-default-directory (project-current)))))))
				   (add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
				   (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
				   (add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
				   (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
				   (add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode)))))

	(:name cucumber
	       :type git
	       :url "git://github.com/michaelklishin/cucumber.el.git"
	       :load-path "."
	       :features feature-mode
	       :depends '(yasnippet)
	       :post-init (lambda () (progn
				       ;; load bundle snippets
				       (yas/load-directory (expand-file-name (concat el-get-dir "/cucumber/snippets")))
				       (add-to-list 'auto-mode-alist '("\\.feature" . feature-mode)))))

	(:name rsense
	       :type git
	       :url "git://github.com/m2ym/rsense.git"
	       :prepare (lambda () (setq rsense-home (expand-file-name (concat el-get-dir "/rsense"))))
	       :features rsense
	       :load-path "etc")

	(:name js2-mode
	       :post-init (lambda () (progn
				       (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
				       (add-hook 'js2-mode-hook
						 (lambda () (progn
							      (setq imenu-create-index-function 'javascript-imenu-create-index)
							      (local-set-key (kbd "<return>") 'newline-and-indent)
							      (setq javascript-indent-level 2)))
						 t))))

	(:name textmate
	       :type git
	       :url "git://github.com/defunkt/textmate.el.git"
	       :load "textmate.el"
	       :post-init (lambda () (textmate-mode 1)))

	(:name magit
	       :features (magit magit-svn)
	       :post-init (lambda () (global-set-key (kbd "C-x C-z") 'magit-status)))

	(:name project-mode
	       :type git
	       :url "http://github.com/timfel/emacs-project-mode.git"
	       :load-path "."
	       :features project-mode
	       :post-init (lambda () (progn
				       (project-mode 1)
				       (project-mode-menu)
				       (project-load-all)
				       (global-set-key "\C-t" 'project-fuzzy-search))))

	;; (:name dictionary-el    :type apt-get)
	;; (:name emacs-goodies-el :type apt-get)

	(:name showoff-mode
	       :type git
	       :url "https://github.com/developernotes/showoff-mode.git"
	       :load-path "."
	       :features showoff-mode)

	(:name org-mode
	       :post-init (lambda () (progn
				       (setq org-hide-leading-stars t)
				       (setq org-agenda-files '())
				       (add-to-list 'org-agenda-files (expand-file-name "~/Desktop"))
				       (setq org-agenda-files (append (file-expand-wildcards
								       (expand-file-name "~/Documents/HPI/11SS/*"))
								      org-agenda-files))
				       (setq org-insert-mode-line-in-empty-file t))))

	))

;; Auctex depends on pdflatex being available, only install if desired on this system
(if (executable-find "pdflatex")
    (setq el-get-sources
          (append '((:name auctex
                           :build `("./autogen.sh" ,
                                    (concat "./configure --with-lispdir=`pwd` --with-texmf-dir=$HOME/texmf --with-emacs=" el-get-emacs)
                                    "make"))
                    (:name reftex
                           :post-init (lambda () (progn
                                                   (setq-default TeX-master nil)
                                                   (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
                                                   (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
                                                   (add-hook 'LaTeX-mode-hook 'reftex-mode)
                                                   (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
                                                   (add-hook 'LaTeX-mode-hook 'flyspell-mode)
                                                   (add-hook 'LaTeX-mode-hook (lambda () (local-set-key "\M-i" 'ispell-word)))
                                                   (setq reftex-plug-into-AUCTeX t)
                                                   (setq TeX-auto-save t)
                                                   (setq TeX-save-query nil)
                                                   (setq TeX-parse-self t)
                                                   (setq-default TeX-master nil)))))
                  el-get-sources)))

;; (setq el-get-sources '())
;; (setq my-packages
;;       (mapcar 'el-get-as-symbol
;; 	      (append '(magithub gist ruby-electric autopair
;; 				 haml-mode nxhtml rspec-mode
;; 				 sass-mode cssh el-get
;; 				 switch-window vkill yasnippet
;; 				 xcscope anything auto-complete
;; 				 sudo-save)
;; 		      (mapcar 'el-get-source-name el-get-sources))))
(el-get 'sync)

;; stops me killing emacs by accident!
(setq confirm-kill-emacs 'yes-or-no-p)

;; move deleted files to trash
(setq delete-by-moving-to-trash t)

;; C/C++/Java Options
(setq-default c-basic-offset 4)
(add-hook 'c++-mode-hook 'flyspell-prog-mode)
(add-hook 'c-mode-hook 'flyspell-prog-mode)

;; Flyspell options
(require 'ispell)
(add-to-list 'ispell-dictionary-alist
             '("de"
               "[a-zA-Z\304\326\334\344\366\337\374]"
               "[^a-zA-Z\304\326\334\344\366\337\374]"
               "[']" t ("-C" "-d" "de_DE") "~latin1" iso-8859-15))
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=fast"))
(setq flyspell-issue-message-flag nil)
(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "de") "english" "de")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))
(global-set-key (kbd "<f8>") 'fd-switch-dictionary)

;; Start the emacs server
(setq server-use-tcp t) ;; Use TCP mode, my socket is often unavailable
(setq server-host "127.0.0.1")
(if (functionp 'server-running-p)
    (if (not (server-running-p)) ;; Server might be running
        (server-start)))

(recentf-mode t)

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

;; Get rid of toolbar, scrollbar, menubar
(progn
  (tool-bar-mode nil)
  ;; (menu-bar-mode)
  (scroll-bar-mode nil))

;; show ascii table
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))

;; insert date into buffer at point
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d - %l:%M %p")))

;; Kills some live buffers, leaves some emacs work buffers and the currently active tab
(defun kill-all-but-active-buffers (&optional list)
  "For each buffer in LIST, kill it silently if unmodified. Otherwise ask.
LIST defaults to all existing live buffers."
  (interactive)
  (if (null list) (setq list (buffer-list)))
  (dolist (buffer list)
    (let ((name (buffer-name buffer)))
      (if (and (not (string-equal (buffer-name) name)) ;; Don't kill the active buffer
               (notany
                (lambda (x) (string-match-p x name))
                '("^$" "\\*Messages\\*" "\\*Buffer List\\*" "\\*buffer-selection\\*"
                  "\\*Shell Command Output\\*" "\\*scratch\\*" "ECB" "magit"))
               (/= (aref name 0) ? ))
          (if (buffer-modified-p buffer)
              (if (yes-or-no-p
                   (format "Buffer %s has been edited. Kill? " name))
                  (kill-buffer buffer))
            (kill-buffer buffer))))))
(global-set-key "\C-x\C-ka" 'kill-all-but-active-buffers)

;; fullscreen
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
(condition-case nil
    (toggle-fullscreen)
  (error nil))

;; Commenting blocks
(global-set-key [(control /)] 'comment-or-uncomment-region-or-line)

;; wheel mouse
(mouse-wheel-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-source-path (quote (("~/Devel/projects" "Dev")
                           ("~/Documents/HPI/11SS/" "Uni")
                           ("~/Finnlabs/Finnlabs/" "Finn"))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "xdg-open") (output-dvi "xdg-open") (output-pdf "xdg-open") (output-html "xdg-open"))))
 '(ecb-compilation-buffer-names (quote (("*Calculator*") ("*vc*") ("*vc-diff*") ("*Apropos*") ("*Occur*") ("*shell*") ("\\*[cC]ompilation.*\\*" . t) ("\\*i?grep.*\\*" . t) ("*JDEE Compile Server*") ("*Help*") ("*Completions*") ("*Backtrace*") ("*Compile-log*") ("*bsh*") ("*Messages*") ("*Buffer List*"))))
 '(ecb-compile-window-height 8)
 '(ecb-compile-window-temporally-enlarge (quote both))
 '(ecb-compile-window-width (quote edit-window))
 '(ecb-layout-name "left8")
 '(ecb-layout-window-sizes (quote (("left8" (ecb-directories-buffer-name 0.23671497584541062 . 0.29310344827586204) (ecb-sources-buffer-name 0.23671497584541062 . 0.22413793103448276) (ecb-methods-buffer-name 0.23671497584541062 . 0.25862068965517243) (ecb-history-buffer-name 0.23671497584541062 . 0.20689655172413793)))))
 '(ecb-options-version "2.40")
 '(ecb-tree-indent 2)
 '(ecb-windows-width 0.2))
;; resize the windows on emacs and run ecb-store-window-sizes

;; Tabbar, this is in emacs-goodies, so it'll only work in Ubuntu
(condition-case nil
    (save-excursion
      (require 'tabbar)
      (tabbar-mode t)
      (dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
        (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))

      (defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
        `(defun ,name (arg)
           (interactive "P")
           ,do-always
           (if (equal nil arg)
               ,on-no-prefix
             ,on-prefix)))

      (defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
      (defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))
      (global-set-key [(backtab)] 'shk-tabbar-next)
      (global-set-key "\C-c<left>" 'shk-tabbar-prev)
      ;; add a buffer modification state indicator in the tab label,
      ;; and place a space around the label to make it looks less crowd
      (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
        (setq ad-return-value
              (if (and (buffer-modified-p (tabbar-tab-value tab))
                       (buffer-file-name (tabbar-tab-value tab)))
                  (concat " + " (concat ad-return-value " "))
                (concat " " (concat ad-return-value " ")))))

      ;; called each time the modification state of the buffer changed
      (defun ztl-modification-state-change ()
        (tabbar-set-template tabbar-current-tabset nil)
        (tabbar-display-update))
      ;; first-change-hook is called BEFORE the change is made
      (defun ztl-on-buffer-modification ()
        (set-buffer-modified-p t)
        (ztl-modification-state-change))
      (add-hook 'after-save-hook 'ztl-modification-state-change)
      ;; this doesn't work for revert, I don't know
      ;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
      (add-hook 'first-change-hook 'ztl-on-buffer-modification)
      (custom-set-faces
       ;; custom-set-faces was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       ))
  (error nil))

;; anything-rcodetools
(add-to-list 'load-path "~/.emacs.d/plugins/rcodetools")

(require 'anything-rcodetools)
;;       ;; Command to get all RI entries.
(setq rct-get-all-methods-command "PAGER=cat fri -l -L")
;;       (setq rct-get-all-methods-command "PAGER=cat fri -l")
                                        ;(setq rct-get-all-methods-command "PAGER=cat ri -l")
;;       ;; See docs
;; (define-key ruby-mode-map "\M-\C-i" 'rct-complete-symbol)
(define-key anything-map "\C-z" 'anything-execute-persistent-action)
                                        ;(rct-get-all-methods)

;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(require 'cl)
(ido-mode t)
(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a
   symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))
(global-set-key [(control .)] 'ido-goto-symbol)

;; DTD mode
(autoload 'dtd-mode "tdtd" "Major mode for SGML and XML DTDs." t)
(autoload 'dtd-etags "tdtd" "Execute etags on FILESPEC and match on DTD-specific regular expressions." t)
(autoload 'dtd-grep "tdtd" "Grep for PATTERN in files matching FILESPEC." t)
(setq auto-mode-alist (append (list
                               '("\\.dcl$" . dtd-mode)
                               '("\\.dec$" . dtd-mode)
                               '("\\.dtd$" . dtd-mode)
                               '("\\.ele$" . dtd-mode)
                               '("\\.ent$" . dtd-mode)
                               '("\\.mod$" . etd-mode))
                              auto-mode-alist))

(add-hook 'css-mode-hook (lambda()
                           (local-set-key (kbd "<return>") 'newline-and-indent)))

;; ruby electric
(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))

;; nxhtml
                                        ;(setq *nxhtml-autostart-file* (expand-file-name "~/.emacs.d/plugins/nxhtml/autostart.el"))
                                        ;(load *nxhtml-autostart-file*)
                                        ;(setq
                                        ;      nxhtml-global-minor-mode t
                                        ;      mumamo-chunk-coloring 'submode-colored
                                        ;      nxhtml-skip-welcome t
                                        ;      indent-region-mode t
                                        ;      nxhtml-default-encoding "utf8"
                                        ;      rng-nxml-auto-validate-flag nil
                                        ;      nxml-degraded t)
                                        ;(add-to-list 'auto-mode-alist '("\\.html$" . nxhtml-mumamo-mode))
                                        ;(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . eruby-nxhtml-mumamo-mode))
                                        ;(add-hook 'nxhtml-mumamo-mode-hook 'tabkey2-mode)
                                        ;(add-hook 'eruby-nxhtml-mumamo-mode-hook 'tabkey2-mode)


;; flymake
(add-to-list  'load-path "~/.emacs.d/plugins/flymake")
(require 'flymake)

;; I don't like the default colors :)
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

;; Feature request: have flymake create its temp files in the system
;; temp file directory instead of in the same directory as the
;; file. When using it with Ruby on Rails and autotest, autotest sees
;; the temp file and tries to do something with it and dies, forcing
;; me to restart it, thus killing the magic of autotest. Putting
;; flymake’s temp files elsewhere seems like the easiest way to dodge
;; this.
;;
;; I second the above request. I know there are workarounds for
;; autotest, but it seems like we don’t want to find work arounds for
;; every new web framework, we want to get flymake working in a way
;; that won’t conflict with any other tools.
;;
;; It is easy to patch your autotest to ignore flymake files. I have
;; submitted a patch which hopefully will be included in future
;; releases. For more info see: Emacs, flymake and autotest: the fix
;;
;; Here is a suggestion for a solution (100% untested). Replace
;; flymake-create-temp-inplace above with
(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-intemp))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '(".+\\.rjs$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(require 'flymake-jslint)
(add-hook 'javascript-mode-hook
          '(lambda ()
             ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                 (flymake-mode))
             ))

;; yasnippet rails
;; (load "~/.emacs.d/plugins/yasnippets-rails/setup.el")

(add-to-list 'load-path "~/.emacs.d/plugins/autotest")
(require 'autotest)

(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")
(require 'auto-complete-config)
                                        ;(global-auto-complete-mode t)
                                        ;(define-key ac-complete-mode-map "\C-n" 'ac-next)
                                        ;(define-key ac-complete-mode-map "\C-p" 'ac-previous)
;;     ;; start completion when entered 3 characters
                                        ;(setq ac-auto-start 2)
;; Add following code to your .emacs.
;;
                                        ;(define-key ac-complete-mode-map "\t" 'ac-complete)
                                        ;(define-key ac-complete-mode-map "\r" nil)


                                        ;(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")
                                        ; (when (require 'auto-complete nil t)
                                        ;   (require 'auto-complete-yasnippet)
                                        ;   (require 'auto-complete-ruby)
                                        ;   (require 'auto-complete-css)

(global-auto-complete-mode t)           ;enable global-mode
(setq ac-auto-start t)                  ;automatically start
(setq ac-dwim 3)                        ;Do what i mean
(setq ac-override-local-map nil)        ;don't override local map
(ac-flyspell-workaround)
(setq ac-delay 5)
(setq ac-auto-show-menu 2)
;;   (define-key ac-complete-mode-map "\t" 'ac-expand)
;;   (define-key ac-complete-mode-map "\r" 'ac-complete)
;;   (define-key ac-complete-mode-map "\M-n" 'ac-next)
;;   (define-key ac-complete-mode-map "\M-p" 'ac-previous)
(set-default 'ac-sources '(ac-source-yasnippet ac-source-semantic ac-source-abbrev ac-source-words-in-buffer))

(setq ac-modes
      (append ac-modes
              '(eshell-mode
                                        ;org-mode
                )))
                                        ;(add-to-list 'ac-trigger-commands 'org-self-insert-command)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir ac-source-words-in-buffer))))

;; Standard copy'n'paste
(cua-mode 1)

(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (region-beginning) (region-end) "wc -w"))

;; fix clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; support for opening files via ssh
(require 'tramp)

;; Re-initialize colors when creating a new frame, to fix color-palette incompats between terminal and X
(defun setup-window-system-frame-colours (&rest frame)
  (color-theme-solarized-light))
(defadvice server-create-window-system-frame
  (after set-window-system-frame-colours ())
  "Set custom frame colours when creating the first frame on a display"
  (message "Running after frame-initialize")
  (setup-window-system-frame-colours))
(ad-activate 'server-create-window-system-frame)
(add-hook 'after-make-frame-functions 'setup-window-system-frame-colours t)

