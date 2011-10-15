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
	yasnippet xcscope anything sudo-save

	(:name auto-complete
	       :post-init (lambda () (progn
				       (require 'auto-complete-config)
				       (global-auto-complete-mode t)     ;; enable global-mode
				       (setq ac-auto-start nil)          ;; do not automatically start
				       (setq ac-dwim 3)                  ;; Do what i mean
				       (setq ac-override-local-map nil)  ;; don't override local map
				       (ac-flyspell-workaround)
				       (setq ac-delay 5)
				       (setq ac-auto-show-menu 2)
				       (set-default 'ac-sources '(ac-source-yasnippet ac-source-semantic ac-source-abbrev ac-source-words-in-buffer))

				       (setq ac-modes (append ac-modes '(eshell-mode)))
				       (add-hook 'emacs-lisp-mode-hook
						 (lambda ()
						   (setq ac-sources '(ac-source-yasnippet
								      ac-source-abbrev
								      ac-source-words-in-buffer
								      ac-source-symbols))))
				       (add-hook 'eshell-mode-hook
						 (lambda ()
						   (setq ac-sources '(ac-source-yasnippet
								      ac-source-abbrev
								      ac-source-files-in-current-dir 
								      ac-source-words-in-buffer)))))))

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
	       :post-init (lambda () (progn
				       (color-theme-solarized-light)
				       ;; Re-initialize colors when creating a new frame, to fix color-palette incompats between terminal and X
				       (defun setup-window-system-frame-colours (&rest frame)
					 (color-theme-solarized-light))
				       (defadvice server-create-window-system-frame
					 (after set-window-system-frame-colours ())
					 "Set custom frame colours when creating the first frame on a display"
					 (message "Running after frame-initialize")
					 (setup-window-system-frame-colours))
				       (ad-activate 'server-create-window-system-frame)
				       (add-hook 'after-make-frame-functions 'setup-window-system-frame-colours t))))

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
	       :post-init (lambda () (progn
				       (textmate-mode 1)
				       ;; Commenting blocks
				       (global-set-key [(control /)] 'comment-or-uncomment-region-or-line))))

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
