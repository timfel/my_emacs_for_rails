;; stops me killing emacs by accident!
(setq confirm-kill-emacs 'yes-or-no-p)

;; move deleted files to trash
(setq delete-by-moving-to-trash t)

;; C/C++/Java Options
(setq-default c-basic-offset 4)

;; Auxtex Options
(require 'reftex)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'reftex-mode)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (local-set-key "\M-i" 'ispell-word)))
(setq reftex-plug-into-AUCTeX t)
(setq TeX-auto-save t)
(setq TeX-save-query nil)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

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
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'c++-mode-hook 'flyspell-prog-mode)
(add-hook 'c-mode-hook 'flyspell-prog-mode)
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
(condition-case nil
    (if (not (server-running-p)) ;; Server might be running
        (server-start))
  (error nil)) ;; If this throws an error, we're in daemon mode

;(setq locale-coding-system 'utf-8)
;(set-terminal-coding-system 'utf-8)
;(set-keyboard-coding-system 'utf-8)
;(set-selection-coding-system 'utf-8)
;(prefer-coding-system 'utf-8)
;'(buffer-encoding (quote utf-8))
'(recentf-mode t)
;'(transient-mark-mode t)

;(set-default-font "Bitstream Vera Sans Mono-10")
;(set-fontset-font (frame-parameter nil 'font)
;  'han '("cwTeXHeiBold" . "unicode-bmp"))

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

;; (setq default-frame-alist '((font . "inconsolata")))

;; Get back font antialiasing
(push '(font-backend xft x) default-frame-alist)

;(global-font-lock-mode t t)
(setq font-lock-maximum-decoration t)

;(setq default-directory "~/Documentos/WyeWorks/Proys/")
(setq default-directory "~/")

;; Get rid of toolbar, scrollbar, menubar
(progn
  (tool-bar-mode nil)
  ;; (menu-bar-mode)
  (scroll-bar-mode nil))

(add-to-list 'load-path "~/.emacs.d/plugins/textmate")
(require 'textmate)
(textmate-mode 1)

;; redo
(add-to-list  'load-path "~/.emacs.d/plugins/redo")
(require 'redo)
(global-set-key [(control -)] 'redo)

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


;; Centering code stolen from somewhere and restolen from
;; http://www.chrislott.org/geek/emacs/dotemacs.html
;; centers the screen around a line...
(global-set-key "\C-cc"  'centerer)
(defun centerer ()
   "Repositions current line: once middle, twice top, thrice bottom"
   (interactive)
   (cond ((eq last-command 'centerer2)  ; 3 times pressed = bottom
          (recenter -1))
         ((eq last-command 'centerer1)  ; 2 times pressed = top
          (recenter 0)
          (setq this-command 'centerer2))
         (t                             ; 1 time pressed = middle
          (recenter)
          (setq this-command 'centerer1))))


;; Kills live buffers, leaves some emacs work buffers
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun nuke-some-buffers (&optional list)
  "For each buffer in LIST, kill it silently if unmodified. Otherwise ask.
LIST defaults to all existing live buffers."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
           (name (buffer-name buffer)))
      (and (not (string-equal name ""))
           ;(not (string-equal name "*Messages*"))
          ;; (not (string-equal name "*Buffer List*"))
           ;(not (string-equal name "*buffer-selection*"))
           ;(not (string-equal name "*Shell Command Output*"))
           (not (string-equal name "*scratch*"))
           (/= (aref name 0) ? )
           (if (buffer-modified-p buffer)
               (if (yes-or-no-p
                    (format "Buffer %s has been edited. Kill? " name))
                   (kill-buffer buffer))
             (kill-buffer buffer))))
    (setq list (cdr list))))


;; fullscreen
;(defun toggle-fullscreen ()
;(interactive)
;(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;'(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
;(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;'(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;)
;(toggle-fullscreen)

;; Commenting blocks
(global-set-key [(control /)] 'comment-or-uncomment-region-or-line)

;; maxframe
(add-to-list  'load-path "~/.emacs.d/plugins/maxframe")
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)
(add-hook 'window-setup-hook 'ecb-redraw-layout t)

;(set-background-color "#2b2b2b")
;(set-foreground-color "white")
;(set-face-background 'modeline "DarkRed")
;(set-face-foreground 'modeline "white")
;; color-theme
(add-to-list  'load-path "~/.emacs.d/plugins/color-theme")
(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-github)
(color-theme-github)

(mouse-wheel-mode t)
;; wheel mouse
;(defun up-slightly () (interactive) (scroll-up 5))
;(defun down-slightly () (interactive) (scroll-down 5))
;(global-set-key [mouse-4] 'down-slightly)
;(global-set-key [mouse-5] 'up-slightly)
;(defun up-one () (interactive) (scroll-up 1))
;(defun down-one () (interactive) (scroll-down 1))
;(global-set-key [S-mouse-4] 'down-one)
;(global-set-key [S-mouse-5] 'up-one)
;(defun up-a-lot () (interactive) (scroll-up))
;(defun down-a-lot () (interactive) (scroll-down))
;(global-set-key [C-mouse-4] 'down-a-lot)
;(global-set-key [C-mouse-5] 'up-a-lot)


;; cedet
;; See cedet/common/cedet.info for configuration details.
(load-file "~/.emacs.d/plugins/cedet/common/cedet.el")
; Enable EDE (Project Management) features
(global-ede-mode 1)
;; * This enables the database and idle reparse engines
(semantic-load-enable-minimum-features)
(semantic-load-enable-code-helpers)
(setq semantic-load-turn-everything-on t)
;(global-set-key "\C-c/" 'Semantic-ia-complete-symbol)
;(global-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)

;; ecb
(add-to-list 'load-path "~/.emacs.d/plugins/ecb")
(require 'ecb)
(setq ecb-tip-of-the-day nil)
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
(condition-case nil
    (ecb-activate)
  (error nil)) ;; On small displays and in daemon mode, this errors out
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-source-path (quote (("~/Devel/projects" "Dev")
                           ("~/Documents/HPI/10WS/" "Uni")
                           ("~/Finnlabs/Finnlabs/" "Finn"))))
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
; '(show-paren-mode t))


;; find-recursive
(add-to-list 'load-path "~/.emacs.d/plugins/find-recursive")
(require 'find-recursive)

;; Tabbar
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
 )

;; anything
(add-to-list 'load-path "~/.emacs.d/plugins/anything")
(require 'anything)

;; anything-rcodetools
(add-to-list 'load-path "~/.emacs.d/plugins/rcodetools")
;(require 'rcodetools)
;(require 'icicles-rcodetools)
;(require 'anything)
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
(ido-mode t)

;; tabkey2
(load "~/.emacs.d/tabkey2.el")
(tabkey2-mode nil)

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


;; css
;(add-to-list  'load-path "~/.emacs.d/plugins/css-mode")
;(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
;(setq auto-mode-alist (append '(("\\.css$" . css-mode)) auto-mode-alist))
(add-hook 'css-mode-hook
         (lambda()
           (local-set-key (kbd "<return>") 'newline-and-indent)
))


;; javascript
(add-to-list  'load-path "~/.emacs.d/plugins/javascript")
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

(defvar javascript-identifier-regexp "[a-zA-Z0-9.$_]+")

(defun javascript-imenu-create-method-index-1 (class bound)
  (let (result)
    (while (re-search-forward (format "^ +\\(\%s\\): *function" javascript-identifier-regexp) bound t)
      (push (cons (format "%s.%s" class (match-string 1)) (match-beginning 1)) result))
    (nreverse result)))

(defun javascript-imenu-create-method-index()
  (cons "Methods"
        (let (result)
          (dolist (pattern (list (format "\\b\\(%s\\) *= *Class\.create" javascript-identifier-regexp)
                                 (format "\\b\\([A-Z]%s\\) *= *Object.extend(%s"
                                         javascript-identifier-regexp
                                         javascript-identifier-regexp)
                                 (format "^ *Object.extend(\\([A-Z]%s\\)" javascript-identifier-regexp)
                                 (format "\\b\\([A-Z]%s\\) *= *{" javascript-identifier-regexp)))
            (goto-char (point-min))
            (while (re-search-forward pattern (point-max) t)
              (save-excursion
                (condition-case nil
                    (let ((class (replace-regexp-in-string "\.prototype$" "" (match-string 1)))
                          (try 3))
                      (if (eq (char-after) ?\()
                          (down-list))
                      (if (eq (char-before) ?{)
                          (backward-up-list))
                      (forward-list)
                      (while (and (> try 0) (not (eq (char-before) ?})))
                        (forward-list)
                        (decf try))
                      (if (eq (char-before) ?})
                          (let ((bound (point)))
                            (backward-list)
                            (setq result (append result (javascript-imenu-create-method-index-1 class bound))))))
                  (error nil)))))
          (delete-duplicates result :test (lambda (a b) (= (cdr a) (cdr b)))))))

(defun javascript-imenu-create-function-index ()
  (cons "Functions"
         (let (result)
           (dolist (pattern '("\\b\\([[:alnum:].$]+\\) *= *function" "function \\([[:alnum:].]+\\)"))
             (goto-char (point-min))
             (while (re-search-forward pattern (point-max) t)
               (push (cons (match-string 1) (match-beginning 1)) result)))
           (nreverse result))))

(defun javascript-imenu-create-index ()
  (list
   (javascript-imenu-create-function-index)
   (javascript-imenu-create-method-index)))

(add-hook 'javascript-mode-hook
  (lambda ()
    (setq imenu-create-index-function 'javascript-imenu-create-index)
    (local-set-key (kbd "<return>") 'newline-and-indent)
    (setq javascript-indent-level 2)
  )
t)

;; ruby-mode
(add-to-list 'load-path "~/.emacs.d/plugins/ruby-mode")
(require 'ruby-mode)
(require 'ruby-electric)
(add-hook 'ruby-mode-hook 'turn-on-font-lock)
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("^Gemfile$" . ruby-mode))

;; ruby-block
(add-to-list 'load-path "~/.emacs.d/plugins/ruby-block")
(require 'ruby-block)

;; ruby electric
(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))
(setq hippie-expand-try-functions-list
     '(try-complete-abbrev
   try-complete-file-name
   try-expand-dabbrev))


;; yaml
(add-to-list 'load-path "~/.emacs.d/plugins/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("^Gemfile.lock$" . yaml-mode))

;; rdebug
(add-to-list 'load-path "~/.emacs.d/plugins/rdebug")
(require 'rdebug)
(setq rdebug-short-key-mode t)


;; ri-emacs
(setq ri-ruby-script (expand-file-name "~/.emacs.d/plugins/ri-emacs/ri-emacs.rb"))
;(autoload 'ri (expand-file-name "~/.emacs.d/plugins/ri-emacs/ri-ruby.el") nil t)
(load "~/.emacs.d/plugins/ri-emacs/ri-ruby.el")

;; rsense
(setq rsense-home (expand-file-name "~/.emacs.d/plugins/rsense"))
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

;; ruby-mode-hook
(add-hook 'ruby-mode-hook
         (lambda()
           (add-hook 'write-file-functions
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace)
                           )))
           (set (make-local-variable 'indent-tabs-mode) 'nil)
           (set (make-local-variable 'tab-width) 2)
           (imenu-add-to-menubar "IMENU")
;           (require 'ruby-electric)
           (ruby-electric-mode t)
;           (require 'ruby-block)
           (ruby-block-mode t)
;           (local-set-key 'f1 'ri)
           (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
;           (local-set-key 'f4 'ri-ruby-show-args)
           (if (project-current)
               (rsense-open-project (project-default-directory (project-current))))
           (define-key ruby-mode-map "\M-\C-o" 'rct-complete-symbol)
           (local-set-key (kbd "<return>") 'newline-and-indent)
))

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

;; Feature request: have flymake create its temp files in the system temp file directory instead of in the same directory as the file. When using it with Ruby on Rails and autotest, autotest sees the temp file and tries to do something with it and dies, forcing me to restart it, thus killing the magic of autotest. Putting flymake’s temp files elsewhere seems like the easiest way to dodge this.
;;
;; I second the above request. I know there are workarounds for autotest, but it seems like we don’t want to find work arounds for every new web framework, we want to get flymake working in a way that won’t conflict with any other tools.
;;
;; It is easy to patch your autotest to ignore flymake files. I have submitted a patch which hopefully will be included in future releases. For more info see: Emacs, flymake and autotest: the fix
;;
;; Here is a suggestion for a solution (100% untested). Replace flymake-create-temp-inplace above with

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

(add-hook 'ruby-mode-hook
          '(lambda ()

             ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                 (flymake-mode))
             ))

(require 'flymake-jslint)
(add-hook 'javascript-mode-hook
          '(lambda ()
             ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                 (flymake-mode))
             ))

;; Rinari
(add-to-list 'load-path "~/.emacs.d/plugins/rinari")
(require 'rinari)
(setq rinari-tags-file-name "TAGS")


;; yasnippet
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")
(setq require-final-newline nil)

;; yasnippet rails
(load "~/.emacs.d/plugins/yasnippets-rails/setup.el")


(add-to-list 'load-path "~/.emacs.d/plugins/autotest")
(require 'autotest)


;; rhtml-mode
(add-to-list 'load-path "~/.emacs.d/plugins/rhtml")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
  (lambda () (rinari-launch)))
(add-to-list 'auto-mode-alist '("\\.html.erb$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.html.rb$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
(add-hook 'rhtml-mode
          (let ((original-command (lookup-key rhtml-mode-map [tab])))
            `(lambda ()
               (setq yas/fallback-behavior
                     '(apply ,original-command))
               (local-set-key [tab] 'yas/expand))))


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

   (add-hook 'ruby-mode-hook
             (lambda ()
               (setq ac-sources (append '(ac-source-rsense-method ac-source-rsense-constant) ac-sources))
               (setq ac-sources (append ac-sources '(ac-source-words-in-same-mode-buffers)))
               (setq ac-omni-completion-sources '(("\\.\\=" ac-source-rcodetools)))));)

;; emacs-project-mode
(add-to-list 'load-path "~/.emacs.d/plugins/emacs-project-mode")
(require 'project-mode)
(project-mode 1)
(project-mode-menu)
(project-load-all)
(global-set-key "\C-t" 'project-fuzzy-search)

;; ri
;(load "~/.emacs.d/plugins/ri/ri.el")

;; snippet
;(add-to-list 'load-path "~/.emacs.d/plugins/snippet")

;; rails-emacs
;(add-to-list 'load-path "~/.emacs.d/plugins/emacs-rails")
;(require 'rails)

;(kill-buffer "*ESS*")
;(kill-buffer "*Compile-Log*")
;(kill-buffer "*Messages*")



;; Standard copy'n'paste
(cua-mode 1)

;; Always force spaces
(setq-default indent-tabs-mode nil)

;; Some useful shortcuts
(global-set-key "\C-l" 'goto-line)

;; Fix clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
