(recentf-mode t)
(show-paren-mode t)
(blink-cursor-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode t)

(windmove-default-keybindings)

(fset 'yes-or-no-p 'y-or-n-p)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setq confirm-kill-emacs 'yes-or-no-p
      visible-bell nil
      ring-bell-function #'ignore
      delete-by-moving-to-trash t
      make-backup-files nil
      query-replace-highlight t
      search-highlight t
      font-lock-maximum-decoration t
      require-final-newline t
      major-mode 'text-mode
      show-paren-style 'mixed
      inhibit-startup-screen t
      default-directory "~/"
      initial-scratch-message nil
      comment-multi-line t
      comment-style 'extra-line
      sentence-end-double-space nil
      frame-title-format '(multiple-frames "%b" ("" (:eval (if (functionp 'treemacs-current-workspace) (or (treemacs-workspace->name (treemacs-current-workspace)) "%b") "No workspace %b")))))

(setq-default fill-column 79
	      indent-tabs-mode nil
	      buffer-file-coding-system 'utf-8-unix
	      c-basic-offset 4)

(global-set-key (kbd "M-<up>") 'timfel/move-line-up)
(global-set-key (kbd "M-<down>") 'timfel/move-line-down)
(global-set-key (kbd "ESC <up>") 'timfel/move-line-up)
(global-set-key (kbd "ESC <down>") 'timfel/move-line-down)
(global-set-key (kbd "C-z") 'beep)
(global-set-key (kbd "M-q") 'fill-paragraph)
(global-set-key [f11] 'toggle-frame-fullscreen)

(global-set-key (kbd "C-M-q")
		(lambda ()
		  (interactive)
		  (save-excursion
		    (let ((fill-column 12345678))
		      (fill-paragraph)
		      (let ((end (save-excursion
				   (forward-paragraph 1)
				   (backward-sentence)
				   (point-marker))))
			(beginning-of-line)
			(while (progn (forward-sentence)
				      (<= (point) (marker-position end)))
			  (just-one-space)
			  (delete-char -1)
			  (newline t)))))))

(global-set-key (kbd "C-x <left>")
		(lambda ()
		  (interactive)
		  (push-mark)
		  (if (xref-marker-stack-empty-p)
		      (xref-pop-marker-stack)
		    (previous-buffer))))

(global-set-key (kbd "C-x <right>")
		(lambda ()
		  (interactive)
		  (pop-global-mark)))

(let ((scroll-down-in-place (lambda (n)
			      (interactive "p")
			      (previous-line n)
			      (scroll-down n)))
      (scroll-up-in-place (lambda (n)
			    (interactive "p")
			    (next-line n)
			    (scroll-up n))))
  (global-set-key [mouse-4] scroll-down-in-place)
  (global-set-key [mouse-5] scroll-up-in-place)
  (global-set-key [C-up] scroll-down-in-place)
  (global-set-key [C-down] scroll-up-in-place))

(global-set-key (kbd "C-/")
		(lambda ()
		  (interactive)
		  (if mark-active
		      (call-interactively #'comment-or-uncomment-region)
		    (save-excursion
		      (beginning-of-line)
		      (set-mark (point))
		      (end-of-line)
		      (call-interactively #'comment-or-uncomment-region)))))

(if (not (display-graphic-p))
    (progn
      (xterm-mouse-mode 1)
      (global-set-key (kbd "M-[ 1 ; 3 a") 'timfel/move-line-up)
      (global-set-key (kbd "M-[ 1 ; 3 b") 'timfel/move-line-down)
      (global-set-key (kbd "M-[ 1 ; 2 a") (kbd "S-<up>"))
      (global-set-key (kbd "M-[ 1 ; 2 b") (kbd "S-<down>"))
      (global-set-key (kbd "M-[ 1 ; 5 a") (kbd "C-<up>"))
      (global-set-key (kbd "M-[ 1 ; 5 b") (kbd "C-<down>"))
      (global-set-key (kbd "M-[ 1 ; 5 d") (kbd "C-<left>"))
      (global-set-key (kbd "M-[ 1 ; 5 c") (kbd "C-<right>")))
  (progn
    (global-set-key (kbd "M-]") 'forward-list)
    (global-set-key (kbd "M-[") 'backward-list)))


(if (display-graphic-p)
    (run-with-idle-timer 0 nil
			 (lambda ()
			   (if (memq window-system '(x pgtk))
			       (set-face-attribute 'default nil :font "DejaVu Sans Mono-10")
			     (if (eq window-system 'w32)
				 (set-face-attribute 'default nil :family "Consolas" :height 105))))))

(when (eq system-type 'windows-nt)
  (with-eval-after-load 'grep
    ;; findstr can handle the basic find|grep use case
    (grep-apply-setting 'grep-find-template
			"findstr /S /N /D:. /C:<R> <F>")
    (setq find-name-arg nil)))

(when (eq system-type 'gnu/linux)
  (let ((exe (or (executable-find "wslview") "xdg-open")))
    (setq browse-url-generic-program
	  exe
	  browse-url-browser-function 'browse-url-generic))

  (advice-add 'x-clipboard-yank
	      :override
	      (lambda ()
		(interactive)
		(insert (shell-command-to-string "timeout 0.1 xsel"))))

  (add-to-list 'term-file-aliases
	       '("st-256color" . "xterm-256color")
	       '("screen-256color" . "xterm-256color"))

  (if (or (eq window-system 'pgtk)
	  (and (not window-system) (getenv "WAYLAND_DISPLAY")))
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
	(setq interprogram-cut-function 'wl-copy
	      interprogram-paste-function 'wl-paste))))

(when-let* ((nvm "~/.nvm/versions/node/")
	  (_ (file-exists-p nvm)))
  (add-to-list 'exec-path (f-join nvm (car (sort (directory-files nvm) #'string-greaterp)) "bin"))
  (setenv "PATH" (string-join exec-path path-separator)))

(when-let* ((texlive "/usr/local/texlive/2014/bin/x86_64-linux/")
	    _ (file-exists-p texlive))
  (add-to-list 'exec-path texlive)
  (setenv "PATH" (concat texlive ":" (getenv "PATH"))))

(when-let* ((jdk21 (expand-file-name "~/.mx/jdks/labsjdk-ce-21/"))
	    (_ (and (file-exists-p jdk21) (not (getenv "JAVA_HOME")))))
  (setenv "JAVA_HOME" jdk21))

(when-let* ((sdkman (getenv "SDKMAN_DIR"))
            (jdk21 (expand-file-name (concat sdkman "candidates/java/21.0.4-oracle")))
	    (_ (and (file-exists-p jdk21) (not (getenv "JAVA_HOME")))))
  (setenv "JAVA_HOME" jdk21))

(when-let* ((eclipse (expand-file-name "~/dev/eclipse/eclipse"))
	    (_ (and (file-exists-p eclipse) (not (getenv "ECLIPSE_EXE")))))
  (setenv "ECLIPSE_EXE" eclipse))
