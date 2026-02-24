(if (display-graphic-p)
    (run-with-idle-timer 0 nil
			 (lambda ()
			   (if (memq window-system '(x pgtk))
			       (set-face-attribute 'default nil :font "DejaVu Sans Mono-10")
			     (if (eq window-system 'w32)
				 (set-face-attribute 'default nil :family "Consolas" :height 105))))))

(when (eq system-type 'gnu/linux)
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
