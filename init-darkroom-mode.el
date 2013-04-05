;;; darkroom-mode.el - Distraction free editing mode
;; Author: Martin Svenson, modified by Tim Felgentreff
;; Usage: M-x darkroom-mode
;; License: free for all usages/modifications/distributions/whatever.

(require 'cl)

;; ------ Customization -----
(defgroup darkroom nil
  "A full-screen mode for distraction-free editing."
  :group 'convenience)

(defcustom darkroom-mode-left-margin 20
  "Margin to add to the left side of the screen."
  :type 'integer
  :group 'darkroom)

(defcustom darkroom-mode-right-margin 0
  "Margin to add to the right side of the screen."
  :type 'integer
  :group 'darkroom)

(defcustom darkroom-mode-font-increase 7
  "Increase for font size
   XXX: This needs to be 7 on my Windows setup for fullscreen to cover
   the taskbar"
  :type 'integer
  :group 'darkroom)

(defcustom darkroom-mode-enable-longline-wrap t
  "If `longlines-mode', should `longlines-wrap-follows-window-size'
also be enabled on entering `darkroom-mode'?"
  :type 'boolean
  :group 'darkroom)

(defcustom darkroom-mode-enable-hook nil
  "Hook called by darkroom-mode-enable."
  :type 'hook
  :group 'darkroom)

(defcustom darkroom-mode-enable-hook nil
  "Hook called by darkroom-mode-disable."
  :type 'hook
  :group 'darkroom)

;; -------- code start -------
(setq *darkroom-mode-memtable* (make-hash-table))

(defun* darkroom-recall (var &optional (frame (selected-frame)))
  (cdr (assoc var (gethash frame *darkroom-mode-memtable*))))

(defun* darkroom-remember (var val &optional (frame (selected-frame)))
  (let* ((varlist (gethash frame *darkroom-mode-memtable*))
	 (target (assoc var varlist)))
    (cond (target
	   (setf (cdr target) val))
	  (t
	   (puthash frame (cons (cons var val)
				varlist) *darkroom-mode-memtable*)))))

(defun darkroom-mode-set-enabled(var)
  (darkroom-remember 'darkroom-mode-enabled var))

(defun darkroom-mode-enabledp()
  (darkroom-recall 'darkroom-mode-enabled))

(defun darkroom-mode-update-window()
  (set-window-margins (selected-window)
		      left-margin-width
		      right-margin-width))

;;;###autoload
(defun darkroom-mode ()
  (interactive)
  (cond ((darkroom-mode-enabledp)
	 (darkroom-mode-disable))
	(t
	 (darkroom-mode-enable))))

(defun darkroom-mode-enable()
  (interactive)
  ; ----- margins
  ; note: margins are buffer local, so if multi-monitor support is
  ;       enabled, frame-locals are used. Otherwise, it's set
  ;       globally.
  ; - remember margins (only needed if multi-monitor support is disabled)
  (darkroom-remember 'left-margin-width
		     (default-value 'left-margin-width))
  (darkroom-remember 'right-margin-width
		     (default-value 'right-margin-width))
  ; - set margins
  (setq-default left-margin-width darkroom-mode-left-margin)
  (setq-default right-margin-width darkroom-mode-right-margin)
  (darkroom-mode-update-window)

  ; - set font size
  (enlarge-font darkroom-mode-font-increase)

  ; ----- other settings
  ; - remember
  (darkroom-remember 'line-spacing (frame-parameter nil 'line-spacing))
  (when (and  (boundp 'longlines-mode)
	      longlines-mode
	      darkroom-mode-enable-longline-wrap)
    (darkroom-remember 'longlines-wrap-follow
	  longlines-wrap-follows-window-size))
  ; - set
  (modify-frame-parameters (selected-frame)
			   '((line-spacing . 10)))
  (when (and 
	 (boundp 'longlines-mode)
	 longlines-mode
	 darkroom-mode-enable-longline-wrap)
    (longlines-mode 0)
    (setq longlines-wrap-follows-window-size t)
    (longlines-mode 1))

  (run-hooks 'darkroom-mode-enable-hook)
  (darkroom-mode-set-enabled t)

  ; ---- frame size
  (if (eq nil (frame-parameter nil 'fullscreen))
      (progn
  	(toggle-fullscreen)
  	(set-frame-parameter nil 'fullscreen 'fullboth)))

  (message (format "darkroom mode enabled on %s" (selected-frame))))

(defun darkroom-mode-disable()
  (interactive)
  (run-hooks 'darkroom-mode-disable-hook)

  ; - restore settings
  (modify-frame-parameters
   (selected-frame)
   `((line-spacing . ,(darkroom-recall 'line-spacing))))
  (when (and
	 (boundp 'longlines-mode)
	 longlines-mode
	 (darkroom-recall 'longlines-wrap-follow)
	 darkroom-mode-enable-longline-wrap)
    (longlines-mode 0)
    (setq longlines-wrap-follows-window-size
	  (darkroom-recall 'longlines-wrap-follow))
    (longlines-mode 1))
  ; - restore margins
  (setq-default left-margin-width
		(darkroom-recall 'left-margin-width))
  (setq-default right-margin-width
		(darkroom-recall 'right-margin-width))
  (darkroom-mode-update-window)

  ; - set font size
  (enlarge-font (- 0 darkroom-mode-font-increase))

  ; - restore frame size
  (if (frame-parameter nil 'fullscreen)
      (progn
	(toggle-fullscreen)
	(set-frame-parameter nil 'fullscreen nil)))

  (darkroom-mode-set-enabled nil)
  (message (format "darkroom-mode disabled on %s" (selected-frame))))

;;;;;;;;;;;;;;;;; end ;;;;;;;;;;;;;;;;;
(provide 'darkroom-mode)
