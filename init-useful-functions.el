;; add to the buffer-local write-contents-functions
;; remove tabs and delete trailing whitespace from files
(defun friendly-whitespace ()
  (interactive)
  (add-hook 'write-contents-functions
            '(lambda()
               (save-excursion
                 (untabify (point-min) (point-max))
                 (delete-trailing-whitespace)))))

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

;; wc
(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (region-beginning) (region-end) "wc -w"))


;; remove duplicate lines
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
	  (progn
	    (goto-char start)
	    (re-search-forward "^\\(.+\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
	(replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))
