;; add to the buffer-local write-contents-functions
;; remove tabs and delete trailing whitespace from files
(defun friendly-whitespace ()
  (interactive)
  (add-hook 'write-contents-functions
            #'(lambda()
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
               (not (seq-some
                (lambda (x) (string-match-p x name))
                '("^$" "\\*Messages\\*" "\\*Buffer List\\*" "\\*buffer-selection\\*"
                  "\\*Shell Command Output\\*" "\\*scratch\\*" "ECB" "magit")))
               (/= (aref name 0) ? ))
          (if (buffer-modified-p buffer)
              (if (yes-or-no-p
                   (format "Buffer %s has been edited. Kill? " name))
                  (kill-buffer buffer))
            (kill-buffer buffer))))))

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

(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.
Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  (interactive)
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count)
        (progn
          (setq indent-tabs-mode nil)
          (message "using spaces for indentation")))
    (if (> tab-count space-count)
        (progn
          (setq indent-tabs-mode t
                c-basic-offset 4
                tab-width 4)
          (message "using tabs for indentation")))))

;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (eshell-command
;;    (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;; ;;;  Jonas.Jarnestrom<at>ki.ericsson.se A smarter
;; ;;;  find-tag that automagically reruns etags when it cant find a
;; ;;;  requested item and then makes a new try to locate it.
;; ;;;  Fri Mar 15 09:52:14 2002
;; (defadvice find-tag (around refresh-etags activate)
;;   "Rerun etags and reload tags if tag not found and redo find-tag.
;;    If buffer is modified, ask about save before running etags."
;;   (let ((extension (file-name-extension (buffer-file-name))))
;;     (condition-case err
;;         ad-do-it
;;       (error (and (buffer-modified-p)
;;                   (not (ding))
;;                   (y-or-n-p "Buffer is modified, save it? ")
;;                   (save-buffer))
;;              (er-refresh-etags extension)
;;              ad-do-it))))

;; (defun er-refresh-etags (extension)
;;   "Run etags on all peer files in current dir and reload them silently."
;;   (interactive "sExtensions (inserted as regex in *.%%s): ")
;;   (shell-command (format "etags -a *.%s" (or extension "el")))
;;   (let ((tags-revert-without-query t))  ; don't query, revert silently
;;     (visit-tags-table default-directory nil)))


(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun update-proxies-from-wpad ()
  (interactive)
  ;; (setq wpad (shell-command-to-string "curl -s wpad")
  (if url-proxy-services
      (setq url-proxy-services nil)
    (let ((wpad (shell-command-to-string "curl -s wpad")))
      (if (string-match "PROXY\s\\([^; \n\t]+\\)" wpad)
          (progn
            (setq wpad (match-string-no-properties 1 wpad))
            (let ((wpad_with_protocol (if (not (string-match "^https?://" wpad))
                                          (concat "http://" wpad)
                                        wpad)))
              (setenv "http_proxy" wpad_with_protocol)
              (setenv "https_proxy" wpad_with_protocol)
              (setenv "HTTP_PROXY" wpad_with_protocol)
              (setenv "HTTPS_PROXY" wpad_with_protocol))
            (if (string-match "\\([^:]+\\):\\([0-9]+\\)$" wpad)
                (progn
                  (setq url-proxy-services
                        (list (cons "http" wpad)
                              (cons "https" wpad)))
                  (setq copilot-network-proxy
                        `(:host ,(match-string-no-properties 1 wpad)
                                :port ,(string-to-number (match-string-no-properties 2 wpad)))))
              (progn
                (setq url-proxy-services
                      (list (cons "http" (concat wpad ":80"))
                            (cons "https" (concat wpad ":443"))))
                (setq copilot-network-proxy `(:host ,wpad :port 443))))
            (message "Proxies set: %s" url-proxy-services))
        (progn
          (setq url-proxy-services nil)
          (setenv "http_proxy" nil)
          (setenv "https_proxy" nil)
          (setenv "HTTP_PROXY" nil)
          (setenv "HTTPS_PROXY" nil)
          (message "Proxies disabled")))))
  (cdar url-proxy-services))

(defun projector-enable ()
  (interactive)
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-15")
  (display-line-numbers-mode)
  (load-theme 'tango))

(defun ales/fill-paragraph (&optional P)
  "When called with prefix argument call `fill-paragraph'.
Otherwise split the current paragraph into one sentence per line."
  (interactive "P")
  (if (not P)
      (save-excursion 
        (let ((fill-column 12345678)) ;; relies on dynamic binding
          (fill-paragraph) ;; this will not work correctly if the paragraph is
                           ;; longer than 12345678 characters (in which case the
                           ;; file must be at least 12MB long. This is unlikely.)
          (let ((end (save-excursion
                       (forward-paragraph 1)
                       (backward-sentence)
                       (point-marker))))  ;; remember where to stop
            (beginning-of-line)
            (while (progn (forward-sentence)
                          (<= (point) (marker-position end)))
              (just-one-space) ;; leaves only one space, point is after it
              (delete-char -1) ;; delete the space
              (newline)        ;; and insert a newline
              ))))
    ;; otherwise do ordinary fill paragraph
    (fill-paragraph P)))


(defun ospl/fill-paragraph ()
  "Fill the current paragraph until there is one sentence per line.
This unfills the paragraph, and places hard line breaks after each sentence."
  (interactive)
  
  (save-excursion
    (fill-paragraph)
    ;; "Unfill the paragraph at point.
    ;; This repeatedly calls `join-line' until the whole paragraph does
    ;; not contain hard line breaks any more."
    (let ((fill-column 100000))
      (fill-paragraph))
    (beginning-of-line)

    ;; insert line breaks again
    (let ((end-of-paragraph (make-marker)))
      (set-marker end-of-paragraph (line-end-position))
      (forward-sentence)
      (while (< (point) end-of-paragraph)
        (just-one-space)
        (delete-backward-char 1)
        (newline)
        (forward-sentence))
      (set-marker end-of-paragraph nil))))

(defun org-narrow-to-calls ()
  (interactive)
  (while (< (point) (point-max))
    (let ((heading-title (car (cddddr (org-heading-components))))
          (heading-depth (car (org-heading-components))))
      (if (= heading-depth 4)
          (if (or (equal "python call" (downcase heading-title))
                  (equal "leads call" (downcase heading-title)))
              (org-next-visible-heading 1)
            (org-cut-subtree))
        (org-next-visible-heading 1)))))

