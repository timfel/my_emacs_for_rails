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
                               (setq position (if (overlayp (cdr symbol))
						  (overlay-start (cdr symbol))
						(cdr symbol))))

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

;; Patch ruby-mode
(defun ruby-accurate-end-of-block (&optional end)
  "TODO: document."
  (let (state
        (end (or end (point-max))))
    (while (and (setq state (apply 'ruby-parse-partial end state))
		(nth 2 state) (>= (nth 2 state) 0) (< (point) end)))))
