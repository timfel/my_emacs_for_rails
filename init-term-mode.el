;; Term mode
;; enable cua and transient mark modes in term-line-mode
(defadvice term-line-mode (after term-line-mode-fixes ())
  (set (make-local-variable 'truncate-lines) nil)
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) t)
  (local-set-key "\C-c\C-n" 'term-char-mode))
(ad-activate 'term-line-mode)
;; disable cua and transient mark modes in term-char-mode
(defadvice term-char-mode (after term-char-mode-fixes ())
  (set (make-local-variable 'truncate-lines) nil)
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) nil))
(ad-activate 'term-char-mode)
