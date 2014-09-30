(setq backup-inhibited t)
(add-to-list 'default-frame-alist '(background-mode . dark))
(set-background-color "darkslategray")
(set-foreground-color "wheat")
(global-font-lock-mode)
(global-set-key [(meta s)] 'isearch-repeat-forward)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [delete] 'delete-char)
(set-cursor-color "white")
;(custom-set-variables)
;(custom-set-faces)
;(setq font-lock-syntax-table t)
;(setq font-lock-auto-fontify t)
(font-lock-mode)
(setq line-number-mode t)
(setq column-number-mode t)
(setq-default font-maximum-decoration t)
;(set-face-font 'default "-misc-fixed-medium-r-normal--10-*-*-*-c-80-iso8859-1")


;(custom-set-variables
; '(user-mail-address "adrpo@ida.liu.se" t)
; '(query-user-mail-address nil))
;(custom-set-faces)

(setq load-path (cons "/home/adrpo/emacs/" load-path))
(setq load-path (cons "/home/adrpo/rml-2.1.9-debug/elisp" load-path))

(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))

(autoload 'python-mode "python-mode" "Python editing mode." t)


(setq auto-mode-alist (mapcar 'purecopy
                              '(("\\.c$" . c-mode)
                                ("\\.h$" . c-mode)
                                ("\\.daml$" . daml-mode)
                                ("\\.html$" . html-mode)
                                ("\\.shtml$" . html-mode)
                                ("\\.rml$" . rml-mode)
                                 ("\\.el$" . emacs-lisp-mode)
                                ("\\.emacs$" . emacs-lisp-mode)
                                ("\\.tex$" . tex-mode)
				("\\.java$" . java-mode))))

(load-file "/home/adrpo/rml-2.1.9-debug/elisp/rml-db.el") 

(autoload 'daml-mode "damlite" "DAML editing mode." t)
(autoload 'rml-mode "rml-mode" "RML editing mode." t)
;(autoload 'rmldb "rmldb-mode.el" "RML debug mode." t)


