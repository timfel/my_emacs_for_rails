(defun load-init-files (list)
  (mapcar (lambda (e) (load (expand-file-name (format "~/.emacs.d/init-%s.el" e))))
          list))

;; Run el-get
(load-init-files '(el-get-packages))
;; Some global functions, settings and modes
(load-init-files '(useful-functions global-settings))
;; Now setup packages that weren't loaded and/or setup through el-get
(load-init-files '(irc flyspell flymake tabbar term-mode ido server tramp bibtex skeletons))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list (quote (("Okular" "okular --unique %o#src:%n%b"))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "xdg-open") (output-dvi "xdg-open") (output-pdf "Okular") (output-html "xdg-open"))))
 '(ecb-compilation-buffer-names (quote (("*Calculator*") ("*vc*") ("*vc-diff*") ("*Apropos*") ("*Occur*") ("*shell*") ("\\*[cC]ompilation.*\\*" . t) ("\\*i?grep.*\\*" . t) ("*JDEE Compile Server*") ("*Help*") ("*Completions*") ("*Backtrace*") ("*Compile-log*") ("*bsh*") ("*Messages*") ("*Buffer List*"))))
 '(ecb-compile-window-height 8)
 '(ecb-compile-window-temporally-enlarge (quote both))
 '(ecb-compile-window-width (quote edit-window))
 '(ecb-layout-name "left8")
 '(ecb-layout-window-sizes (quote (("left8" (ecb-directories-buffer-name 0.23671497584541062 . 0.29310344827586204) (ecb-sources-buffer-name 0.23671497584541062 . 0.22413793103448276) (ecb-methods-buffer-name 0.23671497584541062 . 0.25862068965517243) (ecb-history-buffer-name 0.23671497584541062 . 0.20689655172413793)))))
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote (("~/Devel/projects" "Dev") ("~/Documents/HPI/11SS/" "Uni") ("~/Finnlabs/Finnlabs/" "Finn"))))
 '(ecb-tree-indent 2)
 '(ecb-windows-width 0.2)
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . "evince %s"))))
 '(safe-local-variable-values (quote ((encoding . us-ascii) (encoding . utf-8)))))
;; resize the windows on emacs and run ecb-store-window-sizes
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
