(defun org-mode-reftex-setup ()
  (interactive)
  (reftex-mode t)
  (reftex-mode nil)
  (reftex-parse-all)
  (global-auto-revert-mode t)
  ;; add a custom reftex cite format to insert links
  (reftex-set-cite-format
   '((?b . "[[bib:%l][%l-bib]]")
     (?n . "[[notes:%l][%l]]")
     (?p . "[[papers:%l][%l.pdf]]")
     (?t . "%t")
     (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l.pdf]]")))
  (local-set-key (kbd "C-x C-s") (lambda () (interactive) (write-file (buffer-file-name)) (rename-buffer "*bibtex*")))
  (local-set-key (kbd "C-c )") 'reftex-citation)
  (local-set-key (kbd "C-c (") 'org-mode-reftex-search))

(defun org-mode-reftex-search ()
  ;;jump to the notes for the paper pointed to at from reftex search
  (interactive)
  (org-open-link-from-string (format "[[notes:%s]]" (reftex-citation t))))

(defun bibtex ()
  (interactive)
  (find-file "~/Dropbox/Papers/bibtex.org")
  (rename-buffer "*bibtex*")
  (org-mode-reftex-setup))

(setq org-link-abbrev-alist
      '(("bib" . "~/Dropbox/Papers/bibtex.bib::%s")
	("notes" . "~/Dropbox/Papers/bibtex.org::#%s")
	("papers" . "~/Dropbox/Papers/%s.pdf")))

(setq reftex-default-bibliography (list (expand-file-name "~/Dropbox/Papers/bibtex.bib")))
