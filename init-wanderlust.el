;; Tabbar, this is in emacs-goodies, so it'll only work in Ubuntu
(condition-case nil
    (save-excursion
      (require 'wl)
      (require 'wl-other-frame)
      (require 'wl-draft)
      (require 'mailcrypt)
      
      ;; IMAP
      (setq elmo-imap4-default-server "imap.gmail.com")
      (setq elmo-imap4-default-user "timfelgentreff@gmail.com")
      (setq elmo-imap4-default-authenticate-type 'clear)
      (setq elmo-imap4-default-port '993)
      (setq elmo-imap4-default-stream-type 'ssl)

      (setq elmo-imap4-use-modified-utf7 t)

      ;; SMTP
      (setq wl-smtp-connection-type 'starttls)
      (setq wl-smtp-posting-port 587)
      (setq wl-smtp-authenticate-type "plain")
      (setq wl-smtp-posting-user "timfelgentreff")
      (setq wl-smtp-posting-server "smtp.gmail.com")
      (setq wl-local-domain "gmail.com")

      (setq wl-default-folder "%inbox")
      (setq wl-default-spec "%")
      (setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
      (setq wl-trash-folder "%[Gmail]/Trash")

      (setq wl-folder-check-async t)

      (setq elmo-imap4-use-modified-utf7 t)

      (autoload 'wl-user-agent-compose "wl-draft" nil t)
      (if (boundp 'mail-user-agent)
	  (setq mail-user-agent 'wl-user-agent))
      (if (fboundp 'define-mail-user-agent)
	  (define-mail-user-agent
	    'wl-user-agent
	    'wl-user-agent-compose
	    'wl-draft-send
	    'wl-draft-kill
	    'mail-send-hook))

      (add-hook 'wl-summary-mode-hook 'mc-install-read-mode)
      (add-hook 'wl-mail-setup-hook 'mc-install-write-mode)

      (defun mc-wl-verify-signature ()
	(interactive)
	(save-window-excursion
	  (wl-summary-jump-to-current-message)
	  (mc-verify)))

      (defun mc-wl-decrypt-message ()
	(interactive)
	(save-window-excursion
	  (wl-summary-jump-to-current-message)
	  (let ((inhibit-read-only t))
	    (mc-decrypt))))

      (eval-after-load "mailcrypt"
	'(setq mc-modes-alist
	       (append
		(quote
		 ((wl-draft-mode (encrypt . mc-encrypt-message)
				 (sign . mc-sign-message))
		  (wl-summary-mode (decrypt . mc-wl-decrypt-message)
				   (verify . mc-wl-verify-signature))))
		mc-modes-alist))))
  (error nil))
