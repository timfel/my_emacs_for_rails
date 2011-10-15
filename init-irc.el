(require 'tls)
(require 'erc)

(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
;; Highlight my first name and my full-name, and maglev, too
(setq erc-keywords '("\\bTim Felgentreff\\b" "\\bTim\\b" "\\bMaglev\\b" "\\bmaglev\\b" "\\bMagLev\\b"))

;; connect or goto irc
(defun irc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "rkh.im:1337") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc-tls :server "rkh.im" :port 1337 :nick "phlebas" :password "timfel" :full-name "Tim Felgentreff"))))

;; A hook for when our keywords or text are matched
(defun my-erc-page-me (match-type nick message)
  "Notify the current user when someone sends a message that
matches a regexp in `erc-keywords'."
  (interactive)
  (when (and (eq match-type 'keyword)
             ;; I don't want to see anything from the erc server
             (not (string-match "\\`\\([sS]erver\\|localhost\\|root\\)" nick))
             ;; or bots
             (not (string-match "\\(^CIA[^!]*\\|bot\\|serv\\)!" nick)))
    (my-erc-page-popup-notification nick message)))
(add-hook 'erc-text-matched-hook 'my-erc-page-me)

;; A hook for private messages
(defun my-erc-page-me-PRIVMSG (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg)))
      (my-erc-page-popup-notification nick msg)
      nil)))
(add-hook 'erc-server-PRIVMSG-functions 'my-erc-page-me-PRIVMSG)

;; Pop-ups for highlights
(defun my-erc-page-popup-notification (nick msg)
  (when window-system
    ;; must set default directory, otherwise start-process is unhappy
    ;; when this is something remote or nonexistent
    (let ((default-directory "~/"))
      ;; 8640000 milliseconds = 1 day
      (start-process "page-me" nil "notify-send"
                     "-u" "normal" "-t" "8640000"
                     (format "%s: %s" nick msg)))))

