;;; gptel-modeline-status.el --- GPTel status in the modeline -*- lexical-binding: t -*-

;; Copyright (C) 2026 Tim Felgentreff <timfelgentreff@gmail.com>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, version 3.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see https://www.gnu.org/licenses/.

;;; Author: Tim Felgentreff <timfelgentreff@gmail.com>

;;; Code:

(require 'gptel)
(require 'gptel-request)

(defconst gptel-modeline-status-in "🤖💬")
(defconst gptel-modeline-status-out "🤖?")

(defun gptel-modeline-status--update (&optional symbol)
  (setf global-mode-string
        (thread-last global-mode-string
                     (remove 'gptel-modeline-status-in)
                     (remove 'gptel-modeline-status-out)))
  (when symbol
    (setf global-mode-string
          (append global-mode-string
                  (list (intern (format "gptel-modeline-status-%s" symbol))))))
  (force-mode-line-update t))

;;; Hooks

(add-hook 'gptel-post-request-hook
          (lambda (&rest _) (gptel-modeline-status--update 'out)))

(add-hook 'gptel-pre-response-hook
          (lambda (&rest _) (gptel-modeline-status--update 'in)))

(add-hook 'gptel-post-response-functions
          (lambda (&rest _) (gptel-modeline-status--update)))

;;; Advice

(advice-add 'keyboard-quit
            :before (lambda (&rest _args)
                      (ignore-errors (gptel-abort (current-buffer)))
                      (gptel-modeline-status--update)))

(provide 'gptel-modeline-status)
