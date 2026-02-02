;;; timfel.el --- Utility functions from tim     -*- lexical-binding: t -*-

;; Copyright (C) 2025 Tim Felgentreff <timfelgentreff@gmail.com>
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

;;; Commentary:

;; These are just various utility functions that I picked up over the
;; years and that I find useful in my Emacs usage.

(require 'dash)

(defun timfel/friendly-whitespace ()
  "Add to the buffer-local write-contents-functions remove tabs and delete
trailing whitespace from files"
  (interactive)
  (add-hook 'write-contents-functions
            #'(lambda()
               (save-excursion
                 (untabify (point-min) (point-max))
                 (delete-trailing-whitespace)))))

(defun timfel/ascii-table ()
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

(defun timfel/kill-all-but-active-buffers ()
  "For each buffer, kill it silently if unmodified, not currently visible,
not read-only, and it's name does not start with '*'"
  (interactive)
  (let ((cnt 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (unless (or (buffer-modified-p buf)
                    (get-buffer-window buf 'visible)
                    buffer-read-only
                    (string-match-p "\\*" (buffer-name buf)))
          (kill-buffer buf)
          (setq cnt (1+ cnt)))))
    (message (format "Killed %d buffers." cnt))))

(defun timfel/uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.+\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun timfel/how-many-region (begin end regexp &optional interactive)
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

(defun timfel/infer-indentation-style ()
  (interactive)
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (timfel/how-many-region (point-min) (point-max) "^  "))
        (tab-count (timfel/how-many-region (point-min) (point-max) "^\t")))
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

(defun timfel/move-line (n)
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

(defun timfel/move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun timfel/move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun timfel/update-proxies-from-wpad ()
  (interactive)
  (let* (wpad
         (no_proxy "localhost,127.0.0.1,*.oraclecorp.com,oraclecorp.com,*.oraclecloud.com,oraclecloud.com,*.us.oracle.com")
         (no-proxy (--> no_proxy
                        (regexp-quote it)
                        (string-replace "\\*" ".*" it)
                        (string-replace "," "\\|" it))))
    (if (not (or url-proxy-services
                 (not (setq wpad (shell-command-to-string "curl -s wpad")))
                 (not (string-match "PROXY\s\\([^; \n\t]+\\)" wpad))))
        (progn
          (setq wpad (match-string-no-properties 1 wpad))
          (let ((wpad_with_protocol (if (not (string-match "^https?://" wpad))
                                        (concat "http://" wpad)
                                      wpad)))
            (setenv "http_proxy" wpad_with_protocol)
            (setenv "https_proxy" wpad_with_protocol)
            (setenv "HTTP_PROXY" wpad_with_protocol)
            (setenv "HTTPS_PROXY" wpad_with_protocol)
            (setenv "no_proxy" no_proxy)
            (setenv "NO_PROXY" no_proxy)
            )
          (let* ((m (string-match "\\([^:]+\\):\\([0-9]+\\)$" wpad))
                 (host (if m (match-string-no-properties 1 wpad) wpad))
                 (port (if m (match-string-no-properties 2 wpad) 443))
                 (http (if m wpad (concat wpad ":80")))
                 (https (if m wpad (concat wpad ":443"))))
            (setq
             url-proxy-services (list
                                 (cons "http" http)
                                 (cons "https" https)
                                 (cons "no_proxy" no-proxy))
             copilot-network-proxy `(:host ,host :port ,(string-to-number port)))
            (setenv "GRADLE_OPTS" (format "-Dhttp.proxyHost=%s -Dhttp.proxyPort=%s -Dhttps.proxyHost=%s -Dhttps.proxyPort=%s" host port host port))
            (setenv "MAVEN_OPTS" (format "-Dhttp.proxyHost=%s -Dhttp.proxyPort=%s -Dhttps.proxyHost=%s -Dhttps.proxyPort=%s" host port host port)))
          (message "Proxies set: %s" url-proxy-services))
      (progn
        (setq url-proxy-services nil)
        (setenv "http_proxy" nil)
        (setenv "https_proxy" nil)
        (setenv "no_proxy" nil)
        (setenv "HTTP_PROXY" nil)
        (setenv "HTTPS_PROXY" nil)
        (setenv "NO_PROXY" nil)
        (setenv "MAVEN_OPTS" nil)
        (setenv "GRADLE_OPTS" nil)
        (message "Proxies disabled"))))
  (cdar url-proxy-services))

(defun timfel/org-narrow-to-headings (regexp depth)
  "Narrow the current org buffer to the headings at given depth matching
the given regexp"
  (interactive
   (list (read-string
	  "Regex for headings to keep (lowercase): "
	  "\\(python\\|leads\\).*call")
	 (read-number "Heading depth: " 4)))
  (while (< (point) (point-max))
    (let ((heading-title (car (cddddr (org-heading-components))))
          (heading-depth (car (org-heading-components))))
      (if (= heading-depth depth)
          (if (string-match (downcase regexp) (downcase heading-title))
              (org-next-visible-heading 1)
            (org-cut-subtree))
        (org-next-visible-heading 1)))))

(defun timfel/determine-recent-project-root ()
  "Examine the recent buffers to find the most recent one that was under a
VC root"
  (if-let* ((buffers-root-dirs (seq-keep (lambda (buffer)
                                           (if-let* ((path (buffer-file-name buffer))
                                                     (backend (vc-backend path))
                                                     (dir (file-name-parent-directory path))
                                                     (root (vc-call-backend backend 'root dir)))
                                               root))
                                         (buffer-list)))
            (vc-root (seq-first buffers-root-dirs))
            (root (expand-file-name vc-root)))
      root))

(defun timfel/my/lsp/find-eclipse-projects-recursively (directory)
  (let ((current-directory-list (directory-files directory)))
    (seq-concatenate 'list
     (if (seq-some (lambda (elt) (string-equal ".project" elt)) current-directory-list)
         (list directory)
       '())
      (seq-mapcat (lambda (elt) (timfel/my/lsp/find-eclipse-projects-recursively (concat (file-name-as-directory directory) elt)))
                  (seq-filter (lambda (elt) (and (file-directory-p (concat (file-name-as-directory directory) elt))
                                                 (not (f-symlink-p (concat (file-name-as-directory directory) elt)))
                                                 (not (string-prefix-p "." elt))
                                                 (not (string-prefix-p "mxbuild" elt)))) current-directory-list)))))

(defun timfel/my/lsp/reload-all-java-buffers ()
  (interactive)
  (let ((list (buffer-list)))
    (dolist (buffer list)
      (let ((name (buffer-name buffer))
            (mode (with-current-buffer buffer major-mode)))
        (if (eq mode 'java-mode)
            (if (not (buffer-modified-p buffer))
                (with-current-buffer buffer
                  (if (funcall buffer-stale-function)
                      (progn
                        (message "Reverting %s" (buffer-name))
                        (revert-buffer :ignore-auto :noconfirm))))))))))

(defun timfel/my/lsp/kill-old-java-buffers ()
  (interactive)
  (let ((list (buffer-list))
        (recent-cnt 0))
    (dolist (buffer list)
      (let ((name (buffer-name buffer))
            (mode (with-current-buffer buffer major-mode)))
        (if (eq mode 'java-mode)
            (progn
              (setq recent-cnt (+ 1 recent-cnt))
              (if (and (not (buffer-modified-p buffer))
                       (> recent-cnt 5))
                  (kill-buffer buffer))))))))

(defun timfel/my/lsp/kill-all-java-buffers ()
  (interactive)
  (let ((list (buffer-list)))
    (dolist (buffer list)
      (let ((name (buffer-name buffer))
            (mode (with-current-buffer buffer major-mode)))
        (if (eq mode 'java-mode)
            (if (and (not (buffer-modified-p buffer))
                     (not (eq (current-buffer) buffer)))
                (kill-buffer buffer)))))))

(defun timfel/my/lsp/clear-workspace ()
  (interactive)
  (seq-do (lambda (elt)
            (lsp-workspace-folders-remove elt))
          (lsp-session-folders (lsp-session)))
  (puthash 'jdtls
           '()
           (lsp-session-server-id->folders (lsp-session)))
  (lsp--persist-session (lsp-session)))

(defun timfel/my/lsp/import-eclipse-projects ()
  (interactive)
  (let* ((base-dir (read-directory-name "Base directory to search projects in: "))
         (base-dirs (completing-read-multiple "Base sub-directories to search projects in: " (directory-files base-dir) nil t))
         (projects-found (seq-mapcat (lambda (elt) (timfel/my/lsp/find-eclipse-projects-recursively (concat (file-name-as-directory base-dir) elt))) base-dirs))
         (projects-to-import (completing-read-multiple "Select projects to import (comma-sep, * for all): " projects-found nil nil))
         (additional-required-projects '())
         (go-again t))
    (require 'xml)
    (if (equal projects-to-import '("*"))
        (setq projects-to-import projects-found))
    (setq projects-to-import
          (mapcan (lambda (elt)
                    (if (string-suffix-p "*" elt)
                        (seq-filter (lambda (elt2) (string-prefix-p (substring elt 0 (- (length elt) 1)) elt2)) projects-found)
                      (list elt)))
                  projects-to-import))
    (while go-again
      (setq go-again (length projects-to-import))
      (seq-do (lambda (elt)
                (progn
                  ;; find the required projects for each selected project
                  (seq-do
                   (lambda (elt)
                     (add-to-list 'additional-required-projects (car (xml-node-children elt))))
                   (xml-get-children
                    (car
                     (xml-get-children
                      (assq 'projectDescription (xml-parse-file (concat (file-name-as-directory elt) ".project")))
                      'projects))
                    'project))
                  ;; find JAR projects on the factorypath that are part of a
                  ;; workspace project no go through
                  (if (file-exists-p (concat (file-name-as-directory elt) ".factorypath"))
                      (seq-do (lambda (elt)
                                (if (string-equal (xml-get-attribute-or-nil elt 'kind) "WKSPJAR")
                                    (add-to-list 'additional-required-projects (cadr (split-string (xml-get-attribute elt 'id) "/")))))
                              (xml-get-children
                               (assq 'factorypath (xml-parse-file (concat (file-name-as-directory elt) ".factorypath")))
                               'factorypathentry)))))
              projects-to-import)

      ;; resolve dependencies
      (seq-do (lambda (elt)
                (let ((name (car (xml-node-children (car (xml-get-children
                                                          (assq 'projectDescription (xml-parse-file (concat (file-name-as-directory elt) ".project")))
                                                          'name))))))
                  (if (seq-contains-p additional-required-projects name)
                      (add-to-list 'projects-to-import elt))))
              projects-found)
      ;; if we added projects to the list of projects to import, go deeper
      (setq go-again (> (length projects-to-import) go-again)))

    ;; expand-file-name and remove duplicates from projects-to-import
    (setq projects-to-import (seq-uniq (seq-map #'expand-file-name projects-to-import)))

    ;; add projects to session
    (dolist (exp projects-to-import)
      (if (not (seq-contains-p (lsp-session-folders (lsp-session)) exp))
          (progn
            (lsp-workspace-folders-add exp)
            (puthash 'jdtls
                     (append (gethash 'jdtls
                                      (lsp-session-server-id->folders (lsp-session)))
                             (list exp))
                     (lsp-session-server-id->folders (lsp-session)))
            )))
    (lsp--persist-session (lsp-session))
    (seq-do (lambda (elt) (message (format "Imported '%s'" elt))) projects-to-import)))

(defun timfel/my/lsp/rebuild-java ()
  (interactive)
  (timfel/my/lsp/reload-all-java-buffers)
  (lsp-send-notification
   (lsp-make-request "java/buildWorkspace" t)))

(defun timfel/update-windows-terminal-settings-json ()
    "Find the Windows-side Windows terminal settings.json which is under
%LOCALAPPDATA%\\Packages\\Microsoft.WindowsTerminal_8wekyb3d8bbwe\\
LocalState\\settings.json
(finding that PATH on the Windows side if we are running in WSL), remove
the actions json key and value, and insert the actions json key and
value that is generated by term-keys/windows-terminal-json"
    (interactive)
    (require 'term-keys-windows-terminal)
    (with-temp-buffer
      (insert (term-keys/windows-terminal-json))
      (let* ((actions-json (buffer-string))
             (actions-obj (if (fboundp 'json-parse-string)
                              (json-parse-string actions-json :object-type 'hash-table :array-type 'list)
                            (with-temp-buffer
                              (insert actions-json)
                              (goto-char (point-min))
                              (json-read))))
             (actions (cond
                       ((hash-table-p actions-obj)
                        (or (gethash "actions" actions-obj) actions-obj))
                       ((listp actions-obj) actions-obj)
                       (t nil))))
        (cl-labels
            ((timfel/wsl-p ()
               (and (eq system-type 'gnu/linux)
                    (ignore-errors
                      (with-temp-buffer
                        (insert-file-contents "/proc/version")
                        (goto-char (point-min))
                        (re-search-forward "Microsoft" nil t)))))
             (timfel/trim-crlf (s)
               (when s
                 (replace-regexp-in-string "[\r\n]+\\'" "" s)))
             (timfel/shell-trim (cmd)
               (timfel/trim-crlf (shell-command-to-string cmd)))
             (timfel/wsl-localappdata ()
               (let* ((raw (timfel/shell-trim "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -NoProfile -Command \"\\$env:LOCALAPPDATA\""))
                      (u (and raw (timfel/shell-trim (format "wslpath -u '%s'" raw)))))
                 (and (stringp u) (not (string-empty-p u)) u)))
             (timfel/windows-localappdata ()
               (cond
                ((eq system-type 'windows-nt)
                 (getenv "LOCALAPPDATA"))
                ((timfel/wsl-p)
                 (timfel/wsl-localappdata))
                (t nil)))
             (timfel/windows-terminal-settings-path ()
               (when-let* ((lad (timfel/windows-localappdata)))
                 (let ((p (expand-file-name
                           (concat (file-name-as-directory lad)
                                   "Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/settings.json"))))
                   p))))
          (when actions
            (when-let* ((settings-path (timfel/windows-terminal-settings-path)))
              (when (file-exists-p settings-path)
                (let* ((settings-obj
                        (with-temp-buffer
                          (insert-file-contents settings-path)
                          (goto-char (point-min))
                          (json-parse-buffer :object-type 'hash-table :array-type 'list))))
                  (when (hash-table-p settings-obj)
                    (puthash "actions" actions settings-obj)
                    (let ((json-encoding-pretty-print t))
                      (with-temp-file settings-path
                        (insert (json-encode settings-obj)))))))))))))

(dolist (sym (apropos-internal "^timfel/" #'fboundp))
  (let* ((old-name (symbol-name sym))
         (new-name (intern (string-remove-prefix "timfel/" old-name))))
    (if (fboundp new-name)
        (warn "Function %s already exists" new-name)
      (defalias new-name sym))))

(provide 'timfel)
