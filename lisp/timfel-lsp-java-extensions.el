;;; timfel-lsp-java-extensions.el --- LSP Java helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Local helpers for Java/JDTLS workspace management.

;;; Code:

(require 'lsp-mode)
(require 'xml)

;;;###autoload
(defun timfel/my/lsp/find-eclipse-projects-recursively (directory)
  "Recursively find Eclipse projects under DIRECTORY."
  (let ((current-directory-list (directory-files directory)))
    (seq-concatenate 'list
                     (if (seq-some (lambda (elt) (string-equal ".project" elt))
                                   current-directory-list)
                         (list directory)
                       '())
                     (seq-mapcat
                      (lambda (elt)
                        (timfel/my/lsp/find-eclipse-projects-recursively
                         (concat (file-name-as-directory directory) elt)))
                      (seq-filter
                       (lambda (elt)
                         (and (file-directory-p (concat (file-name-as-directory directory) elt))
                              (not (file-symlink-p (concat (file-name-as-directory directory) elt)))
                              (not (string-prefix-p "." elt))
                              (not (string-prefix-p "mxbuild" elt))))
                       current-directory-list)))))

;;;###autoload
(defun timfel/my/lsp/reload-all-java-buffers ()
  "Reload all stale unmodified Java buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((mode (with-current-buffer buffer major-mode)))
      (when (and (eq mode 'java-mode)
                 (not (buffer-modified-p buffer)))
        (with-current-buffer buffer
          (when (funcall buffer-stale-function)
            (message "Reverting %s" (buffer-name))
            (revert-buffer :ignore-auto :noconfirm)))))))

;;;###autoload
(defun timfel/my/lsp/kill-old-java-buffers ()
  "Kill all but the 5 most recent unmodified Java buffers."
  (interactive)
  (let ((recent-cnt 0))
    (dolist (buffer (buffer-list))
      (let ((mode (with-current-buffer buffer major-mode)))
        (when (eq mode 'java-mode)
          (setq recent-cnt (1+ recent-cnt))
          (when (and (not (buffer-modified-p buffer))
                     (> recent-cnt 5))
            (kill-buffer buffer)))))))

;;;###autoload
(defun timfel/my/lsp/kill-all-java-buffers ()
  "Kill all unmodified Java buffers except the current buffer."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((mode (with-current-buffer buffer major-mode)))
      (when (and (eq mode 'java-mode)
                 (not (buffer-modified-p buffer))
                 (not (eq (current-buffer) buffer)))
        (kill-buffer buffer)))))

;;;###autoload
(defun timfel/my/lsp/clear-workspace ()
  "Remove all folders from the current LSP workspace session."
  (interactive)
  (seq-do (lambda (elt)
            (lsp-workspace-folders-remove elt))
          (lsp-session-folders (lsp-session)))
  (puthash 'jdtls
           '()
           (lsp-session-server-id->folders (lsp-session)))
  (lsp--persist-session (lsp-session)))

;;;###autoload
(defun timfel/my/lsp/import-eclipse-projects ()
  "Find and import Eclipse projects into the current JDTLS workspace."
  (interactive)
  (let* ((base-dir (read-directory-name "Base directory to search projects in: "))
         (base-dirs (completing-read-multiple
                     "Base sub-directories to search projects in: "
                     (directory-files base-dir) nil t))
         (projects-found
          (seq-mapcat
           (lambda (elt)
             (timfel/my/lsp/find-eclipse-projects-recursively
              (concat (file-name-as-directory base-dir) elt)))
           base-dirs))
         (projects-to-import
          (completing-read-multiple
           "Select projects to import (comma-sep, * for all): "
           projects-found nil nil))
         (additional-required-projects '())
         (go-again t))
    (when (equal projects-to-import '("*"))
      (setq projects-to-import projects-found))
    (setq projects-to-import
          (mapcan (lambda (elt)
                    (if (string-suffix-p "*" elt)
                        (seq-filter
                         (lambda (elt2)
                           (string-prefix-p (substring elt 0 (1- (length elt))) elt2))
                         projects-found)
                      (list elt)))
                  projects-to-import))
    (while go-again
      (setq go-again (length projects-to-import))
      (seq-do
       (lambda (elt)
         (seq-do
          (lambda (project)
            (add-to-list 'additional-required-projects
                         (car (xml-node-children project))))
          (xml-get-children
           (car
            (xml-get-children
             (assq 'projectDescription
                   (xml-parse-file (concat (file-name-as-directory elt) ".project")))
             'projects))
           'project))
         (when (file-exists-p (concat (file-name-as-directory elt) ".factorypath"))
           (seq-do
            (lambda (factory-entry)
              (when (string-equal (xml-get-attribute-or-nil factory-entry 'kind) "WKSPJAR")
                (add-to-list 'additional-required-projects
                             (cadr (split-string (xml-get-attribute factory-entry 'id) "/")))))
            (xml-get-children
             (assq 'factorypath
                   (xml-parse-file (concat (file-name-as-directory elt) ".factorypath")))
             'factorypathentry))))
       projects-to-import)
      (seq-do
       (lambda (elt)
         (let ((name (car (xml-node-children
                           (car (xml-get-children
                                 (assq 'projectDescription
                                       (xml-parse-file (concat (file-name-as-directory elt) ".project")))
                                 'name))))))
           (when (seq-contains-p additional-required-projects name)
             (add-to-list 'projects-to-import elt))))
       projects-found)
      (setq go-again (> (length projects-to-import) go-again)))
    (setq projects-to-import (seq-uniq (seq-map #'expand-file-name projects-to-import)))
    (dolist (exp projects-to-import)
      (unless (seq-contains-p (lsp-session-folders (lsp-session)) exp)
        (lsp-workspace-folders-add exp)
        (puthash 'jdtls
                 (append (gethash 'jdtls
                                  (lsp-session-server-id->folders (lsp-session)))
                         (list exp))
                 (lsp-session-server-id->folders (lsp-session)))))
    (lsp--persist-session (lsp-session))
    (seq-do (lambda (elt)
              (message "Imported '%s'" elt))
            projects-to-import)))

;;;###autoload
(defun timfel/my/lsp/rebuild-java ()
  "Reload Java buffers and request a JDTLS workspace build."
  (interactive)
  (timfel/my/lsp/reload-all-java-buffers)
  (lsp-send-notification
   (lsp-make-request "java/buildWorkspace" t)))

(provide 'timfel-lsp-java-extensions)

;;; timfel-lsp-java-extensions.el ends here
