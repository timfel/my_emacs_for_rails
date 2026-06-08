;;; eglot-jdtls.el --- Eglot Java helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Minimal Eglot/JDTLS integration.  JDTLS is expected to be available as
;; `eglot-jdtls-command' in PATH; this file only supplies per-project
;; workspace/cache directories and initialization options.

;;; Code:

(require 'project)
(require 'subr-x)
(require 'eglot)

(defgroup eglot-jdtls nil
  "Small Java helpers for Eglot."
  :group 'eglot)

(defcustom eglot-jdtls-command "jdtls"
  "JDTLS executable to run.
The default uses `jdtls' from PATH.  For TRAMP projects, Eglot starts the
process through TRAMP, so this should also be available in the remote PATH."
  :type 'string
  :group 'eglot-jdtls)

(defcustom eglot-jdtls-initialization-options
  '(:settings
    (:java (:format (:onType (:enabled t)
                    :comments (:enabled t)
                    :enabled t)
            :completion (:guessMethodArguments t
                         :overwrite t
                         :enabled t)
            :import (:maven (:enabled t)
                     :gradle (:enabled t))
            :autobuild (:enabled t)
            :saveActions (:organizeImports t)
            :jdt (:ls (:javac (:enabled t)))
            :project (:importOnFirstTimeStartup t))))
  "Initialization options passed to JDTLS."
  :type 'sexp
  :group 'eglot-jdtls)

(defun eglot-jdtls--project-id (project-root)
  "Return a stable directory name for PROJECT-ROOT."
  (let* ((root (directory-file-name (expand-file-name project-root)))
         (name (file-name-nondirectory (file-local-name root)))
         (safe-name (string-trim
                     (replace-regexp-in-string "[^[:alnum:]._-]+" "-" name)
                     "-+"
                     "-+"))
         (hash (substring (secure-hash 'sha1 root) 0 12)))
    (format "%s-%s" (if (string-empty-p safe-name) "project" safe-name) hash)))

(defun eglot-jdtls--state-base (project-root)
  "Return the JDTLS state base directory for PROJECT-ROOT."
  (if-let ((remote (file-remote-p project-root)))
      (let ((remote-dir (format "/tmp/emacs-jdtls-%s/"
                                (replace-regexp-in-string "[^[:alnum:]._-]+" "-"
                                                          (user-login-name)))))
        (concat remote (file-name-as-directory remote-dir)))
    (file-name-as-directory (expand-file-name (locate-user-emacs-file ".cache/eglot.jdtls")))))

(defun eglot-jdtls--project-dir (project-root)
  "Return the JDTLS state directory for PROJECT-ROOT."
  (expand-file-name (eglot-jdtls--project-id project-root)
                    (eglot-jdtls--state-base project-root)))

(defun eglot-jdtls--local-argument (file-name)
  "Return FILE-NAME as a path suitable for the JDTLS process."
  (file-local-name (expand-file-name file-name)))

(defun eglot-jdtls--workspace-roots (project)
  "Return root directories to advertise as workspace folders for PROJECT."
  (let (roots)
    (dolist (root (cons (project-root project)
                        (project-external-roots project))
                  (nreverse roots))
      (when (stringp root)
        (let ((root (file-name-as-directory (expand-file-name root))))
          (unless (member root roots)
            (push root roots)))))))

(defun eglot-jdtls--initialization-options (project)
  "Return JDTLS initialization options for PROJECT."
  (let ((options (copy-tree eglot-jdtls-initialization-options)))
    (plist-put options
               :workspaceFolders
               (vconcat
                (mapcar #'eglot-path-to-uri
                        (eglot-jdtls--workspace-roots project))))
    options))

;;;###autoload
(defun eglot-jdtls (_interactive project)
  "Return an Eglot contact for JDTLS in PROJECT."
  (let* ((project-root (project-root project))
         (project-dir (eglot-jdtls--project-dir project-root))
         (cache-dir (expand-file-name "cache/" project-dir))
         (workspace-dir (expand-file-name "workspace/" project-dir)))
    (make-directory cache-dir t)
    (make-directory workspace-dir t)
    (append
     (list eglot-jdtls-command
           "-configuration" (eglot-jdtls--local-argument cache-dir)
           "-data" (eglot-jdtls--local-argument workspace-dir))
     (list :initializationOptions
           (eglot-jdtls--initialization-options project)))))

(provide 'eglot-jdtls)

;;; eglot-jdtls.el ends here
