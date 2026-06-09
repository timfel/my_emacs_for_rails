;;; eglot-jdb.el --- Eglot Java JDB helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Minimal eglot/jdb integration.

;;; Code:

(require 'gud)
(require 'eglot)

(defun eglot-jdb--normalize-path (path)
  "Normalize a PATH returned by JDTLS."
  (cond
   ((not (stringp path)) nil)
   ((string-prefix-p "file://" path)
    (eglot-uri-to-path path))
   (t path)))

(defun eglot-jdb--normalize-path-list (paths)
  "Normalize PATHS returned by JDTLS into a deduplicated list."
  (delete-dups
   (delq nil
         (mapcar #'eglot-jdb--normalize-path (append paths nil)))))

(defun eglot-jdb--project-paths ()
  "Return classpath and sourcepath data for the current Java project."
  (if-let* ((n buffer-file-name)
            (uri (eglot-path-to-uri n))
            (server (eglot-current-server))
            (classpath-reply (eglot-execute server (list :command
                                                         "java.project.getClasspaths"
                                                         :arguments
                                                         (vector uri "{\"scope\":\"runtime\"}"))))
            (sourcepath-reply (eglot-execute server '(:command "java.project.listSourcePaths")))
            (classpath-list
             (eglot-jdb--normalize-path-list
              (append (plist-get classpath-reply :classpaths)
                      (plist-get classpath-reply :modulepaths)
                      nil)))
            (sourcepath-list
             (eglot-jdb--normalize-path-list
              (mapcar (lambda (entry) (plist-get entry :path))
                      (append (plist-get sourcepath-reply :data) nil)))))
      (list :classpath-list classpath-list
            :sourcepath-list sourcepath-list
            :classpath (mapconcat #'identity classpath-list path-separator)
            :sourcepath (mapconcat #'identity sourcepath-list path-separator))))

(defun eglot-jdb--java-debug-address (args)
  "Extract a JDWP attach address from Java process ARGS."
  (when (and (stringp args)
             (or (string-match "-agentlib:jdwp=[^[:space:]]*address=\\([^,[:space:]]+\\)" args)
                 (string-match "--vm.agentlib:jdwp=[^[:space:]]*address=\\([^,[:space:]]+\\)" args)
                 (string-match "-Xrunjdwp:[^[:space:]]*address=\\([^,[:space:]]+\\)" args)))
    (let ((address (match-string 1 args)))
      (cond
       ((string-prefix-p "*:" address)
        (concat "localhost:" (substring address 2)))
       ((string-prefix-p "0.0.0.0:" address)
        (concat "localhost:" (substring address 8)))
       (t address)))))

(defun eglot-jdb--java-process-candidates ()
  "Return running Java processes that expose a JDWP attach address."
  (let (candidates)
    (dolist (pid (list-system-processes))
      (let* ((attrs (process-attributes pid))
             (comm (alist-get 'comm attrs))
             (args (alist-get 'args attrs))
             (address (eglot-jdb--java-debug-address args)))
        (when address
          (push (cons (format "%s [%s] %s"
                              address pid
                              (truncate-string-to-width
                               (or args comm "java") 120 nil nil t))
                      address)
                candidates))))
    (nreverse candidates)))

(defun eglot-jdb--read-attach-address ()
  "Prompt for a JDWP attach address from running Java processes."
  (let* ((candidates (eglot-jdb--java-process-candidates))
         (launch-choice "[Start jdb without attach]")
         (choice
          (completing-read
           "JDB target: "
           (cons launch-choice (mapcar #'car candidates))
           nil t nil nil
           (if candidates (caar candidates) launch-choice))))
    (unless (equal choice launch-choice)
      (cdr (assoc choice candidates)))))

(defun eglot-jdb--command-line (paths attach-address extra-args)
  "Build a jdb command line from PATHS, ATTACH-ADDRESS and EXTRA-ARGS."
  (let ((extra-args (or extra-args "")))
    (combine-and-quote-strings
     (append
      (list (or (and (boundp 'gud-jdb-command-name) gud-jdb-command-name)
                "jdb"))
      (if attach-address
          (list "-attach" attach-address)
        (append
         (when-let ((classpath (plist-get paths :classpath)))
           (unless (string-empty-p classpath)
             (list "-classpath" classpath)))
         (when-let ((sourcepath (plist-get paths :sourcepath)))
           (unless (string-empty-p sourcepath)
             (list "-sourcepath" sourcepath)))))
      (unless (string-empty-p extra-args)
        (split-string-and-unquote extra-args))))))

;;;###autoload
(defun eglot-jdb ()
  "Run GUD jdb using classpaths and sourcepaths from the current Eglot project."
  (interactive)
  (require 'gud)
  (unless (derived-mode-p 'java-mode 'java-ts-mode)
    (user-error "Run `eglot-jdb' from a Java buffer"))
  (let* ((paths (eglot-jdb--project-paths))
         (attach-address (eglot-jdb--read-attach-address))
         (extra-args (read-from-minibuffer
                      (if attach-address
                          "Additional jdb arguments (optional): "
                        "Class or additional jdb arguments (optional): ")))
         (command-line (eglot-jdb--command-line paths attach-address extra-args)))
    (jdb command-line)
    (when (buffer-live-p gud-comint-buffer)
      (with-current-buffer gud-comint-buffer
        (setq-local gud-jdb-use-classpath t
                    gud-jdb-classpath (plist-get paths :classpath-list)
                    gud-jdb-sourcepath (plist-get paths :sourcepath-list))))))

(provide 'eglot-jdb)

;;; eglot-jdb.el ends here
