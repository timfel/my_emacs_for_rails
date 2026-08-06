;;; -*- lexical-binding: t -*-

(require 'use-package)

(use-package lsp-mode
  :defines (lsp-headerline-arrow)
  :functions (c-clear-string-fences c-restore-string-fences
              dired-get-file-for-visit lsp--line-character-to-point
              lsp-booster--advice-final-command
              lsp-booster--advice-json-parse lsp-diagnostics
              lsp-find-definition lsp-find-references
              lsp-execute-code-action lsp-ido-workspace-symbol
              my/c-clear-string-fences)
  :ensure t
  :commands (lsp)
  :bind (:map lsp-mode-map
         ("C-," . lsp-execute-code-action)
         ("M-." . lsp-find-definition)
         ("C-M-." . lsp-find-references)
         ("C-S-t" . lsp-ido-workspace-symbol))
  :hook ((lsp-mode . flymake-mode))
  :custom
  (lsp-print-io nil)
  (lsp-lens-enable t)
  (lsp-completion-enable-additional-text-edit t)
  (lsp-enable-snippet t)
  (lsp-enable-indentation nil)
  (lsp-before-save-edits t)
  (lsp-enable-file-watchers nil)
  (lsp-print-performance nil)
  (lsp-report-if-no-buffer t)
  (lsp-enable-snippet t)
  (lsp-enable-xref t)
  (lsp-diagnostics-provider :flymake)
  (lsp-completion-enable t)
  (lsp-completion-filter-on-incomplete nil)
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind nil)
  (lsp-completion-sort-initial-results nil)
  (lsp-response-timeout 30)
  (lsp-diagnostic-clean-after-change nil)
  (lsp-eldoc-render-all t)
  (lsp-eldoc-enable-hover t)
  (lsp-idle-delay 1.000)
  (lsp-tcp-connection-timeout 20)
  (lsp-modeline-diagnostics-enable t)
  (lsp-modeline-code-actions-enable nil)
  :config
  (require 'treemacs)
  (setq lsp-headerline-arrow ">")
  (defun lsp-goto-next-diagnostic ()
    "Get lsp-diagnostics, it returns a hash mapping file names to a list of
     hashes, each of which is a diagnostic. Search in the file names for the
     current buffer's file name. If found, search the list of diagnostics.
     Get the vallue for the :range key, and compare the :start of the
     resulting hash with the current point position until we find the next
     diagnostic that is after the current point. If found, set the point to
     that next diagnistic's start position. If there are no more diagnistics
     after the current point in the list, take the next file name from the
     outer hash. If the file name that got picked is not the current buffer,
     open the file and position the point at the start of the range of the
     first hash in the list of diagnistics."
    (interactive)
    (let* ((current-file (or (buffer-file-name) (dired-get-file-for-visit)))
           (diagnostics-table (lsp-diagnostics))
           (all-files (hash-table-keys diagnostics-table))
           (files-ordered (seq-sort #'string-lessp all-files))
           (point-pos (point))
           (found (gethash current-file diagnostics-table))
           (files-to-seek (seq-drop-while
                           (lambda (f) (and found (not (string-equal f current-file))))
                           files-ordered)))
      (catch 'done
        (dolist (file files-to-seek)
          (let* ((diagnostics (gethash file diagnostics-table))
                 (next-diag
                  (seq-find (lambda (diag)
                              (let ((range (plist-get diag :range))
                                    (severity (plist-get diag :severity)))
                                (when (and range (< severity 2))
                                  (let* ((start (plist-get range :start))
                                         (pos (lsp--line-character-to-point
                                               (plist-get start :line)
                                               (plist-get start :character))))
                                    (or
                                     (not (equal file current-file))
                                     (< point-pos pos))))))
                            diagnostics)))
            (when next-diag
              (let* ((range (plist-get next-diag :range))
                     (start (plist-get range :start)))
                (unless (equal file current-file)
                  (find-file file))
                (goto-char (point-min))
                (forward-line (plist-get start :line))
                (forward-char (plist-get start :character))
                (message "Jumped to diagnostic: %s (%d)" (plist-get next-diag :message) (plist-get next-diag :severity))
                (throw 'done t))))))))

  (defun lsp-goto-previous-diagnostic ()
    "Move to the previous diagnostic before point."
    (interactive)
    (let* ((current-file (or (buffer-file-name) (dired-get-file-for-visit)))
           (diagnostics-table (lsp-diagnostics))
           (all-files (hash-table-keys diagnostics-table))
           (files-ordered (seq-sort #'string-lessp all-files))
           (point-pos (point))
           (found (gethash current-file diagnostics-table))
           (files-to-seek (seq-reverse (append (seq-take-while
                                                (lambda (f) (and found (not (string-equal f current-file))))
                                                files-ordered)
                                               (list current-file)))))
      (catch 'done
        (dolist (file files-to-seek)
          (let* ((diagnostics (gethash file diagnostics-table))
                 (reversed-diags (reverse diagnostics))
                 (prev-diag
                  (seq-find (lambda (diag)
                              (let ((range (plist-get diag :range))
                                    (severity (plist-get diag :severity)))
                                (when (and range (< severity 2))
                                  (let* ((start (plist-get range :start))
                                         (pos (lsp--line-character-to-point
                                               (plist-get start :line)
                                               (plist-get start :character))))
                                    (or
                                     (not (equal file current-file))
                                     (> point-pos pos))))))
                            reversed-diags)))
            (when prev-diag
              (let* ((range (plist-get prev-diag :range))
                     (start (plist-get range :start))
                     (pos (lsp--line-character-to-point
                           (plist-get start :line)
                           (plist-get start :character))))
                (unless (equal file current-file)
                  (find-file file))
                (goto-char pos)
                (message "Jumped to diagnostic: %s" (plist-get prev-diag :message))
                (throw 'done t))))))))

  (defun my/c-clear-string-fences (orig-fun)
    (condition-case nil
        (funcall orig-fun)
      (error
       nil)))
  (advice-add #'c-clear-string-fences :around #'my/c-clear-string-fences)
  (advice-add #'c-restore-string-fences :around #'my/c-clear-string-fences)

  (if (executable-find "emacs-lsp-booster")
      (progn
        (defun lsp-booster--advice-json-parse (old-fn &rest args)
          "Try to parse bytecode instead of json."
          (or
           (when (equal (following-char) ?#)
             (let ((bytecode (read (current-buffer))))
               (when (byte-code-function-p bytecode)
                 (funcall bytecode))))
           (apply old-fn args)))
        (advice-add (if (progn (require 'json)
                               (fboundp 'json-parse-buffer))
                        'json-parse-buffer
                      'json-read)
                    :around
                    #'lsp-booster--advice-json-parse)

        (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
          "Prepend emacs-lsp-booster command to lsp CMD."
          (let ((orig-result (funcall old-fn cmd test?)))
            (if (and (not test?)                             ;; for check lsp-server-present?
                     (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                     lsp-use-plists
                     (not (functionp 'json-rpc-connection))  ;; native json-rpc
                     (executable-find "emacs-lsp-booster"))
                (progn
                  (when-let* ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
                    (setcar orig-result command-from-exec-path))
                  (message "Using emacs-lsp-booster for %s!" orig-result)
                  (cons "emacs-lsp-booster" orig-result))
              orig-result)))
        (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))))

(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :demand t
  :functions (determine-recent-project-root lsp-treemacs--build-error-list)
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
         ("C-c e" . lsp-treemacs-errors-list))
  :custom
  (lsp-treemacs-error-list-current-project-only t)
  (lsp-treemacs-error-list-expand-depth nil)
  (lsp-treemacs-error-list-severity 1)
  :config
  (cl-flet*
      ((error-list-advice (oldfun &rest args)
         "Show only diagnostics for the current folder if there are any."

         (if-let* ((root (determine-recent-project-root))
                   (folder-arg (seq-elt args 0))
                   (folder (expand-file-name folder-arg)))
             (when (string-prefix-p root folder)
               (apply oldfun args))
           (apply oldfun args))))

    (advice-add #'lsp-treemacs--build-error-list
                :around
                #'error-list-advice)))

(use-package lsp-java
  :ensure t
  :functions (-flatten -map -uniq dap-register-debug-template
              lsp-find-session-folder lsp-find-workspace lsp-session
              lsp-workspace-shutdown my/lsp-find-session-folder-with-mx
              my/setup-java-workspace-dir)
  :after (lsp-mode treemacs)
  :demand t
  :mode ("\\.java.*\\.class" . java-mode)
  :custom
  (lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/snapshots/jdt-language-server-latest.tar.gz")
  (lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true"))
  (lsp-java-content-provider-preferred "fernflower")
  (lsp-java-save-actions-organize-imports t)
  (lsp-java-format-on-type-enabled nil)
  (lsp-java-format-comments-enabled t)
  (lsp-java-format-enabled t)
  (lsp-java-autobuild-enabled nil)
  (lsp-java-inhibit-message t)
  (lsp-java-import-gradle-enabled nil)
  (lsp-java-completion-import-order ["java" "javax" "org" "com"])
  (lsp-java-import-order ["java" "javax" "org" "com"])
  (dap-java-default-debug-port 8000)
  :config
  (defun my/lsp-find-session-folder-with-mx (oldfun session file-name)
    (or (funcall oldfun session file-name)
        (funcall oldfun session
                 (replace-regexp-in-string
                  "/mxbuild/\\(jdk[0-9]+/\\)?" "/"
                  file-name))))
  (advice-add #'lsp-find-session-folder :around #'my/lsp-find-session-folder-with-mx)

  (dap-register-debug-template "Java Attach com.oracle.graal.python"
                               (list :type "java"
                                     :request "attach"
                                     :hostName "localhost"
                                     :projectName "com.oracle.graal.python"
                                     :port 8000))

  (defun my/setup-java-workspace-dir ()
    (unless (lsp-find-workspace 'jdtls nil)
      (if-let* ((p (project-current))
                (r (project-root p))
                (name (string-trim
                       (replace-regexp-in-string
                        "[^[:alnum:]]+" "-"
                        (directory-file-name (expand-file-name r)))
                       "-+"
                       "-+"))
                (wsuserdir
                 (expand-file-name
                  (if (string= name "") "root" name)
                  (expand-file-name "jdtls.workspaces/" "~/.cache/"))))
          (unless (equal lsp-java-workspace-dir wsuserdir)
            (->> (lsp-session)
                 (lsp-session-folder->servers)
                 (hash-table-values)
                 (-flatten)
                 (-uniq)
                 (mapc #'lsp-workspace-shutdown))
            (setq lsp--session nil)
            (require 'desktop)
            (defvar desktop-globals-to-save)
            (add-to-list 'desktop-globals-to-save 'lsp-java-workspace-dir)
            (add-to-list 'desktop-globals-to-save 'lsp-java-workspace-cache-dir)
            (add-to-list 'desktop-globals-to-save 'lsp-session-file)
            (setq
             lsp-java-workspace-dir wsuserdir
             lsp-java-workspace-cache-dir (expand-file-name "cache/" lsp-java-workspace-dir)
             lsp-session-file (expand-file-name ".lsp-session-v1" wsuserdir))
            (message (format "Setting Eclipse workspace to %s, session to %s" lsp-java-workspace-dir lsp-session-file))
            (message (format "You may have to adapt %s/.metadata/.plugins/org.eclipse.core.runtime/.settings/org.eclipse.jdt.launching.prefs to give the default VM the name that mx told you" lsp-java-workspace-dir))
            (find-file-noselect (format "%s/.metadata/.plugins/org.eclipse.core.runtime/.settings/org.eclipse.jdt.launching.prefs" lsp-java-workspace-dir))))))

  (add-hook 'java-mode-hook
            (lambda ()
              (my/setup-java-workspace-dir)
              (if (lsp-find-workspace 'jdtls nil) (lsp)))))

(use-package dap-mode
  :ensure t
  :after lsp
  :demand t
  :commands (dap-debug)
  :config
  (dap-auto-configure-mode t)
  :custom
  (dap-stack-trace-limit 40)
  (dap-auto-configure-features '(sessions locals tooltip))
  (dap-ui-locals-expand-depth nil)
  (dap-print-io nil)
  (dap-auto-show-output t))

(use-package dap-node
  :after dap-mode
  :demand t
  :config (dap-register-debug-template
           "Node Attach 9229"
           (list :type "node"
                 :cwd nil
                 :request "attach"
                 :protocol "auto"
                 :address "127.0.0.1"
                 :stopOnEntry t
                 :port 9229
                 :program ""
                 :name "Node Attach 9229")))

(use-package timfel-lsp-java-extensions
  :commands (timfel/my/lsp/find-eclipse-projects-recursively
             timfel/my/lsp/reload-all-java-buffers
             timfel/my/lsp/kill-old-java-buffers
             timfel/my/lsp/kill-all-java-buffers
             timfel/my/lsp/clear-workspace
             timfel/my/lsp/import-eclipse-projects
             timfel/my/lsp/rebuild-java)
  :after (timfel lsp)
  :demand t)
