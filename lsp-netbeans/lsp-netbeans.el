;;; lsp-netbeans.el --- Apache Netbeans Language Server Client settings  -*- lexical-binding: t; -*-

;; Author: Tim Felgentreff <timfelgentreff@gmail.com>
;; Package-Requires: ((emacs "25.1") (lsp-mode "6.0") (markdown-mode "2.3") (dash "2.18.0") (f "0.20.0") (ht "2.0") (request "0.3.0") (treemacs "2.5") (dap-mode "0.5"))
;; Keywords: languague tools java groovy graalvm lsp
;; URL: https://github.com/timfel/lsp-netbeans

;; Copyright (C) 2021  Tim Felgentreff
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; LSP client for the Apache Netbeans LSP server.

;; Currently missing many features that are in the VSCode extension, including, but not limited to:
;; - All custom commands, such as
;;     java.build.workspace
;;     java.generate.getters
;;     java.generate.setters
;;     java.generate.getters.setters
;;     java.generate.implementMethod
;;     java.generate.overrideMethod
;;     java.generate.equals.hashCode
;;     java.generate.delegateMethod
;;     java.generate.constructor
;;     java.generate.logger
;;     java.generate.toString
;;     graalvm.pause.script
;;     java.super.implementation
;; - Groovy support
;; - Managing/running/debugging unittest directly
;; - GraalVM Native Image launch/debug configuration
;; - Attach to JVM process debug configs
;; - Launch JVM debug config

;;; Code:

(require 'lsp-mode)

(defcustom lsp-netbeans-download-url "https://ci-builds.apache.org/job/Netbeans/job/netbeans-vscode/lastSuccessfulBuild/artifact/java/java.lsp.server/build/*zip*/build.zip"
  "URL to download the Apache Netbeans LSP server from"
  :group 'lsp-netbeans
  :type 'string)

(defcustom lsp-netbeans-jdk nil
  "JDK home to run netbeans"
  :group 'lsp-netbeans
  :type 'directory)

(defcustom lsp-netbeans-busy-spinner t
  "Show a busy indicator when the lsp server process is consuming lots of CPU"
  :group 'lsp-netbeans
  :type 'boolean)

(defvar lsp-netbeans--busy-spinner-spinner nil)

(defcustom lsp-netbeans-install-dir (f-join lsp-server-install-dir "asf.apache-netbeans-java")
  "Apache Netbeans language server installation dir"
  :group 'lsp-netbeans
  :type 'string)

(defvar lsp-netbeans-user-dir nil
  "Apache Netbeans language server userdir")

(defcustom-lsp lsp-netbeans-javadoc-load-timeout 10
  "Load timeout for docs."
  :type 'number
  :group 'lsp-netbeans
  :package-version '(lsp-mode . "6.2")
  :lsp-path "netbeans.javadoc.load.timeout")

(defcustom-lsp lsp-netbeans-onSave-organizeImports (lsp-json-bool nil)
  "Enable organize imports action on a document save"
  :type '(choice (const :json-false)
                 (const t))
  :group 'lsp-netbeans
  :package-version '(lsp-mode . "6.2")
  :lsp-path "netbeans.java.onSave.organizeImports")

(defcustom-lsp lsp-netbeans-import-groups '("java" "javax" "org" "com" "")
  "Groups of import statements (specified by their package prefixes) and their sorting order. Import statements within a group are ordered alphabetically"
  :type 'vector
  :group 'lsp-netbeans
  :package-version '(lsp-mode . "6.2")
  :lsp-path "netbeans.java.imports.groups")

(defcustom-lsp lsp-netbeans-groovySupport-enabled nil
  "Enables experimental Groovy and Spock support in Language Server"
  :type 'boolean
  :group 'lsp-netbeans
  :package-version '(lsp-mode . "6.2")
  :lsp-path "netbeans.groovySupport.enabled")

(defcustom-lsp lsp-netbeans-format-settingsPath ""
  "Path to the file containing exported formatter settings"
  :type 'file
  :group 'lsp-netbeans
  :package-version '(lsp-mode . "6.2")
  :lsp-path "netbeans.format.settingsPath")

(defun lsp-netbeans-server-command (main-port)
  (let ((cmd (list (f-join lsp-netbeans-install-dir "run.sh"))))
    (if (not (string-empty-p lsp-netbeans-jdk))
        (progn
          (add-to-list 'cmd "--jdkhome" t)
          (add-to-list 'cmd  lsp-netbeans-jdk t)))
    (if lsp-log-io
        (add-to-list 'cmd "-J-Dnetbeans.logger.console=true" t))
    (add-to-list 'cmd "--start-java-debug-adapter-server=listen:0" t)
    (add-to-list 'cmd (format "--start-java-language-server=listen:%d" main-port) t)
    (add-to-list 'cmd "--userdir" t)
    (add-to-list 'cmd
                 (if lsp-netbeans-user-dir
                     lsp-netbeans-user-dir
                   (f-join lsp-server-install-dir "asf.apache-netbeans-java.userdir"))
                 t)
    cmd))

(defun lsp-netbeans--install-server (_client callback error-callback update?)
  (let* ((backup-dir (concat lsp-netbeans-install-dir "-backup-" (format-time-string "%d-%m-%Y"))))
    (if (or update?
            (and (file-exists-p lsp-netbeans-install-dir)
                 (not (file-exists-p backup-dir)))
            (not (file-exists-p lsp-netbeans-install-dir)))
        (progn
          (if (file-exists-p lsp-netbeans-install-dir)
              (progn
                (if (file-exists-p backup-dir)
                    (delete-directory backup-dir t))
                (f-move lsp-netbeans-install-dir backup-dir)))
          (delete-directory lsp-netbeans-install-dir t)
          (make-directory lsp-netbeans-install-dir t)
          (let* ((download-path (f-join lsp-netbeans-install-dir "vspackage.zip")))
            (lsp-async-start-process
             (lambda ()
               (message "Unzipping %s" download-path)
               (lsp-unzip
                download-path
                lsp-netbeans-install-dir)
               (let ((vsix (car (directory-files-recursively lsp-netbeans-install-dir ".*\\.vsix"))))
                 (message "Unzipping %s" vsix)
                 (lsp-unzip
                  vsix
                  lsp-netbeans-install-dir))
               (let ((run-script-path (f-join lsp-netbeans-install-dir "run.sh")))
                 (with-temp-file run-script-path
                   (insert "#!/bin/bash
                      cd `dirname $0`
                      cd extension
                      node out/nbcode.js $@ >&2"))
                 (shell-command (concat "chmod u+x " run-script-path)))
               (message "Done downloading Netbeans LSP server")
               (funcall callback))
             error-callback
             "sh" "-c" (format "curl -L -C - --output %s %s" download-path lsp-netbeans-download-url)))))))

(lsp-interface (netbeans:ShowQuickPickParams (:placeHolder :canPickMany :items) nil)
               (netbeans:QuickPickItem (:label) (:detail :description :picked :userData))
               (netbeans:ShowInputBoxParams (:prompt) (:value))
               (netbeans:Tests (:file :name :range :tests) nil)
               (netbeans:Test (:id :file :name :range) nil))

(defun lsp-netbeans--pick-multiple (title items)
  (require 'widget)
  (let ((finished nil)
        (name "*Pick Items*")
        (selection '()))
    (with-current-buffer-window name name (lambda (window _result) (select-window window t))
      (widget-insert title)
      (widget-insert "\n\n")
      (dolist (item items)
        (widget-create 'checkbox
                       :notify (lambda (widget &rest _ignore)
                                 (if (widget-value widget)
                                     (push item selection)
                                   (delete item selection)))
                       nil)
        (widget-insert (format " %s\n" item)))
      (widget-create 'push-button
                     :notify (lambda (&rest _ignore) (setq finished t))
                     "Accept Selection")
      (widget-insert " ")
      (widget-create 'push-button
                     :notify (lambda (&rest _ignore)
                               (setq selection (mapcar #'identity items))
                               (setq finished t))
                     "Accept All")
      (use-local-map widget-keymap))
    (while (not finished)
      (let* ((seq (read-key-sequence-vector nil))
             (binding (local-key-binding seq)))
        (if (equal seq [13])
            (widget-button-press)
          (if binding
              (call-interactively binding)
            (let ((gbinding (global-key-binding seq)))
              (if gbinding
                  (call-interactively gbinding)))))))
    (kill-buffer name)
    (message "%s" selection)
    selection))

(lsp-defun lsp-netbeans--show-quick-pick (_workspace (&netbeans:ShowQuickPickParams :place-holder :can-pick-many :items))
  (if-let* ((selectfunc (if can-pick-many #'completing-read-multiple #'completing-read))
            (itemHt (make-hash-table :test #'equal))
            (itemLabels (mapc (-lambda ((item &as &netbeans:QuickPickItem :label :detail? :description?))
                                (ht-set! itemHt (format "%s %s %s" label detail? description?) label))
                              items))
            (result (funcall-interactively
                     selectfunc
                     (format "%s%s " place-holder (if can-pick-many " (* for all)" "")) itemHt))
            (choices (if (listp result)
                         (if (equal result '("*"))
                             (ht-values itemHt)
                           (seq-map (lambda (r) (ht-get itemHt r)) result))
                       (list (ht-get itemHt result)))))
      (vconcat (seq-filter #'identity (cl-map 'list
                                              (-lambda ((item &as &netbeans:QuickPickItem :label :picked? :user-data?))
                                                (if (member label choices)
                                                    (lsp-make-netbeans-quick-pick-item :label label :picked? t :user-data? user-data?)
                                                  nil))
                                              items)))))

(defun lsp-netbeans--load-tests ()
  (if-let* ((project-root (lsp-find-session-folder (lsp-session) (buffer-file-name)))
            (tests (lsp-request
                    "workspace/executeCommand"
                    (list :command "java.load.workspace.tests"
                          :arguments (vector (format "file://%s" project-root))))))
      tests))

(lsp-defun lsp-netbeans--xref-from-file-reference (file display-text (&Range :start (start &as &Position
                                                                                           :character col
                                                                                           :line line)))
  (xref-make display-text (xref-make-file-location (lsp--uri-to-path file) line col)))

(lsp-defun lsp-netbeans-project-tests ()
  (interactive)
  (-if-let* ((rawTests (lsp-netbeans--load-tests))
             (xrefs (flatten-tree (cl-map
                                   'list
                                   (-lambda ((item &as &netbeans:Tests :name :file :range :tests))
                                     `(,(lsp-netbeans--xref-from-file-reference file name range)
                                       ,(cl-map 'list
                                                (-lambda ((item &as &netbeans:Test :id :file :range))
                                                  (lsp-netbeans--xref-from-file-reference file id range))
                                                tests)))
                                   rawTests))))
      (lsp-show-xrefs xrefs nil nil)))

(defun lsp-netbeans-source-action ()
  "Source generators."
  (interactive)
  (lsp-execute-code-action-by-kind "source"))

(defun lsp-netbeans-refactor-action ()
  "Refactorings."
  (interactive)
  (lsp-execute-code-action-by-kind "refactor"))

(defun lsp-netbeans-build-project ()
  (interactive)
  (lsp-request-async "workspace/executeCommand"
                     (list :command "java.build.workspace" :arguments '())
                     (lambda (result)
                       (setf (lsp--workspace-status-string (cl-first (lsp-workspaces))) nil)
                       (force-mode-line-update)
                       (pcase result
                         (1 (lsp--info "Successfully build project."))
                         (2 (lsp--error "Failed to build project."))))))

(defun lsp-netbeans-get-project-packages ()
  (interactive)
  (with-output-to-temp-buffer "*netbeans-packages*"
    (print (mapconcat #'identity
                      (cl-sort
                       (lsp-request "workspace/executeCommand"
                                    (list :command "java.get.project.packages"
                                          :arguments (vector (format "file://%s" buffer-file-name))))
                       'string-lessp :key 'downcase)
                      "\n"))))

(defun lsp-netbeans-resolve-project-problems ()
  (interactive)
  (with-output-to-temp-buffer "*netbeans-packages*"
    (print (lsp-request "workspace/executeCommand"
                        (list :command "java.project.resolveProjectProblems"
                              :arguments (vector (format "file://%s" buffer-file-name)))))))

(defun lsp-netbeans-clear-caches ()
  (interactive)
  (lsp-request "workspace/executeCommand" (list :command "java.clear.project.caches")))

(defun lsp-netbeans-super-impl ()
  (interactive)
  (lsp-find-locations
   "workspace/executeCommand"
   (list
    :command "java.super.implementation"
    :arguments (vector
                (format "file://%s" buffer-file-name)
                (lsp--cur-position)))))

(defun lsp-netbeans-organize-imports ()
  "Organize java imports."
  (interactive)
  (lsp-execute-code-action-by-kind "source.organizeImports"))

(defun lsp-netbeans-kill-userdir ()
  (interactive)
  (f-delete lsp-netbeans-user-dir t)
  (message "Killed %s" lsp-netbeans-user-dir))

(defun lsp-netbeans--file-sourceFor (uri)
  (run-with-timer
   1
   nil
   (lambda ()
     (with-lsp-workspace (lsp-find-workspace 'netbeans nil)
       (lsp-request "workspace/executeCommand" (list :command "java.source.for"
                                                     :arguments (vector uri))))))
  (let* ((url (url-generic-parse-url (url-unhex-string uri)))
         (path-and-query (url-path-and-query (url-generic-parse-url (url-filename url))))
         (path (car path-and-query))
         (query (cdr path-and-query))
         (fully-qualified-name (url-target url)))
    (if (string-equal "CLASS" query)
        (let* ((class-path (split-string fully-qualified-name "[\\.\\$]"))
               (try-path path)
               (loc (concat "for " fully-qualified-name)))
          (dolist (item class-path loc)
            (setq try-path (concat try-path "/" item))
            (let ((result (concat try-path ".java")))
              (if (file-exists-p result)
                  (setq loc result))))))))

(defun lsp-netbeans--resolve-with-focus (action)
  (message "what to do?"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tcp-connection 'lsp-netbeans-server-command)
  :activation-fn (lsp-activate-on "java")
  :server-id 'netbeans
  :initialization-options (list :nbcodeCapabilities
                                (list
                                 :testResultsSupport (lsp-json-bool nil)
                                 :statusBarMessageSupport (lsp-json-bool nil)
                                 :showHtmlPageSupport (lsp-json-bool nil)
                                 :wantsGroovySupport (lsp-json-bool nil)))
  :priority 10
  :multi-root t
  :uri-handlers (lsp-ht ("sourcefor" #'lsp-netbeans--file-sourceFor))
  :request-handlers (ht
                     ("window/showQuickPick" #'lsp-netbeans--show-quick-pick))
  :action-handlers (ht
                    ("workbench.action.focusActiveEditorGroup" #'lsp-netbeans--resolve-with-focus))
  :initialized-fn #'lsp-netbeans--initialized
  :download-server-fn #'lsp-netbeans--install-server))

(defun lsp-netbeans--initialized (_ws)
  (lsp-netbeans--make-busy-spinner))

(defun lsp-netbeans--treemacs-sync ()
  (if (bound-and-true-p lsp-treemacs-sync-mode)
      (progn
        (lsp-treemacs--treemacs->lsp)
        (let* ((wsname (treemacs-workspace->name (treemacs-current-workspace)))
               (wsuserdir (f-join lsp-server-install-dir
                                  (format "asf.apache-netbeans-java.userdir.%s" wsname))))
          (if (not (equal lsp-netbeans-user-dir wsuserdir))
              (progn
                ;; shutdown servers
                (->> (lsp-session)
                     (lsp-session-folder->servers)
                     (hash-table-values)
                     (-flatten)
                     (-uniq)
                     (-map #'lsp-workspace-shutdown))
                (if (equal "Default" wsname)
                    (setq lsp-netbeans-user-dir nil)
                  (setq lsp-netbeans-user-dir wsuserdir)))))))
  lsp-netbeans-user-dir)

(with-eval-after-load 'lsp-treemacs
  (add-hook 'treemacs-switch-workspace-hook #'lsp-netbeans--treemacs-sync)
  (lsp-netbeans--treemacs-sync))

(defun lsp-netbeans--busy-spinner-start ()
  (if-let* ((ws (lsp-find-workspace 'netbeans nil))
            (proc (lsp--workspace-cmd-proc ws))
            (output-buffer (get-buffer-create "*lsp-netbeans-cpu-usage*")))
      ; check the cpu usage of the netbeans process and show a spinner, if it is
      ; high
      (make-process
       :name "lsp-netbeans-cpu-usage-check"
       :buffer output-buffer
       :command `("/bin/bash"
                  "-c"
                  ,(format
                    "top -b -d 0.1 -n 2 -p $(ps --ppid `ps --ppid $(ps --ppid %s -o pid=) -o pid=` -o pid=) | tail -1 | awk '{print $9}'"
                    (process-id proc)))
       :sentinel (lambda (_ evt)
                   (if (string-equal evt "finished\n")
                       (let ((cpu (with-current-buffer output-buffer
                                    (goto-char (point-max))
                                    (forward-line -1)
                                    (thing-at-point 'number))))
                         (if (> cpu 80)
                             (if (not lsp-netbeans--busy-spinner-spinner)
                                 (setq lsp-netbeans--busy-spinner-spinner (spinner-start 'vertical-rising)))
                           (progn
                             (spinner-stop lsp-netbeans--busy-spinner-spinner)
                             (setq lsp-netbeans--busy-spinner-spinner nil)))
                         (lsp-netbeans--make-busy-spinner)))))
    ; stop spinner if no ws or proc found
    (spinner-stop lsp-netbeans--busy-spinner-spinner)))

(defun lsp-netbeans--make-busy-spinner ()
  (if lsp-netbeans-busy-spinner
      (run-with-timer 5 nil #'lsp-netbeans--busy-spinner-start)))

(defun lsp-netbeans-start-busy-spinner ()
  (interactive)
  (setq lsp-netbeans-busy-spinner nil)
  ; wait for any previous timer to expire, then run the new one
  (run-with-timer 10 nil (lambda ()
                           (setq lsp-netbeans-busy-spinner t)
                           (lsp-netbeans--busy-spinner-start))))

(provide 'lsp-netbeans)

;;; lsp-netbeans.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
