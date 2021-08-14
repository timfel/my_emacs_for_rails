(require 'lsp-mode)
(require 'dap-mode)

(defcustom lsp-netbeans-download-url "https://marketplace.visualstudio.com/_apis/public/gallery/publishers/ASF/vsextensions/apache-netbeans-java/12.4.301/vspackage"
  "URL to download the server from"
  :group 'lsp-netbeans
  :type 'string)

(defcustom lsp-netbeans-install-dir (f-join lsp-server-install-dir "asf.apache-netbeans-java")
  "Apache Netbeans Java Language Server installation dir"
  :group 'lsp-netbeans
  :type 'string)

(defun lsp-netbeans-server-command (main-port)
  `(,(f-join lsp-netbeans-install-dir "run.sh")
    "--start-java-debug-adapter-server=listen:0"
    ,(format "--start-java-language-server=listen:%d" main-port)))

(defun lsp-netbeans--install-server (client callback error-callback update?)
  (unless (and (not update?)
               (f-exists? lsp-netbeans-install-dir))
    (make-directory lsp-netbeans-install-dir t)
    (let ((download-path (f-join lsp-netbeans-install-dir "vspackage")))
      (lsp-download-install
       (lambda ()
         (let ((run-script-path (f-join lsp-netbeans-install-dir "run.sh")))
           (with-temp-file run-script-path
             (insert "#!/bin/bash
                      cd `dirname $0`
                      cd extension
                      node out/nbcode.js $@ >&2")
             (executable-make-buffer-file-executable-if-script-p)))
         (message "Done downloading Netbeans LSP server"))
       (lambda (err) (error err))
       :url lsp-netbeans-download-url
       :store-path download-path
       :decompress :zip))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tcp-connection 'lsp-netbeans-server-command)
  :activation-fn (lsp-activate-on "java")
  :server-id 'netbeans
  :initialization-options (list :nbcodeCapabilities
                                (list
                                 :testResultsSupport (lsp-json-bool nil)
                                 :statusBarMessageSupport (lsp-json-bool nil)
                                 :wantsGroovySupport (lsp-json-bool nil)))
  :priority 10
  :multi-root t
  :download-server-fn #'lsp-netbeans--install-server))

;; TODO: commands
;; java.build.workspace
;; java.generate.getters
;; java.generate.setters
;; java.generate.getters.setters
;; java.generate.implementMethod
;; java.generate.overrideMethod
;; java.generate.equals.hashCode
;; java.generate.delegateMethod
;; java.generate.constructor
;; java.generate.logger
;; java.generate.toString
;;
;; (defun lsp-netbeans-super-implementation ()
;;   (interactive)
;;   (lsp-send-execute-command
;;    "java.super.implementation"
;;    (vector
;;     "file:///...java"
;;     (list
;;      :range (list
;;              :start (list :line 1385 :character 20)
;;              :end (list :line 1385 :character 24))))))

(defun dap-netbeans--populate-attach-args (conf)
  (dap--put-if-absent conf :hostName (read-string "Enter host: " "localhost"))
  (dap--put-if-absent conf :port (read-string "Enter port: " "8000"))
  (dap--put-if-absent conf :host "localhost")
  (dap--put-if-absent conf :name (format "%s(%s)" (plist-get conf :host) (plist-get conf :port)))
  conf)

(defun dap-netbeans--populate-default-args (conf)
  (setq conf (plist-put conf :type "java8+"))

  (setq conf (pcase (plist-get conf :request)
               ;; ("launch" (dap-java--populate-launch-args conf))
               ("attach" (dap-netbeans--populate-attach-args conf))
               (_ (dap-netbeans--populate-attach-args conf))))

  (plist-put conf :debugServer (with-current-buffer (get-buffer "*netbeans::stderr*")
                                 (goto-char (point-max))
                                 (re-search-backward "Java Debug Server Adapter listening at port ")
                                 (forward-word 8)
                                 (thing-at-point 'word)))
  (plist-put conf :__sessionId (number-to-string (float-time)))
  conf)

(dap-register-debug-provider "java8+" #'dap-netbeans--populate-default-args)
(dap-register-debug-template "Java8+ Socket Attach"
                             (list :id "com.sun.jdi.SocketAttach"
                                   :type "java8+"
                                   :request "attach"
                                   :name "Attach to Port"
                                   :hostName "localhost"
                                   :port nil))
;; (dap-register-debug-template "Java8+ Process Attach"
;;                              (list :id "com.sun.jdi.ProcessAttach"
;;                                    :type "java8+"
;;                                    :request "attach"
;;                                    :name "Attach to Process"
;;                                    :processId nil))
