(require 'lsp-mode)

(defcustom lsp-graalvm-download-url
  (cond
   ((eq system-type 'darwin)
    "https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-20.0.0/graalvm-ce-java8-darwin-amd64-20.0.0.tar.gz")
   ((eq system-type 'gnu/linux)
    "https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-20.0.0/graalvm-ce-java8-linux-amd64-20.0.0.tar.gz"))
  "URL to download the GraalVM from"
  :group 'lsp-graalvm
  :type 'string)

(defcustom lsp-graalvm-install-dir (f-join lsp-server-install-dir "graalvm")
  "GraalVM installation dir"
  :group 'lsp-graalvm
  :type 'string)

(defcustom lsp-graalvm-python-delegate-server "pyls --tcp --port %d"
  "Commandline to launch delegate server. Leave %d in the commandline for the port/"
  :group 'lsp-graalvm
  :type 'string)

(defcustom lsp-graalvm-js-delegate-server
  ;; TODO: switch to "npx javascript-typescript-langserver -p %d" once that is included in GraalVM
  (concat "node " (f-join lsp-graalvm-install-dir "jre/languages/js/bin/javascript-typescript-langserver -p %d"))
  "Commandline to launch delegate server. Leave %d in the commandline for the port/"
  :group 'lsp-graalvm
  :type 'string)

(defcustom lsp-graalvm-ruby-delegate-server "solargraph socket --port %d"
  "Commandline to launch delegate server. Leave %d in the commandline for the port/"
  :group 'lsp-graalvm
  :type 'string)

;; TODO: enable R
(defcustom lsp-graalvm-R-delegate-server nil ; "R --slave -e 'languageserver::run(port=%d)'"
  "Commandline to launch delegate server. Leave %d in the commandline for the port/"
  :group 'lsp-graalvm
  :type 'string)


(defun lsp-graalvm-server-command (main-port)
  (let ((delegates ""))
    (if (> main-port 0)
        (seq-mapn (lambda (name)
                    (let ((cmd (eval (intern (concat "lsp-graalvm-" name "-delegate-server"))))
                          (pname (format "graalvm-%s-delegate" name)))
                      (if cmd
                          (let ((port (lsp--find-available-port "localhost" (cl-incf lsp--tcp-port))))
                            ;; launch the delegate language servers that are installed inside our graalvm
                            (let (oldproc (lsp-session-get-metadata pname))
                              (if oldproc (delete-process oldproc)))
                            (lsp-session-set-metadata
                             pname
                             (start-process-shell-command pname
                                                          (get-buffer-create (concat "*" pname "*"))
                                                          (f-join lsp-graalvm-install-dir "bin" (format cmd port))))
                            (setq delegates
                                  (concat delegates (format "%s@%d," name port)))))))
                  (list "R" "python" "js" "ruby")))
    (if (string-match-p ",$" delegates)
        (setq delegates (substring delegates 0 -1)))
    `(,(f-join lsp-graalvm-install-dir "bin" "js")
      "--jvm"
      "--experimental-options"
      "--polyglot"
      ,(format "--lsp.Delegates=%s" delegates)
      ,(format "--lsp=%d" main-port))))


(defmacro lsp-graalvm--multiple-async-shell-commands (error-callback &rest commands)
  "Run COMMANDS in sequences, each runs asynchronously."
  (cl-labels ((aux (commands)
                   (pcase commands
                     (`(,command . ,rest)
                      `(lsp-async-start-process
                        (lambda ()
                          ,(first (last command))
                          ,(aux rest))
                        ,error-callback
                        ,@(butlast command))))))
    (aux commands)))


(defun lsp-graalvm--install-server (client callback error-callback update?)
  (unless (and (not update?)
               (f-exists? lsp-graalvm-install-dir))
    (lexical-let* ((temp-file (f-join (f-dirname lsp-graalvm-install-dir) "graalvm.tar.gz"))
                   (error-cb error-callback)
                   (cb callback)
                   (install-dir lsp-graalvm-install-dir)
                   (download-script (format "curl -L -C - --output %s %s" temp-file lsp-graalvm-download-url))
                   (unzip-script (format "mkdir -p %s && tar -C %s --strip-components=1 -xzf %s" lsp-graalvm-install-dir lsp-graalvm-install-dir temp-file)))
      (lsp--info "Downloading GraalVM...")
      (f-delete lsp-graalvm-install-dir t)

      (lsp-graalvm--multiple-async-shell-commands
       error-cb
       ("sh" "-c" download-script
        (lsp--info "Extracting GraalVM..."))
       ("sh" "-c" unzip-script
        (lsp--info "Installing GraalVM languages..."))
       ("sh" "-c" (f-join install-dir "bin" "gu install ruby R python")
        (lsp--info "Setting up TruffleRuby OpenSSL gem..."))
       ("sh" "-c" (f-join install-dir "jre" "languages" "ruby" "lib" "truffle" "post_install_hook.sh")
        (lsp--info "Installing GraalVM delegate language server for JavaScript"))
       ("sh" "-c" (f-join install-dir "bin" "npm install -g javascript-typescript-langserver")
        (lsp--info "Installing GraalVM delegate language server for Ruby"))
       ("sh" "-c" (f-join install-dir "bin" "gem install solargraph")
        (lsp--info "Installing GraalVM delegate language server for Python"))
       ;; TODO: use graalpython here
       ("sh" "-c" (format "python3 -m venv %s && %s -m pip install python-language-server"
                          install-dir
                          (f-join install-dir "bin" "python3"))
        ;; TODO: enable R
        ;;  (lsp--info "Installing GraalVM delegate language server for R"))
        ;; ("sh" "-c" (f-join install-dir "bin" "R --slave -e 'install.packages(\"languageserver\")'")
        (progn (lsp--info "Done installing GraalVM language server.")
               (funcall cb)))))))


(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tcp-connection 'lsp-graalvm-server-command)
  :major-modes '(python-mode ruby-mode js-mode javascript-mode)
  :server-id 'graallsp
  :priority 10
  :download-server-fn #'lsp-graalvm--install-server))
