;;; lsp-graalvm.el --- GraalVM Language Server Client settings         -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Tim Felgentreff

;; Author: Tim Felgentreff <timfelgentreff@gmail.com>
;; Keywords: ruby python R javascript graalvm lsp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; LSP client for the built-in GraalVM language server

;;; Code:

(require 'lsp-mode)

(defcustom lsp-graalvm-download-url
  (cond
   ((eq system-type 'darwin)
    "https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-21.3.0/graalvm-ce-java11-darwin-amd64-21.3.0.tar.gz")
   ((eq system-type 'gnu/linux)
    "https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-21.3.0/graalvm-ce-java11-linux-amd64-21.3.0.tar.gz"))
  "URL to download the GraalVM from"
  :group 'lsp-graalvm
  :type 'string)

(defcustom lsp-graalvm-install-dir (f-join lsp-server-install-dir "graalvm")
  "GraalVM installation dir"
  :group 'lsp-graalvm
  :type 'string)

(defcustom lsp-graalvm-python-delegate-server (format "%s %s" (f-join "pyls_venv" "bin" "pyls") "--tcp --port %d")
  "Commandline to launch delegate server. Leave %d in the commandline for the port/"
  :group 'lsp-graalvm
  :type 'string)

(defcustom lsp-graalvm-js-delegate-server "npx javascript-typescript-langserver -p %d"
  "Commandline to launch delegate server. Leave %d in the commandline for the port/"
  :group 'lsp-graalvm
  :type 'string)

(defcustom lsp-graalvm-ruby-delegate-server "solargraph socket --port %d"
  "Commandline to launch delegate server. Leave %d in the commandline for the port/"
  :group 'lsp-graalvm
  :type 'string)

(defcustom lsp-graalvm-R-delegate-server "R --slave -e 'languageserver::run(port=%d)'"
  "Commandline to launch delegate server. Leave %d in the commandline for the port/"
  :group 'lsp-graalvm
  :type 'string)

(defcustom lsp-graalvm-languages '("R" "js" "python" "ruby")
  "Which languages to enable this server for"
  :group 'lsp-graalvm
  :type '(set (const "R") (const "js") (const "python") (const "ruby")))

(defcustom lsp-graalvm-custom-server nil
  "Wait for a user-started server to connect to for a few seconds"
  :group 'lsp-graalvm
  :type 'boolean)

(defun lsp-graalvm-server-command (main-port)
  (let ((delegates ""))
    (if (> main-port 0)
        (if lsp-graalvm-custom-server
            (progn
              (message "Waiting for server on port %d" main-port)
              (sit-for 15))
          (progn
            (seq-mapn (lambda (name)
                        (let ((cmd (eval (intern (concat "lsp-graalvm-" name "-delegate-server"))))
                              (pname (format "graalvm-%s-delegate" name)))
                          (if cmd
                              (let ((port (lsp--find-available-port "localhost" (cl-incf lsp--tcp-port)))
                                    (delegate-command-buffer (concat "*" pname "*")))
                                ;; kill old delegate servers that may be hanging around
                                (let ((oldproc (lsp-session-get-metadata pname)))
                                  (when oldproc
                                    (delete-process oldproc)
                                    (sit-for 1)))
                                (let ((oldproc (get-buffer-process delegate-command-buffer)))
                                  (when oldproc
                                    (delete-process oldproc)
                                    (kill-buffer delegate-command-buffer)
                                    (sit-for 1)))
                                ;; launch the delegate language servers that are installed inside our graalvm
                                (lsp-session-set-metadata
                                 pname
                                 (start-process-shell-command pname
                                                              (get-buffer-create delegate-command-buffer)
                                                              (f-join lsp-graalvm-install-dir "bin" (format cmd port))))
                                (sit-for 10)
                                (setq delegates
                                      (concat delegates (format "%s@%d," name port)))))))
                      lsp-graalvm-languages)
            (if (string-match-p ",$" delegates)
                (setq delegates (substring delegates 0 -1))))))
    (if lsp-graalvm-custom-server
        '("/usr/bin/sleep" "2147483648")
      `(,(f-join lsp-graalvm-install-dir "bin" "js")
        "--jvm"
        "--experimental-options"
        "--polyglot"
        ,(format "--lsp.Delegates=%s" delegates)
        ,(format "--lsp=%d" main-port)))))


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
       ("sh" "-c" (f-join install-dir "bin" "gu install ruby R python nodejs")
        (lsp--info "Setting up TruffleRuby OpenSSL gem..."))
       ("sh" "-c" (f-join install-dir "languages" "ruby" "lib" "truffle" "post_install_hook.sh")
        (lsp--info "Installing GraalVM delegate language server for JavaScript"))
       ("sh" "-c" (if (seq-contains-p lsp-graalvm-languages "js")
                      (f-join install-dir "bin" "npm install -g javascript-typescript-langserver")
                    "echo Skipped")
        (lsp--info "Installing GraalVM delegate language server for Ruby"))
       ("sh" "-c" (if (seq-contains-p lsp-graalvm-languages "ruby")
                      (f-join install-dir "bin" "gem install solargraph")
                    "echo Skipped")
        (lsp--info "Creating venv for GraalVM language server for Python"))
       ("sh" "-c" (if (seq-contains-p lsp-graalvm-languages "python")
                      (format "%s -m venv %s"
                              ;; TODO: will work once graalpython supports pyls
                              ;; (f-join install-dir "bin" "graalpython")
                              "python3"
                              (f-join install-dir "bin" "pyls_venv"))
                    "echo Skipped")
        (lsp--info "Installing GraalVM delegate language server for Python"))
       ("sh" "-c" (if (seq-contains-p lsp-graalvm-languages "python")
                      (format "VIRTUAL_ENV=%s %s -m pip install python-language-server"
                              (f-join install-dir "bin" "pyls_venv")
                              (f-join install-dir "bin" "pyls_venv" "bin" "python"))
                    "echo Skipped")
        (lsp--info "Installing GraalVM delegate language server for R"))
       ("sh" "-c" (if (seq-contains-p lsp-graalvm-languages "R")
                      (f-join install-dir "bin" "R --vanilla --quiet -e 'utils::install.packages(\"languageserver\", Ncpus=1, INSTALL_opts=\"--no-docs --no-byte-compile --no-staged-install --no-test-load --use-vanilla\")'")
                    "echo Skipped")
        (progn (lsp--info "Done installing GraalVM language server.")
               (funcall cb)))))))

(defun lsp-graalvm-dry-run ()
  (interactive)
  (lsp-request-async "workspace/executeCommand"
                     (list :command "dry_run"
                           :arguments (vector (format "file://%s" (buffer-file-name))))
                     (lambda (result)
                       (setf (lsp--workspace-status-string (cl-first (lsp-workspaces))) nil)
                       (force-mode-line-update)
                       (pcase result
                         (1 (lsp--info "Successfully build project."))
                         (2 (lsp--error "Failed to build project."))))))

(defun lsp-graalvm-get-coverage ()
  (interactive)
  (lsp-request-async "workspace/executeCommand"
                     (list :command "get_coverage"
                           :arguments (vector (format "file://%s" (buffer-file-name))))
                     (lambda (result)
                       result)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tcp-connection #'lsp-graalvm-server-command)
  :major-modes '(python-mode ruby-mode js-mode javascript-mode)
  :server-id 'graallsp
  :priority -1
  :download-server-fn #'lsp-graalvm--install-server))

(provide 'lsp-graalvm)

;;; lsp-graalvm.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
