;;; timfel-gptel-tools.el --- GPTel tool configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Local GPTel tool registrations and tool selection.

;;; Code:

(require 'gptel)
(require 'llm-tool-collection)
(require 'timfel)
(require 'subr-x)
(require 'url-util)

(defun timfel/gptel-tool-execute-command (command &optional working-dir)
  "Execute COMMAND, optionally in WORKING-DIR, and return its output."
  (with-temp-message (format "Executing command: `%s`" command)
    (let ((default-directory (if (and working-dir (not (string= working-dir "")))
                                 (expand-file-name working-dir)
                               default-directory)))
      (shell-command-to-string command))))

(defun timfel/gptel-tool-change-directory (dir)
  "Change `default-directory' to DIR when it exists."
  (when (file-directory-p dir)
    (setq default-directory dir)))

(defun timfel/gptel-tool-get-current-directory ()
  "Return the current default directory."
  default-directory)

(defun timfel/gptel-tool-get-recently-edited-filenames ()
  "Return up to 5 recently visited filenames from open buffers."
  (mapcar #'buffer-file-name
          (seq-take
           (delete-dups
            (seq-remove
             (lambda (buffer)
               (or (null buffer)
                   (not (buffer-file-name buffer))
                   (string-prefix-p " " (buffer-name buffer))))
             (buffer-list)))
           5)))

(defun timfel/gptel-tool-search-in-project (pattern)
  "Search for PATTERN in the recent project using ripgrep."
  (let ((rg-cmd (format "rg --max-count 20 --no-heading --color never %s %s"
                        (shell-quote-argument pattern)
                        (determine-recent-project-root))))
    (shell-command-to-string rg-cmd)))

(defun timfel/gptel-tool-set-file-content (filename content)
  "Overwrite FILENAME with CONTENT and return a confirmation string."
  (with-temp-file filename
    (insert content))
  "Saved!")

(defun timfel/gptel-tool-read-webpage (url)
  "Read URL via `w3m -dump' and return the output."
  (shell-command-to-string (format "w3m -dump '%s'" url)))

(defun timfel/gptel-tool-search-web (phrase)
  "Search the web for PHRASE, or dump it directly when it is a URL."
  (if (string-match-p "^http" phrase)
      (shell-command-to-string (format "w3m -dump '%s'" phrase))
    (shell-command-to-string
     (format "w3m -dump 'https://duckduckgo.com/?q=%s'"
             (url-hexify-string phrase)))))

(gptel-make-tool
 :function #'timfel/gptel-tool-execute-command
 :name "execute_command"
 :description "Executes a shell command and returns the output as a string. IMPORTANT: This tool allows execution of arbitrary code; user confirmation will be required before any command is run."
 :args (list
        '(:name "command"
                :type string
                :description "The complete shell command to execute.")
        '(:name "working_dir"
                :type string
                :description "Optional: The directory in which to run the command. Defaults to the current directory if not specified."))
 :category "command"
 :confirm t
 :include t)

(gptel-make-tool
 :function #'timfel/gptel-tool-change-directory
 :name "change_directory"
 :description "Change the default working directory for subsequent work."
 :args (list '(:name "dir" :type string :description "The directory to cd into."))
 :category "command"
 :confirm t
 :include nil)

(gptel-make-tool
 :function #'timfel/gptel-tool-get-current-directory
 :name "get_current_directory"
 :description "Return the name of the current working directory."
 :args (list)
 :confirm nil
 :include nil
 :category "command")

(gptel-make-tool
 :name "get_recently_edited_filenames"
 :description "Return a list of the 5 most recently opened buffers in this emacs session to help better understand the context of what we are doing and the users request."
 :function #'timfel/gptel-tool-get-recently-edited-filenames
 :args (list)
 :confirm nil
 :include nil
 :category "buffers")

(gptel-make-tool
 :name "search_in_project"
 :description "Search for a string within the project using a fast search tool (like ripgrep)."
 :function #'timfel/gptel-tool-search-in-project
 :args (list '(:name "pattern"
                     :type string
                     :description "Pattern to search for"))
 :confirm t
 :include nil
 :category "search")

(gptel-make-tool
 :name "set_file_content"
 :description "Set the content of a file to the given string. Expects filename, and the full content."
 :function #'timfel/gptel-tool-set-file-content
 :args (list
        '(:name "filename" :type string :description "The file to overwrite.")
        '(:name "content" :type string :description "The new content for the file."))
 :confirm t
 :include t
 :category "files")

(gptel-make-tool
 :function #'timfel/gptel-tool-read-webpage
 :name "read_webpage"
 :description "Read the contents of a URL"
 :args (list '(:name "url"
                     :type string
                     :description "The URL to read"))
 :category "web")

(gptel-make-tool
 :function #'timfel/gptel-tool-search-web
 :name "search_web"
 :description "Search the web for a string."
 :args (list '(:name "phrase"
                     :type string
                     :description "The keywords to search for on the web, just the KEYWORDS"))
 :category "web")

(llm-tool-collection-deftool list-buffers
  (:category "buffers" :tags (buffers editing))
  nil
  "Get the list of files the user has open in buffers."
  (string-join
   (remove nil (mapcar #'buffer-file-name
                       (buffer-list)))
   "\n"))

(mapc (apply-partially #'apply #'gptel-make-tool)
      (llm-tool-collection-get-category "filesystem"))
(mapc (apply-partially #'apply #'gptel-make-tool)
      (llm-tool-collection-get-category "buffers"))

(setq gptel-tools
      (let ((funcs nil)
            (names '("get_recently_edited_filenames"
                     "search_in_project"
                     "set_file_content"
                     "read_webpage"
                     "search_web"
                     "change_directory"
                     "get_current_directory"
                     "execute_command"
                     "view_buffer"
                     "read_file"
                     "list_directory"
                     "list_buffers"
                     "create_file"
                     "patch_file"
                     "create_directory")))
        (dolist (category gptel--known-tools)
          (dolist (pair (cdr category))
            (when (member (car pair) names)
              (push (cdr pair) funcs))))
        funcs))

(setq gptel-use-tools t
      gptel-confirm-tool-calls 'auto)

(provide 'timfel-gptel-tools)

;;; timfel-gptel-tools.el ends here
