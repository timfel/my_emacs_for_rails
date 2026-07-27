;;; gptel-pi-test.el --- Tests for gptel-pi -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-pi)

(defmacro gptel-pi-test--with-file (content &rest body)
  "Write CONTENT to a temporary file and evaluate BODY with `file' bound."
  (declare (indent 1) (debug t))
  `(let ((file (make-temp-file "gptel-pi-test-")))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           ,@body)
       (delete-file file))))

(ert-deftest gptel-pi-truncate-head-by-lines ()
  (let ((result (gptel-pi--truncate-result "one\ntwo\nthree\n" 'head 100 2)))
    (should (plist-get result :truncated))
    (should (equal (plist-get result :content) "one\ntwo\n"))
    (should (= (plist-get result :lines) 2))))

(ert-deftest gptel-pi-truncate-tail-by-lines ()
  (let ((result (gptel-pi--truncate-result "one\ntwo\nthree\n" 'tail 100 2)))
    (should (plist-get result :truncated))
    (should (equal (plist-get result :content) "two\nthree\n"))
    (should (= (plist-get result :lines) 2))))

(ert-deftest gptel-pi-truncate-counts-utf8-bytes ()
  (let ((result (gptel-pi--truncate-result "ää\nok\n" 'head 5 10)))
    (should (plist-get result :truncated))
    (should (equal (plist-get result :content) "ää\n"))
    (should (= (plist-get result :bytes) 5))))

(ert-deftest gptel-pi-truncate-does-not-return-partial-first-line ()
  (let ((result (gptel-pi--truncate-result "abcdef\nsecond\n" 'head 4 10)))
    (should (plist-get result :first-line-too-long))
    (should (equal (plist-get result :content) ""))))

(ert-deftest gptel-pi-read-default-range ()
  (gptel-pi-test--with-file "alpha\nbeta\ngamma"
    (should (equal (gptel-pi--tool-read file)
                   "alpha\nbeta\ngamma\n[Showing lines 1-3 of 3.]"))))

(ert-deftest gptel-pi-read-offset-and-limit ()
  (gptel-pi-test--with-file "alpha\nbeta\ngamma\ndelta"
    (should (equal (gptel-pi--tool-read file 2 2)
                   (concat "beta\ngamma\n"
                           "[Showing lines 2-3 of 4; use offset=4 to continue.]")))))

(ert-deftest gptel-pi-read-limit-at-eof ()
  (gptel-pi-test--with-file "alpha\nbeta\ngamma"
    (should (equal (gptel-pi--tool-read file 2 20)
                   "beta\ngamma\n[Showing lines 2-3 of 3.]"))))

(ert-deftest gptel-pi-read-reports-out-of-range-offset ()
  (gptel-pi-test--with-file "alpha\nbeta"
    (should (string-match-p
             "offset 3 is beyond end of file (2 lines total)"
             (gptel-pi--tool-read file 3)))))

(ert-deftest gptel-pi-read-reports-continuation-after-byte-limit ()
  (gptel-pi-test--with-file "1111\n2222\n3333"
    (let ((gptel-pi-max-tool-bytes 10)
          (gptel-pi-max-tool-lines 20))
      (should (equal (gptel-pi--tool-read file)
                     (concat "1111\n2222\n"
                             "[Showing lines 1-2 of 3; use offset=3 to continue.]"))))))

(ert-deftest gptel-pi-read-exceptionally-long-line-is-actionable ()
  (gptel-pi-test--with-file "123456789\nnext"
    (let ((gptel-pi-max-tool-bytes 5))
      (let ((result (gptel-pi--tool-read file)))
        (should (string-match-p "Line 1 exceeds the 5-byte read limit" result))
        (should (string-match-p "sed -n '1p'" result))))))

(ert-deftest gptel-pi-initializes-org-session-layout ()
  (with-temp-buffer
    (org-mode)
    (insert "@user ")
    (gptel-pi--initialize-org-session)
    (should (equal (buffer-string)
                   (concat "* Archive\n** Tool outputs\n\n** Compactions\n\n"
                           "* Conversation\n@user ")))
    (should (= (point) (point-max)))
    (should (org-find-exact-headline-in-buffer "Conversation"))))

(ert-deftest gptel-pi-archive-entry-preserves-point-and-resolves-link ()
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-pi-session-p t)
    (insert "@user ")
    (gptel-pi--initialize-org-session)
    (insert "Keep point here")
    (let ((original-point (point-marker))
          (link (gptel-pi--archive-entry
                 "tool" "Tool outputs" "bash 0001: test" "full output\n"
                 "- Exit status :: 1\n")))
      (should (equal link "[[gptel-pi-tool-0001][bash 0001: test]]"))
      (should (= (point) original-point))
      (should (string-match-p "<<gptel-pi-tool-0001>>" (buffer-string)))
      (should (string-match-p "#\\+begin_example\nfull output\n#\\+end_example"
                              (buffer-string)))
      (goto-char (point-min))
      (search-forward "<<gptel-pi-tool-0001>>")
      (should (org-link-search "gptel-pi-tool-0001")))))

(ert-deftest gptel-pi-archive-targets-are-monotonic ()
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-pi-session-p t)
    (gptel-pi--initialize-org-session)
    (should (equal (gptel-pi--archive-entry
                    "tool" "Tool outputs" "first" "one")
                   "[[gptel-pi-tool-0001][first]]"))
    (should (equal (gptel-pi--archive-entry
                    "tool" "Tool outputs" "second" "two")
                   "[[gptel-pi-tool-0002][second]]"))))

(ert-deftest gptel-pi-normalizes-only-assistant-headings ()
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-pi-session-p t)
    (insert "* User branch\nUser text\n")
    (let ((begin (point)))
      (insert (propertize "** Findings\nBody\n*** Details\n"
                          'gptel 'response 'front-sticky '(gptel)))
      (gptel-pi-normalize-assistant-headings begin (point-max)))
    (should (equal (buffer-string)
                   "* User branch\nUser text\n*Findings*\nBody\n*Details*\n"))
    (goto-char (point-min))
    (search-forward "Findings")
    (should (eq (get-text-property (match-beginning 0) 'gptel) 'response))
    (should (eq (get-text-property (1- (match-beginning 0)) 'gptel) 'response))))

(ert-deftest gptel-pi-normalization-skips-protected-blocks ()
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-pi-session-p t)
    (insert (concat "#+begin_src text\n** Source heading\n#+end_src\n"
                    "#+begin_example\n** Example heading\n#+end_example\n"
                    "#+begin_quote\n** Quote heading\n#+end_quote\n"
                    "#+begin_reasoning\n** Reasoning heading\n#+end_reasoning\n"
                    "#+begin_tool call\n** Tool heading\n#+end_tool\n"
                    "** Real response heading\n"))
    (gptel-pi-normalize-assistant-headings (point-min) (point-max))
    (should (string-match-p "^\\*\\* Source heading$" (buffer-string)))
    (should (string-match-p "^\\*\\* Example heading$" (buffer-string)))
    (should (string-match-p "^\\*\\* Quote heading$" (buffer-string)))
    (should (string-match-p "^\\*\\* Reasoning heading$" (buffer-string)))
    (should (string-match-p "^\\*\\* Tool heading$" (buffer-string)))
    (should (string-match-p "^\\*Real response heading\\*$" (buffer-string)))))

(ert-deftest gptel-pi-normalization-is-idempotent ()
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-pi-session-p t)
    (insert "** Findings\n")
    (gptel-pi-normalize-assistant-headings (point-min) (point-max))
    (let ((once (buffer-string)))
      (gptel-pi-normalize-assistant-headings (point-min) (point-max))
      (should (equal (buffer-string) once)))))

(ert-deftest gptel-pi-promotes-label-to-child-heading ()
  (with-temp-buffer
    (org-mode)
    (insert "* Conversation\n*Findings*\n")
    (goto-char (point-max))
    (forward-line -1)
    (gptel-pi-promote-label)
    (should (equal (buffer-string) "* Conversation\n** Findings\n"))))

(ert-deftest gptel-pi-eval-prints-arbitrary-values ()
  (should (equal (gptel-pi--tool-eval "42") "42"))
  (should (equal (gptel-pi--tool-eval "nil") "nil"))
  (should (equal (gptel-pi--tool-eval "(list 'one 2)") "(one 2)")))

(ert-deftest gptel-pi-edit-rejects-empty-old-text ()
  (gptel-pi-test--with-file "unchanged"
    (should (string-match-p "must not be empty"
                            (gptel-pi--tool-edit file "" "replacement")))
    (should (equal (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))
                   "unchanged"))))

(ert-deftest gptel-pi-edit-requires-one-match ()
  (gptel-pi-test--with-file "same\nsame\n"
    (should (string-match-p "matches 2 times"
                            (gptel-pi--tool-edit file "same" "changed")))
    (should (equal (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))
                   "same\nsame\n"))))

(ert-deftest gptel-pi-edit-counts-overlapping-matches ()
  (gptel-pi-test--with-file "aaa"
    (should (string-match-p "matches 2 times"
                            (gptel-pi--tool-edit file "aa" "b")))))

(ert-deftest gptel-pi-edit-replaces-unique-match-and-reports-line ()
  (gptel-pi-test--with-file "first\nold text\nlast\n"
    (should (string-match-p "at line 2$"
                            (gptel-pi--tool-edit file "old text" "new text")))
    (should (equal (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))
                   "first\nnew text\nlast\n"))))

(ert-deftest gptel-pi-tool-optional-arguments-are-not-required ()
  (dolist (spec '(("read" "offset" "limit")
                  ("bash" "timeout")))
    (let* ((tool (gptel-get-tool (car spec)))
           (parsed (aref (gptel--parse-tools nil (list tool)) 0))
           (function (plist-get parsed :function))
           (parameters (plist-get function :parameters))
           (required (append (plist-get parameters :required) nil)))
      (dolist (optional (cdr spec))
        (should-not (member optional required))))))

(provide 'gptel-pi-test)
;;; gptel-pi-test.el ends here
