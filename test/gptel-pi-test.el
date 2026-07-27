;;; gptel-pi-test.el --- Tests for gptel-pi -*- lexical-binding: t; -*-

(require 'ert)
(require 'gptel-pi)

(defvar gptel--rewrite-message)

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

(ert-deftest gptel-pi-archive-region-copies-verbatim-and-keeps-source-active ()
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-pi-session-p t)
    (gptel-pi--initialize-org-session)
    (insert "First turn\n#+begin_tool x\nresult\n#+end_tool\nLatest turn\n")
    (goto-char (point-min))
    (search-forward "First turn")
    (let ((begin (match-beginning 0)))
      (search-forward "Latest turn")
      (let* ((end (match-beginning 0))
             (original (buffer-substring begin end)))
        (goto-char end)
        (set-mark begin)
        (activate-mark)
        (should (equal (gptel-pi-archive-region begin end)
                       "[[gptel-pi-compaction-0001][compaction archive 0001]]"))
        (should (equal (buffer-substring (region-beginning) (region-end))
                       original))
        (should (string-match-p
                 (regexp-quote (concat "<<gptel-pi-compaction-0001>>\n\n" original))
                 (buffer-string)))
        (should (string-match-p
                 (regexp-quote
                  "Original transcript: [[gptel-pi-compaction-0001][compaction archive 0001]]")
                 (buffer-string)))))))

(ert-deftest gptel-pi-archive-region-targets-increase ()
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-pi-session-p t)
    (gptel-pi--initialize-org-session)
    (insert "one two")
    (let ((begin (- (point-max) 7))
          (end (- (point-max) 4)))
      (gptel-pi-archive-region begin end)
      (goto-char (point-max))
      (insert "more")
      (setq begin (- (point-max) 4)
            end (point-max))
      (should (equal (gptel-pi-archive-region begin end)
                     "[[gptel-pi-compaction-0002][compaction archive 0002]]")))))

(ert-deftest gptel-pi-compaction-validation-rejects-split-tool-block ()
  (with-temp-buffer
    (org-mode)
    (insert "before\n#+begin_tool x\nresult\n#+end_tool\nafter\n")
    (goto-char (point-min))
    (search-forward "result")
    (let ((inside (point)))
      (should-error
       (gptel-pi--validate-compaction-region (point-min) inside)
       :type 'user-error)
      (should-error
       (gptel-pi--validate-compaction-region inside (point-max))
       :type 'user-error)
      (should-not
       (gptel-pi--validate-compaction-region (point-min) (point-max))))))

(ert-deftest gptel-pi-compaction-refuses-active-request ()
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-pi-session-p t
                gptel-pi--request-active-p t)
    (insert "text")
    (should-error (gptel-pi-compact-region (point-min) (point-max))
                  :type 'user-error)))

(ert-deftest gptel-pi-compact-region-composes-archive-and-rewrite ()
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-pi-session-p t)
    (gptel-pi--initialize-org-session)
    (insert "old transcript")
    (let ((begin (- (point-max) (length "old transcript")))
          (end (point-max))
          rewrite-started)
      (cl-letf (((symbol-function 'gptel-pi--start-compaction-rewrite)
                 (lambda () (setq rewrite-started t))))
        (gptel-pi-compact-region begin end))
      (should rewrite-started)
      (should (equal (buffer-substring (region-beginning) (region-end))
                     "old transcript"))
      (should (string-match-p "<<gptel-pi-compaction-0001>>"
                              (buffer-string)))
      (should (string-match-p "Original transcript: \\[\\[gptel-pi-compaction-0001\\]"
                              (buffer-string))))))

(ert-deftest gptel-pi-compaction-rewrite-disables-tools ()
  (with-temp-buffer
    (let ((gptel-use-tools t)
          (gptel-tools '(fake-tool))
          observed)
      (require 'gptel-rewrite)
      (cl-letf (((symbol-function 'gptel-rewrite)
                 (lambda ()
                   (interactive)
                   (setq observed
                         (list gptel-use-tools gptel-tools
                               gptel--rewrite-message
                               (run-hook-with-args-until-success
                                'gptel-rewrite-directives-hook))))))
        (gptel-pi--start-compaction-rewrite))
      (should-not (nth 0 observed))
      (should-not (nth 1 observed))
      (should (equal (nth 2 observed) gptel-pi--compaction-directive))
      (should (string-match-p "You compact coding-agent transcripts"
                              (nth 3 observed))))))

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

(ert-deftest gptel-pi-read-archives-full-truncated-selection ()
  (gptel-pi-test--with-file "1111\n2222\n3333"
    (with-temp-buffer
      (org-mode)
      (setq-local gptel-pi-session-p t)
      (gptel-pi--initialize-org-session)
      (let ((gptel-pi-max-tool-bytes 10)
            (gptel-pi-max-tool-lines 20))
        (let ((result (gptel-pi--tool-read file)))
          (should (string-match-p "Full selected output: \\[\\[gptel-pi-tool-0001\\]"
                                  result))
          (should (string-match-p "1111\n2222\n3333" (buffer-string))))))))

(ert-deftest gptel-pi-eval-prints-arbitrary-values ()
  (should (equal (gptel-pi--tool-eval "42") "42"))
  (should (equal (gptel-pi--tool-eval "nil") "nil"))
  (should (equal (gptel-pi--tool-eval "(list 'one 2)") "(one 2)")))

(ert-deftest gptel-pi-eval-archives-large-printed-result ()
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-pi-session-p t)
    (gptel-pi--initialize-org-session)
    (let ((gptel-pi-max-tool-bytes 5))
      (let ((result (gptel-pi--tool-eval "\"a long result\"")))
        (should (string-match-p "Full printed result: \\[\\[gptel-pi-tool-0001\\]"
                                result))
        (should (string-match-p "\\\"a long result\\\"" (buffer-string)))))))

(ert-deftest gptel-pi-bash-captures-streams-and-nonzero-status ()
  (with-temp-buffer
    (let (done result)
      (gptel-pi--tool-bash
       (lambda (value) (setq result value done t))
       "printf stdout; printf stderr >&2; exit 7" 10)
      (while (not done) (accept-process-output nil 0.05))
      (should (string-match-p "Bash exited with status 7" result))
      (should (string-match-p "STDOUT:\nstdout" result))
      (should (string-match-p "STDERR:\nstderr" result)))))

(ert-deftest gptel-pi-bash-archives-full-output-and-keeps-tail ()
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-pi-session-p t)
    (gptel-pi--initialize-org-session)
    (let ((gptel-pi-max-tool-bytes 35)
          (gptel-pi-max-tool-lines 20))
      (let ((result (gptel-pi--format-bash-result
                     (current-buffer) "make test" default-directory
                     "old output that is omitted\n" "final error\n"
                     'exit 1 300)))
        (should (string-match-p "Bash exited with status 1" result))
        (should (string-match-p "final error" result))
        (should (string-match-p "Full output: \\[\\[gptel-pi-tool-0001\\]" result))
        (should (string-match-p "old output that is omitted" (buffer-string)))))))

(ert-deftest gptel-pi-bash-reports-timeout ()
  (should (string-prefix-p
           "Bash timed out after 5 seconds (status 124)."
           (gptel-pi--format-bash-result
            (current-buffer) "sleep 10" default-directory "" "" 'exit 124 5))))

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

(ert-deftest gptel-pi-context-lineage-excludes-archive-and-sibling-branches ()
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-pi-session-p t
                gptel-org-branching-context t)
    (insert "* Archive\nARCHIVED HUGE OUTPUT\n"
            "* Conversation\ncommon\n"
            "** Branch A\nold branch\n"
            "** Branch B\nactive branch\n")
    (goto-char (point-max))
    (let ((active (gptel-pi--active-org-lineage-string)))
      (should (string-match-p "Conversation" active))
      (should (string-match-p "common" active))
      (should (string-match-p "Branch B" active))
      (should (string-match-p "active branch" active))
      (should-not (string-match-p "ARCHIVED" active))
      (should-not (string-match-p "Branch A" active)))))

(ert-deftest gptel-pi-context-estimate-uses-model-window-metadata ()
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-pi-session-p t
                gptel-org-branching-context t
                gptel-system-prompt ""
                gptel-model 'gptel-pi-test-model)
    (put 'gptel-pi-test-model :context-window 1)
    (insert "* Conversation\n" (make-string 1500 ?x))
    (goto-char (point-max))
    (let ((gptel-pi-context-chars-per-token 3.0))
      (gptel-pi--update-context-estimate)
      (should (= gptel-pi--context-percent 50))
      (should (equal (gptel-pi--context-mode-line) " Pi ctx 50%")))))

(ert-deftest gptel-pi-context-mode-line-warning-levels ()
  (with-temp-buffer
    (setq-local gptel-pi--context-percent 79)
    (should (equal (gptel-pi--context-mode-line) " Pi ctx 79%!"))
    (setq gptel-pi--context-percent 93)
    (should (equal (gptel-pi--context-mode-line) " Pi ctx 93%!!"))))

(ert-deftest gptel-pi-tool-fingerprint-normalizes-plist-order ()
  (should (equal (gptel-pi--tool-fingerprint "read" '(:path "x" :offset 2))
                 (gptel-pi--tool-fingerprint "read" '(:offset 2 :path "x")))))

(ert-deftest gptel-pi-tool-budget-stops-after-hard-limit ()
  (with-temp-buffer
    (gptel-pi--reset-tool-state)
    (let ((gptel-pi-max-tool-calls 2)
          (gptel-pi-max-identical-tool-calls 10))
      (should-not (gptel-pi--pre-tool-call '(:name "read" :args (:path "one"))))
      (should-not (gptel-pi--pre-tool-call '(:name "read" :args (:path "two"))))
      (let ((stop (gptel-pi--pre-tool-call '(:name "read" :args (:path "three")))))
        (should (plist-get stop :stop))
        (should (string-match-p "2-call tool budget" (plist-get stop :stop-reason)))))))

(ert-deftest gptel-pi-tool-budget-stops-repeated-normalized-call ()
  (with-temp-buffer
    (gptel-pi--reset-tool-state)
    (let ((gptel-pi-max-tool-calls 20)
          (gptel-pi-max-identical-tool-calls 3))
      (should-not
       (gptel-pi--pre-tool-call '(:name "read" :args (:path "x" :offset 2))))
      (should-not
       (gptel-pi--pre-tool-call '(:name "read" :args (:offset 2 :path "x"))))
      (let ((stop
             (gptel-pi--pre-tool-call
              '(:name "read" :args (:path "x" :offset 2)))))
        (should (plist-get stop :stop))
        (should (string-match-p "identical arguments (3 times)"
                                (plist-get stop :stop-reason)))))))

(ert-deftest gptel-pi-tool-budget-stops-repeated-failure ()
  (with-temp-buffer
    (gptel-pi--reset-tool-state)
    (let ((gptel-pi-max-identical-tool-errors 2)
          (failure '(:name "edit" :args (:path "x")
                    :result "Error: old text not found")))
      (should-not (gptel-pi--post-tool-call failure))
      (let ((stop (gptel-pi--post-tool-call failure)))
        (should (plist-get stop :stop))
        (should (string-match-p "same edit failure occurred 2 times"
                                (plist-get stop :stop-reason)))))))

(ert-deftest gptel-pi-tool-count-mode-line-resets ()
  (with-temp-buffer
    (gptel-pi--reset-tool-state)
    (let ((gptel-pi-max-tool-calls 1)
          (gptel-pi-max-identical-tool-calls 10))
      (gptel-pi--pre-tool-call '(:name "read" :args (:path "one")))
      (should (equal (gptel-pi--tool-mode-line) " Pi tools 1!"))
      (gptel-pi--reset-tool-state)
      (should-not (gptel-pi--tool-mode-line)))))

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
