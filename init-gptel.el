;;; -*- lexical-binding: t -*-

(require 'use-package)

(use-package gptel
  :ensure t
  :defines (gptel--openai-models)
  :functions (gptel-make-openai-oauth)
  :commands (gptel)
  :pin melpa
  :custom
  (gptel-model 'gpt-5.6-luna)
  (gptel-default-mode 'org-mode)
  (gptel-log-level 'info)
  (gptel-org-branching-context t)
  :config
  (require 'gptel-openai)
  (require 'gptel-openai-oauth)
  (require 'gptel-request)

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@llm\n")

  (setq gptel-backend
        (gptel-make-openai-oauth "OpenAI" :models gptel--openai-models))

  (dolist (model-effort
           '((gpt-5.6-terra . "high")
             (gpt-5.6-luna  . "high")
             (gpt-5.6-sol   . "high")))
    (let ((model (car model-effort))
          (effort (cdr model-effort)))
      (setplist
       model
       (plist-put (symbol-plist model)
                  :request-params
                  `(:reasoning (:effort ,effort
                                :summary "detailed"))))))

  ;; currently:
  ;;
  ;;  llama-server -hf unsloth/Qwen3.6-35B-A3B-GGUF:UD-Q4_K_XL --alias \
  ;;    local-code-model --port 8080 --host 0.0.0.0 --ctx-size 65536 --flash-attn \
  ;;    on --cache-type-k q8_0 --cache-type-v q8_0 --jinja -np 1 --reasoning off
  ;;
  ;;  llama-server -hf unsloth/gemma-4-E4B-it-qat-GGUF:UD-Q4_K_XL --alias \
  ;;    local-chat-model --port 8080 --host 0.0.0.0 --ctx-size 65536 --flash-attn \
  ;;    on --cache-type-k q8_0 --cache-type-v q8_0 --jinja -np 1 --temp 1.0 \
  ;;    --top-p 0.95 --top-k 64
  (gptel-make-openai "llama-cpp"
    :host "127.0.0.1:8080"
    :protocol "http"
    :stream t
    :models '(local-code-model local-chat-model)
    :key "none")

  (setq gptel-directives
        (let* ((promptdir (expand-file-name "prompts" user-emacs-directory))
               (prompt-files (directory-files promptdir t "\\.md\\'")))
          (mapcar
           (lambda (prompt-file)
             (with-temp-buffer
               (insert-file-contents prompt-file)
               (let* ((gmd (lambda (key)
                             (save-excursion
                               (goto-char (point-min))
                               (when (re-search-forward
                                      (format "^ *<!-- *#\\+%s: \\(.*?\\) *--> *$" key)
                                      nil t)
                                 (string-trim (match-string 1))))))
                      (prompt-name (or (funcall gmd "name") (file-name-base prompt-file)))
                      (prompt-description (or (funcall gmd "description") "NO DESCR")))
                 (goto-char (point-min))
                 (flush-lines "^ *<!--.*--> *$")
                 (goto-char (point-min))
                 (when (looking-at "\n+") (delete-region (point) (match-end 0)))
                 (list
                  (intern prompt-name)
                  prompt-description
                  (buffer-substring-no-properties (point-min) (point-max))))))
           prompt-files))))

(use-package gptel-pi
  :bind (("C-x a i" . gptel-pi)))

(use-package gptel-fim
  :bind (("C-x a c" . gptel-fim)))

(use-package gptel-compile
  :bind (("C-x a C" . gptel-compile)))

(use-package gptel-modeline-status
  :after gptel
  :demand t)

(use-package timfel-gptel-orchestration
  :commands timfel/weekly-confluence-report
  :bind (("C-x a m" . timfel/gptel-open-agents-orchestration)))
