
(setq gc-cons-threshold most-positive-fixnum)
(load (expand-file-name (locate-user-emacs-file "init-packages.el")))
(load (expand-file-name (locate-user-emacs-file "init-my-global-settings.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("5e39e95c703e17a743fb05a132d727aa1d69d9d2c9cde9353f5350e545c793d4"
     "6bdc4e5f585bb4a500ea38f563ecf126570b9ab3be0598bdf607034bb07a8875"
     "d35afe834d1f808c2d5dc7137427832ccf99ad2d3d65d65f35cc5688404fdf30"
     "53a4efdca4c9fb870c3f92e4cfca0fbb638bb29b168a26a363298f9b1d9b9bcf"
     "77f281064ea1c8b14938866e21c4e51e4168e05db98863bd7430f1352cab294a"
     "4c92d278dc295b63daf817d668523d442058d6c90728958dc92b6bc976fffd96"
     "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "b29ba9bfdb34d71ecf3322951425a73d825fb2c002434282d2e0e8c44fce8185"
     "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850"
     "00c5138bb71c95ca37a0fc845656498a8b4ff271ba4e0e0845732d188359d55a"
     "e09401ab2c457e2e4d8b800e1c546dbc8339dc33b2877836ba5d9b6294ae6e55"
     "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2"
     "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c"
     "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7"
     "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae"
     "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b"
     "6c4c97a17fc7b6c8127df77252b2d694b74e917bab167e7d3b53c769a6abb6d6"
     "db152b961f7e6075f226a24bba7faf5b1ff016a0e614afe4e544df5ae2637b3c"
     "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e"
     "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088"
     "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476"
     "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))
 '(darkroom-mode-enable-fullscreen nil)
 '(doc-view-continuous t)
 '(frame-background-mode 'light)
 '(nxml-child-indent 4)
 '(nxml-outline-child-indent 4)
 '(oca-host
   "code-internal.aiservice.ca-montreal-1.oci.oraclecloud.com/20250519/app/litellm")
 '(proced-auto-update-interval 2)
 '(safe-local-variable-values
   '((eval progn (setenv "MX_BUILD_SHALLOW_DEPENDENCY_CHECKS" "true")
           (setenv "MX_OUTPUT_ROOT_INCLUDES_CONFIG" "false"))
     (lsp-java-autobuild-enabled . t) (lsp-java-save-actions-organize-imports)
     (py-indent-offset . 4) (lsp-pyright-extra-paths . "../../mx/src/")
     (smie-indent-basic . 4) (smie-indent-basic . 4) (smie-indent-basic . 4)
     (jsonnet-indent-level . 4)
     (flycheck-disabled-checkers emacs-lisp-checkdoc)
     (reftex-default-bibliography "fallback.bib") (whitespace-line-column . 80)
     (eval ignore-errors
           "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook 'write-contents-functions
                     (lambda nil (delete-trailing-whitespace) nil))
           (require 'whitespace)
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0) (whitespace-mode 1))
     (whitespace-style face tabs trailing lines-tail) (mode . org)
     (encoding . us-ascii) (encoding . utf-8)))
 '(scroll-bar-mode nil)
 '(secondmate-url "https://lively-kernel.org/swacopilot")
 '(sgml-basic-offset 4)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(warning-suppress-types '((comp))))
;; resize the windows on emacs and run ecb-store-window-sizes
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers ((t (:background "#191919"))))
 '(lsp-ui-sideline-code-action ((t (:foreground "firebrick")))))

(setq gc-cons-threshold (* 1024 1024 100)) ; 100 MiB
