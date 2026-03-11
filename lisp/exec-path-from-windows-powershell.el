;;; exec-path-from-windows-powershell.el --- import Windows env into Emacs via PowerShell -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup exec-path-from-windows-powershell nil
  "Import environment variables from PowerShell into Emacs."
  :group 'environment)

(defcustom exec-path-from-windows-powershell-powershell-exe
  (or (executable-find "pwsh")
      (executable-find "powershell")
      "powershell")
  "PowerShell executable to use."
  :type 'string)

(defcustom exec-path-from-windows-powershell-load-profile t
  "If non-nil, load the PowerShell profile (do not pass -NoProfile)."
  :type 'boolean)

(defcustom exec-path-from-windows-powershell-use-vsdev-env t
  "If non-nil, also import a Visual Studio Developer environment."
  :type 'boolean)

(defcustom exec-path-from-windows-powershell-vs-arch "x64"
  "Architecture for VsDevCmd (e.g. x64, x86, arm64)."
  :type 'string)

(defcustom exec-path-from-windows-powershell-vs-host-arch "x64"
  "Host architecture for VsDevCmd (e.g. x64, x86)."
  :type 'string)

(defun exec-path-from-windows-powershell--ps-quote (s)
  "Quote S as a PowerShell single-quoted string."
  (concat "'" (replace-regexp-in-string "'" "''" s t t) "'"))

(defun exec-path-from-windows-powershell--call-powershell (ps-script)
  "Run PowerShell with PS-SCRIPT and return stdout.
Uses a temp .ps1 file to avoid Windows command-line length limits."
  (let* ((exe exec-path-from-windows-powershell-powershell-exe)
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (temp-ps1 (make-temp-file "emacs-env-" nil ".ps1" ps-script))
         (args (append
                (list "-ExecutionPolicy" "Bypass" "-NonInteractive")
                (unless exec-path-from-windows-powershell-load-profile
                  (list "-NoProfile"))
                (list "-File" temp-ps1))))
    (unwind-protect
        (with-temp-buffer
          (let ((exit (apply #'call-process exe nil (current-buffer) nil args)))
            (unless (eq exit 0)
              (error "PowerShell failed (exit %s): %s" exit (buffer-string)))
            (buffer-string)))
      (ignore-errors (delete-file temp-ps1)))))

(defun exec-path-from-windows-powershell--parse-env-block (text)
  "Parse TEXT of the form KEY=VALUE per line into an alist."
  (let (alist)
    (dolist (line (split-string text "\r?\n" t))
      (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" line)
        (push (cons (match-string 1 line) (match-string 2 line)) alist)))
    (nreverse alist)))

(defun exec-path-from-windows-powershell--apply-env (alist)
  "Apply ALIST of (NAME . VALUE) to Emacs process environment and related vars."
  (dolist (kv alist)
    (setenv (car kv) (cdr kv)))

  (let* ((path (or (getenv "PATH") ""))
         (paths (cl-remove-if #'string-empty-p (split-string path ";" t))))
    (setq eshell-path-env path)
    (setq exec-path (append paths (list exec-directory)))))

(defun exec-path-from-windows-powershell--ps-script-base-env ()
  "PowerShell script to print all env vars after profile loads."
  (concat
   "$ErrorActionPreference='Stop';"
   "& $function:Prompt;"
   ;; Enumerate environment via .NET to avoid Env: provider duplicate-key bug.
   "[System.Environment]::GetEnvironmentVariables()"
   " | ForEach-Object { \"$($_.Key)=$($_.Value)\" }"))

(defun exec-path-from-windows-powershell--ps-script-vsdev-env ()
  (let ((arch exec-path-from-windows-powershell-vs-arch)
        (host exec-path-from-windows-powershell-vs-host-arch))
    (concat
     "$ErrorActionPreference='Stop';"
     "$vswhere = Join-Path ${env:ProgramFiles(x86)} 'Microsoft Visual Studio\\Installer\\vswhere.exe';"
     "if (-not (Test-Path $vswhere)) { throw \"vswhere not found: $vswhere\" };"
     "$install = & $vswhere -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath;"
     "if (-not $install) { throw 'No suitable Visual Studio/BuildTools installation found.' };"
     "$vsdev = Join-Path $install 'Common7\\Tools\\VsDevCmd.bat';"
     "$vcvars = Join-Path $install 'VC\\Auxiliary\\Build\\vcvarsall.bat';"
     "$cmdLine = $null;"
     "if (Test-Path $vsdev) {"
     "  $cmdLine = ('call \"' + $vsdev + '\" -no_logo -arch=" arch " -host_arch=" host "');"
     "} elseif (Test-Path $vcvars) {"
     "  $cmdLine = ('call \"' + $vcvars + '\" " arch "');"
     "} else { throw \"Neither VsDevCmd.bat nor vcvarsall.bat found under: $install\" };"
     "$cmd = 'chcp 65001>nul & ' + $cmdLine + ' & set';"
     "& cmd.exe /d /s /c $cmd")))

(defun exec-path-from-windows-powershell--ps-script-vsdev-env ()
  (let ((arch exec-path-from-windows-powershell-vs-arch)
        (host exec-path-from-windows-powershell-vs-host-arch))
    (concat
     "$ErrorActionPreference='Stop';"
     "$vswhere = Join-Path ${env:ProgramFiles(x86)} 'Microsoft Visual Studio\\Installer\\vswhere.exe';"
     "if (-not (Test-Path $vswhere)) { throw \"vswhere not found: $vswhere\" };"
     "$install = & $vswhere -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath;"
     "if (-not $install) { throw 'No suitable Visual Studio/BuildTools installation found.' };"
     "$vsdev = Join-Path $install 'Common7\\Tools\\VsDevCmd.bat';"
     "$vcvars = Join-Path $install 'VC\\Auxiliary\\Build\\vcvarsall.bat';"

     "$cmdLine = $null;"
     "if (Test-Path $vsdev) {"
     "  $cmdLine = ('call \"' + $vsdev + '\" -no_logo -arch=" arch " -host_arch=" host "');"
     "} elseif (Test-Path $vcvars) {"
     "  $cmdLine = ('call \"' + $vcvars + '\" " arch "');"
     "} else { throw \"Neither VsDevCmd.bat nor vcvarsall.bat found under: $install\" };"

     ;; Use `&` instead of `&&` to avoid cmd parsing issues; also force UTF-8.
     "$cmd = 'chcp 65001>nul & ' + $cmdLine + ' & set';"
     "& cmd.exe /d /s /c $cmd"
     )))

;;;###autoload
(defun exec-path-from-windows-powershell-initialize (&optional with-vsdev)
  "Import environment from PowerShell into Emacs.
If WITH-VSDEV is non-nil (or `exec-path-from-windows-powershell-use-vsdev-env' is non-nil),
also import the Visual Studio Developer environment."
  (interactive "P")
  (when (eq system-type 'windows-nt)
    ;; 1) Import base env from PowerShell (profile included by default)
    (let* ((base-text (exec-path-from-windows-powershell--call-powershell
                       (exec-path-from-windows-powershell--ps-script-base-env)))
           (base-alist (exec-path-from-windows-powershell--parse-env-block base-text)))
      (exec-path-from-windows-powershell--apply-env base-alist))

    ;; 2) Optionally import VS dev env (overrides/additions)
    (when (or with-vsdev exec-path-from-windows-powershell-use-vsdev-env)
      (let* ((vs-text (exec-path-from-windows-powershell--call-powershell
                       (exec-path-from-windows-powershell--ps-script-vsdev-env)))
             (vs-alist (exec-path-from-windows-powershell--parse-env-block vs-text)))
        (exec-path-from-windows-powershell--apply-env vs-alist)))))

(provide 'exec-path-from-windows-powershell)
;;; exec-path-from-windows-powershell.el ends here
