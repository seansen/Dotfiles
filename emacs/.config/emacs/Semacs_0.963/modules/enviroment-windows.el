;;; Home-Directory
;;; Unix tools look for HOME, but this is normally not defined on Windows.

(when (and IS-WINDOWS (null (getenv-internal "HOME")))
  (setenv "HOME" (getenv "USERPROFILE"))
  (setq abbreviated-home-dir nil))


;;; System-Variables
(if (eq system-type 'windows-nt)
(setenv  "PATH" (concat
  "c:/Windows/System32" ";"
  "c:/Windows/Microsoft.NET/Framework/v4.0.30319" ";"
  "c:/Users/averh/OneDrive/Home/Applications/Git/usr/bin" ";" ;Unix Tools
  ;"C:\\User\\arch\\bin" ";"                                  ;User binary files
  ;"c:\\Program Files\\Mono\\bin" ";"                         ;Mono Installation.
  ;"c:\\Program Files\\Mono\\lib\\mono\\4.5" ";"
 (getenv "PATH") )))

;;; Shells
(if (eq system-type 'windows-nt)
(defun run-bash ()
      (interactive)
      (let ((shell-file-name "~\\Applications\\Git\\bin\\bash.exe"))
            (shell "*bash*"))))

(if (eq system-type 'windows-nt)
(defun run-cmdexe ()
      (interactive)
      (let ((shell-file-name "cmd.exe"))
            (shell "*cmd.exe*"))))

(if (eq system-type 'windows-nt)
(defun run-powershell ()
  "Run powershell"
  (interactive)
  (async-shell-command "c:/windows/system32/WindowsPowerShell/v1.0/powershell.exe -Command -"
               nil
               nil)))
