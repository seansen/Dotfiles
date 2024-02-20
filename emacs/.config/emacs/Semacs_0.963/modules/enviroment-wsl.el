;;; WSL-Browser

(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))

  ;; get the system-type value
  (setq-default sysTypeSpecific  system-type)
  ;; If type is "gnu/linux", override to "wsl/linux" if it's WSL.
  (cond ((eq sysTypeSpecific 'gnu/linux)
    (when (string-match "Linux.*Microsoft.*Linux"
       (shell-command-to-string "uname -a"))
    (setq-default sysTypeSpecific "wsl/linux") ;; for later use.
    (setq
      cmdExeBin"/mnt/c/Windows/System32/cmd.exe"
      cmdExeArgs '("/c" "start" "") )
    (setq
      browse-url-generic-program  cmdExeBin
      browse-url-generic-args     cmdExeArgs
      browse-url-browser-function 'browse-url-generic)))))

;;; WSL-Open-File-In-Windows

(defmacro wsl--open-with (id &optional app dir)
  `(defun ,(intern (format "wsl/%s" id)) ()
     (interactive)
     (wsl-open-with ,app ,dir)))

(defun wsl-open-with (&optional app-name path)
  "Send PATH to APP-NAME on WSL."
  (interactive)
  (let* ((path (expand-file-name
                (replace-regexp-in-string
                 "'" "\\'"
                 (or path (if (derived-mode-p 'dired-mode)
                              (dired-get-file-for-visit)
                            (buffer-file-name)))
                 nil t)))
         (command (format "%s `wslpath -w %s`" (shell-quote-argument app-name) path)))
    (shell-command-to-string command)))

(wsl--open-with open-in-default-program "explorer.exe" buffer-file-name)
(wsl--open-with reveal-in-explorer "explorer.exe" default-directory)
