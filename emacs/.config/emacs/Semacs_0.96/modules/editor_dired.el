(use-package emacs
  :preface
  (defun kill-all-dired-buffers ()
    "Kill all dired buffers."
    (interactive)
    (save-excursion
      (let ((count 0))
        (dolist (buffer (buffer-list))
          (set-buffer buffer)
          (when (equal major-mode 'dired-mode)
            (setq count (1+ count))
            (kill-buffer buffer)))
        (message "Killed %i dired buffer(s)." count))))

  (defun kill-dired-buffers ()
    (interactive)
     (mapc (lambda (buffer)
             (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
               (kill-buffer buffer)))
           (buffer-list)))

  :bind (:map dired-mode-map ("h" . wsl/open-in-default-program ))
  :config
  ;; auto refresh dired when file changes
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  :custom
  (dired-dwim-target t)
  (dired-guess-shell-alist-user '(("" "xdg-open")))
  (global-auto-revert-non-file-buffers t)
  (dired-find-file-other-buffer t)
  (auto-revert-verbose nil))


;; Dired-All-The-Icons
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))


;; Dired+
(use-package dired+
  :init
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-hide-details-propagate-flag nil)
    )


;; DiredF1
(use-package diredfl
  :hook
  (dired-mode . diredfl-mode))


;; Dired-Open

(use-package dired-open
  :custom
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (dired-open-extensions '(("png" . "feh")
                           ("mkv" . "mpv"))))


;; Dired-Hide-Dotfiles
(use-package dired-hide-dotfiles
  :config
  (dired-hide-dotfiles-mode)
  (define-key dired-mode-map "." 'dired-hide-dotfiles-mode))


;; Dired-Peep
(use-package peep-dired
  :ensure t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;; Dired Rainbow
(put 'dired-find-alternate-file 'disabled nil)

(use-package dired-rainbow
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    ))

;; [#B] Dired-Sidebar

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :init
    (add-hook 'dired-sidebar-mode-hook
      (lambda ()
        (unless (file-remote-p default-directory)
                (auto-revert-mode))))

  :config
    (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
    (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
    (setq dired-sidebar-subtree-line-prefix "__")
    (setq dired-sidebar-use-custom-font t)
    (define-key dired-sidebar-mode-map (kbd "M-p") nil))

;; Dired-Subtree
(use-package dired-subtree
  :config
    (setq dired-subtree-use-backgrounds nil)
    (let ((map dired-mode-map))
    (define-key map (kbd "<tab>") #'dired-subtree-toggle)
    (define-key map (kbd "<C-tab>") #'dired-subtree-cycle)
    (define-key map (kbd "<backtab>") #'dired-subtree-remove))
    (bind-keys :map dired-mode-map
      ("i" . dired-subtree-insert)
      (";" . dired-subtree-remove)))


;; Dired-Narrow
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))


