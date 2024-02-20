
(straight-use-package
 '(openwith :type git :host github :repo "jpkotta/openwith")
:config
    (when (require 'openwith nil 'noerror)
      (setq openwith-associations
            (list
             (list (openwith-make-extension-regexp
                    '("mpg" "mpeg" "mp3" "mp4"
                      "avi" "wmv" "wav" "mov" "flv"
                      "ogm" "ogg" "mkv"))
                   "vlc"
                   '(file))
             (list (openwith-make-extension-regexp
                    '("xbm" "pbm" "pgm" "ppm" "pnm"
                      "png" "gif" "bmp" "tif" "jpeg" "jpg"))
                   "geeqie"
                   '(file))
             (list (openwith-make-extension-regexp
                    '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
                   "libreoffice"
                   '(file))
             '("\\.lyx" "lyx" (file))
             '("\\.chm" "kchmviewer" (file))
             (list (openwith-make-extension-regexp
                    '("pdf" "ps" "ps.gz" "dvi"))
                   "okular"
                   '(file))
             ))
      (openwith-mode 1))
 )
;; https://github.com/jpkotta/openwith/tree/1dc89670822966fab6e656f6519fdd7f01e8301a
