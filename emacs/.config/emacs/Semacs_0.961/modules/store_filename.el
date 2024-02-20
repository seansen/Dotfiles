;******************************************
; FILENAME-CLIPBOARD
;******************************************

;;; Copy to clipboard
(defun saa/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;; Copy to file
(defun saa/insert-filename-as-heading ()
  "Take current filename (word separated by dash) as heading."
  (interactive)
  (insert
   (capitalize
    (replace-regexp-in-string "-" " " (file-name-sans-extension (buffer-name))))))

