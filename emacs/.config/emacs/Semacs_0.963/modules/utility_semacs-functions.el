
;;; Semacs-Insert-Date
;; insert date

(defun semacs/today (&optional arg)
"Insert today's date.

A prefix ARG specifies how many days to move;
negative means previous day.

If region selected, parse region as today's date pivot."
  (interactive "P")
  (let ((date (if (use-region-p)
                  (ts-parse (buffer-substring-no-properties (region-beginning) (region-end)))
                (ts-now)))
        (arg (or arg 0)))
    (if (use-region-p)
        (delete-region (region-beginning) (region-end)))
    (insert (ts-format "%A, %B %e, %Y" (ts-adjust 'day arg date)))))


;;; Semacs-Insert-File-Name
;; Take current filename (word separated by dash) as heading.

(defun semacs/insert-filename-as-heading ()
  "Take current filename (word separated by dash) as heading."
  (interactive)
  (insert
   (capitalize
    (replace-regexp-in-string "-" " " (file-name-sans-extension (buffer-name))))))


;;; Semacs-Copy-File-Name

(defun semacs/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))



;;; Semacs-Split-Window

(defun semacs/window-split()
  (interactive)
  (switch-to-buffer-other-window "*scratch*")
  (dired "~/"))


;;; Semacs-Create folders
;; Offer to create parent directories if they do not exist

(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)



;;; Semacs-Toggle-Window-Split
;; Horizontally to vertically and vice versa

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


;;; Semacs-Vim's "%"
;; https://stackoverflow.com/questions/8627725/matching-braces-in-emacs/11552138#11552138
;; Emulating vi’s % key
;; One of the few things I missed in Emacs from vi was the % key, which jumps to the parenthesis, bracket or brace which matches the one below the cursor. This function implements this functionality, bound to the same key. Inspired by NavigatingParentheses, but modified to use smartparens instead of the default commands, and to work on brackets and braces.

(defun px-match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert <key>."
  (interactive "p")
  (cond
   ((char-equal 41 (char-before)) (backward-list 1))
   ((char-equal 125 (char-before)) (backward-list 1))
   ((and
     (char-equal 123 (char-before))
     (char-equal 10 (char-after)))
    (backward-char 1) (forward-list 1))
   ((looking-at "\\s\(") (forward-list 1))
   ((looking-at "\\s\)") (backward-list 1))
   (t (self-insert-command (or arg 1)))))


;;; Semacs-Align-Text
;; Emacs has a flexible tool, align-regexp, for aligning text but it is surprisingly fiddly to use. For example to align a section of text like this:

;; the quick brown fox
;; jumped over the lazy
;; dogs the quick brown
;; into columns like this:

;; the     quick  brown  fox
;; jumped  over   the    lazy
;; dogs    the    quick  brown
;; you would highlight the text and use C-u M-x align-regexp \(\s-*\)\s- RET 1 RET 0 RET y. See what I mean!

;; The function is of course documented (use C-h f align-regexp to read it), but I found it a bit hard to follow. The \(\s-*\)\s- string is the regular expression that is used to align on, and the final \s- in that string tells emacs to align on a whitespace character. You could replace that with e.g. & to align on & characters. The other three options (i) control how the columns are justified (generally can leave this as 1); (ii) add spaces between columns; and (iii) repeat the alignment throughout the line.

;; To make life easier, I wrote a couple of simple wrappers around align-regexp for common tasks. The first aligns on whitespace, and the second aligns on & (useful for LaTeX tables).

(defun semacs/align-whitespace (start end)
  "Align columns by whitespace"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun semacs/align-& (start end)
  "Align columns by ampersand"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)&" 1 1 t))


;; Indirect Buffer
;; https://emacs.stackexchange.com/questions/40637/how-to-reuse-org-tree-indirect-buffer-windows

(defun semacs/org-indirect-buffer ()
  (interactive)
  (let ((ind-buf (concat (buffer-name) "-narrowclone")))
    (if (get-buffer ind-buf)
        (kill-buffer ind-buf))
    (clone-indirect-buffer-other-window ind-buf t)
    ;;(org-narrow-to-subtree)
    (switch-to-buffer ind-buf)))


;; ** Kill-All-Buffers

(defun semacs/nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))
