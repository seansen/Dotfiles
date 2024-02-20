;; Org-Roam-Vulpea-Tag_People
;;When I mention someone in the task, I would love this task to be automatically tagged with that persons name.

(defun my-vulpea-insert-handle (note)
  "Hook to be called on NOTE after `vulpea-insert'."
  (when-let* ((title (vulpea-note-title note))
              (tags (vulpea-note-tags note)))
    (when (seq-contains-p tags "person")
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (when (eq 'todo (org-element-property
                           :todo-type
                           (org-element-at-point)))
            (org-set-tags
             (seq-uniq
              (cons
               (vulpea--title-to-tag title)
               (org-get-tags nil t))))))))))

(defun vulpea--title-to-tag (title)
  "Convert TITLE to tag."
  (concat "" (s-replace " " "" title)))

(add-hook 'vulpea-insert-handle-functions
          #'my-vulpea-insert-handle)

;; Org-Roam-Rename-Org-Buffer
;;https://org-roam.discourse.group/t/can-buffer-names-match-note-titles/350/7

(defun org+-buffer-name-to-title (&optional end)
  "Rename buffer to value of #+TITLE:.
If END is non-nil search for #+TITLE: at `point' and
delimit it to END.
Start an unlimited search at `point-min' otherwise."
  (interactive)
  (let ((beg (or (and end (point))
         (point-min))))
    (save-excursion
      (when end
    (goto-char end)
    (setq end (line-end-position)))
      (goto-char beg)
      (when (re-search-forward "^[[:space:]]*#\\+TITLE:[[:space:]]*\\(.*?\\)[[:space:]]*$" end t)
    (rename-buffer (match-string 1)))))
  nil)

(defun org+-buffer-name-to-title-config ()
  "Configure Org to rename buffer to value of #+TITLE:."
  (font-lock-add-keywords nil '(org+-buffer-name-to-title)))

(add-hook 'org-mode-hook #'org+-buffer-name-to-title-config)
;(add-hook 'org-roam-find-file-hook #'org+-buffer-name-to-title-config)
;(add-hook 'buffer-list-update-hook 'org+-buffer-name-to-title-config)


;; Org-Roam-Update-ID

;; I encountered the following message when attempting
;; to export data:
;;
;; "org-export-data: Unable to resolve link: FILE-ID"
(defun semacs/force-org-rebuild-cache ()
  "Rebuild the `org-mode' and `org-roam' cache."
  (interactive)
  (org-id-update-id-locations)
  ;; Note: you may need `org-roam-db-clear-all'
  ;; followed by `org-roam-db-sync'
  (org-roam-db-sync)
  (org-roam-update-org-id-locations))


;; Org-Roam-Exclude-Files-From-Search
  ;;The following snippet excludes all headlines with :fc: tag.
  (setq org-roam-db-node-include-function
    (defun rasen/org-roam-include ()
      ;; exclude org-fc headlines from org-roam
      (not (member "fc" (org-get-tags)))))

;; Org-Roam-Show-Node-Origin-In-Minibuffer
;;https://github.com/org-roam/org-roam/issues/1565
  (cl-defmethod org-roam-node-filetitle ((node org-roam-node))
    "Return the file TITLE for the node."
    (org-roam-get-keyword "TITLE" (org-roam-node-file node)))

  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    "Return the hierarchy for the node."
    (let ((title (org-roam-node-title node))
          (olp (org-roam-node-olp node))
          (level (org-roam-node-level node))
          (filetitle (org-roam-node-filetitle node)))
      (concat
       (if (> level 0) (concat filetitle "."))
       (if (> level 1) (concat (string-join olp ".") "."))
       title))
    )
 ;(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:40}" 'face 'org-tag)))
 (setq org-roam-node-display-template (concat "${hierarchy:*} " (propertize "${tags:40}" 'face 'org-tag)))


;; Org-Search-Files-By-Tag
(defun semacs/org-occur-tag-search (tag)
  (interactive
   (list (let ((org-last-tags-completion-table
                (if (derived-mode-p 'org-mode)
                    (org-uniquify
                     (delq nil (append (org-get-buffer-tags)
                                       (org-global-tags-completion-table))))
                  (org-global-tags-completion-table))))
           (org-icompleting-read
            "Tag: " 'org-tags-completion-function nil nil nil
            'org-tags-history))))
  (if tag (org-occur-in-agenda-files
           (concat ":" tag ":"))))


;; Org-Search-Folders-With-Ripgrep
(defun semacs/org-roam-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))
(global-set-key (kbd "C-c rr") 'bms/org-roam-rg-search)

;; Org-Read-Only
;;https://github.com/alphapapa/unpackaged.el#expand-all-options-documentation
;(set 'seansean " 🔒")

(defun unpackaged/org-next-heading-tagged (tag)
  "Move to beginning of next heading tagged with TAG and return point, or return nil if none found."
  (when (re-search-forward (rx-to-string `(seq bol (1+ "*") (1+ blank) (optional (1+ not-newline) (1+ blank))
                                               ;; Beginning of tags
                                               ":"
                                               ;; Possible other tags
                                               (0+ (seq (1+ (not (any ":" blank))) ":") )
                                               ;; The tag that matters
                                               ,tag ":"))
                           nil 'noerror)
    (goto-char (match-beginning 0))))

  ;;;###autoload
(defun unpackaged/org-mark-read-only ()
  "Mark all entries in the buffer tagged \"read_only\" with read-only text properties."
  (interactive)
  (setq seansean " 🔒")
  (message "Org-Read-Only True 🔒")
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (unpackaged/org-next-heading-tagged "read_only")
     (add-text-properties (point) (org-end-of-subtree t)
                          '(read-only t)))))

(defun unpackaged/org-remove-read-only ()
  "Remove read-only text properties from Org entries tagged \"read_only\" in current buffer."
  (interactive)
  (setq seansean " 🔏")
  (message "Org-Read-Only False 🔏")
  (let ((inhibit-read-only t))
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (unpackaged/org-next-heading-tagged "read_only")
       (remove-text-properties (point) (org-end-of-subtree t)
                               '(read-only t))))))
(add-hook 'org-mode-hook 'unpackaged/org-mark-read-only)

(setq global-mode-string (append global-mode-string '(seansean)))

;; Org-Agenda-Expand
;;https://github.com/alphapapa/unpackaged.el#expand-all-options-documentation
(defface unpackaged/org-agenda-preview
  '((t (:background "black")))
  "Face for Org Agenda previews."
  :group 'org)

;;;###autoload
(defun unpackaged/org-agenda-toggle-preview ()
  "Toggle overlay of current item in agenda."
  (interactive)
  (if-let* ((overlay (ov-in 'unpackaged/org-agenda-preview t (line-end-position) (line-end-position))))
      ;; Hide existing preview
      (ov-reset overlay)
    ;; Show preview
    (let* ((entry-contents (--> (org-agenda-with-point-at-orig-entry
                                 nil (buffer-substring (save-excursion
                                                         (unpackaged/org-forward-to-entry-content t)
                                                         (point))
                                                       (org-entry-end-position)))
                                s-trim
                                (concat "\n" it "\n"))))
      (add-face-text-property 0 (length entry-contents)
                              'unpackaged/org-agenda-preview nil entry-contents)
      (ov (line-end-position) (line-end-position)
          'unpackaged/org-agenda-preview t
          'before-string entry-contents))))

(defun unpackaged/org-forward-to-entry-content (&optional unsafe)
  "Skip headline, planning line, and all drawers in current entry.
If UNSAFE is non-nil, assume point is on headline."
  (unless unsafe
    ;; To improve performance in loops (e.g. with `org-map-entries')
    (org-back-to-heading))
  (cl-loop for element = (org-element-at-point)
           for pos = (pcase element
                       (`(headline . ,_) (org-element-property :contents-begin element))
                       (`(,(or 'planning 'property-drawer 'drawer) . ,_) (org-element-property :end element)))
           while pos
           do (goto-char pos)))


;; Org-My-Files
;;Cycle through important files
(setq my/org-files '("~/Org/Zettelkasten/notes.org" "~/Org/Zettelkasten/inbox.org" "~/Org/Zettelkasten/todo.org"))

(defun my/org-files (&optional unrestricted archives)
  "Get the list of agenda files.
Optional UNRESTRICTED means return the full list even if a restriction
is currently in place.
When ARCHIVES is t, include all archive files that are really being
used by the agenda files.  If ARCHIVE is `ifmode', do this only if
`org-agenda-archives-mode' is t."
  (let ((files
     (cond
      ((and (not unrestricted) (get 'my/org-files 'org-restrict)))
      ((stringp my/org-files) (org-read-agenda-file-list))
      ((listp my/org-files) my/org-files)
      (t (error "Invalid value of `my/org-files'")))))
    (setq files (apply 'append
               (mapcar (lambda (f)
                 (if (file-directory-p f)
                     (directory-files
                      f t org-agenda-file-regexp)
                   (list (expand-file-name f org-directory))))
                   files)))
    (when org-agenda-skip-unavailable-files
      (setq files (delq nil
            (mapcar (lambda (file)
                  (and (file-readable-p file) file))
                files))))
    (when (or (eq archives t)
          (and (eq archives 'ifmode) (eq org-agenda-archives-mode t)))
      (setq files (org-add-archive-files files)))
    files))

(defun semacs/org-cycle-files ()
  "Cycle through the files in `my/org-files'.
If the current buffer visits an agenda file, find the next one in the list.
If the current buffer does not, find the first agenda file."
  (interactive)
  (let* ((fs (or (my/org-files t)
         (user-error "No my/org-files files")))
     (files (copy-sequence fs))
     (tcf (and buffer-file-name (file-truename buffer-file-name)))
     file)
    (when tcf
      (while (and (setq file (pop files))
          (not (equal (file-truename file) tcf)))))
    (find-file (car (or files fs)))
    (when (buffer-base-buffer) (pop-to-buffer-same-window (buffer-base-buffer)))))


;;; Org-Capture-Shortcut
;;; function to capture a todo
(defun seamcs/org-capture-mail-follow ()
  (interactive)
  "Capture a TODO item"
  (org-capture nil "mf"))


(defun seamcs/org-capture-mail-readlater ()
  (interactive)
  "Capture a TODO item"
  (org-capture nil "mr"))

