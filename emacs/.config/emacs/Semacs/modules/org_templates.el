;(defun transform-square-brackets-to-round-ones(string-to-transform)
;  "Transforms [ into ( and ] into ), other chars left unchanged."
;  (concat (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) ;string-to-transform)))

(setq org-capture-templates
      '(("t" "Aufgabe in tasks.org" entry (file+headline "~/projects/org/tasks.org" "Inbox")
         "* TODO %?")
        ("w" "Waiting For Reply (Mail)" entry (file+headline "~/projects/org/tasks.org" "Inbox")
         "* WAITING Antwort auf %a")
        ("m" "Aufgabe aus Mail" entry (file+headline "~/projects/org/tasks.org" "Inbox")
         "* TODO %? , Link: %a")
        ("z" "Zeiteintrag in tasks.org" entry (file+headline "~/projects/org/tasks.org" "Inbox")
         "* ZKTO %? \n  %i" :clock-in t :clock-resume t)
        ("c" "Contacts" entry (file "~/projects/org/contacts.org")
         "* %(org-contacts-template-name) \n :PROPERTIES: %(org-contacts-template-email) \n :BIRTHDAY: \n :END:")
        ("j" "Journal" entry (file+datetree "~/projects/org/journal.org")
         "* %?\nEntered on %U\n  %i")
        ("p" "password" entry (file "~/projects/org/passwords.gpg")
         "* %^{Title}\n  %^{PASSWORD}p %^{USERNAME}p")))

;; Automatische Anpassung des Links zu einer E-Mail.
;; Alle Mails werden im Folder 'Archive' gespeichert.
(add-hook 'org-capture-prepare-finalize-hook 'hs/search)
(defun hs/search ()
  (interactive)
  (goto-char 1)
  (replace-string "INBOX" "Archive"))
