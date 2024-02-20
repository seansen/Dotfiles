(use-package doct
  :straight  (:repo "progfolio/doct" :host github :type git))

(setq org-capture-templates
      (doct '(
              ("task" :keys "t"
               :file "~/Org/Zettelkasten/todo.org"
               ;:prepend t
               :template ("* %{todo-state}  %^{Description} %^{ort} %^G"
                          ":PROPERTIES:"
                          ":SCHEDULED: %U"
                          ":LOCATION: %{ort}"
                          ":Created: %U"
                          ":END:"
                          "%?")
               :children (
                          ("Appointment"  :keys "a"
                           :todo-state "MEETING"
                           :ort "%{Location}")
                          ("Todo"  :keys "t"
                           :todo-state "%^{prompt|TODO|NEXT|PROG|PROJ|INFO|WAIT}"
                           :Effort: "%^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n"
                           :ort "%{Location}")
                          ("ISB"  :keys "3"
                           :headline "ISB-Tuch"
                           :todo-state "%^{prompt|TODO|NEXT|PROG|PROJ|INFO|WAIT}"
                           :ort "@Home")))

              ("kontakt" :keys "k"
               :file "~/Org/Zettelkasten/contacts.org"
               :headline "Freunde"
               :template ("* @%(org-contacts-template-name)"
                           ":PROPERTIES:"
                           ":ADDRESS: %^{289 Cleveland St. Brooklyn, 11206 NY, USA}"
                           ":BIRTHDAY: %^{YYYY-MM-DD}"
                           ":PHONE: %^{Nummer}"
                           ":Email: %^{Email}"
                           ":CAPTURED: %<%Y-%m-%d %H:%M>"
                           ":END:")
               :children (
                          ("Freunde"  :keys "k"
                           :headline "Freunde")
                          ("ISB-Tuch"  :keys "i"
                           :headline "ISB-Tuch")))

              ("mail" :keys "m"
               :file "~/Org/Zettelkasten/mail.org"
               :headline "Freunde"
               :template ("* TODO %a %:fromname on %:subject\n%a\n\n%i"
                           ":PROPERTIES:"
                           ":CAPTURED: %<%Y-%m-%d %H:%M>"
                           ":END:")
               :children (
                          ("Follow Up"  :keys "f"
                           :headline "Follow Up")
                          ("Read Later"  :keys "r"
                           :headline "Read Later")))

              ("book" :keys "b"
               :file "~/Org/Zettelkasten/books.org"
               :headline "Books"
               :template ("* %^{Title}"
                          ":PROPERTIES:"
                          ":AUTHOR: %^{NAME, VORNAME}"
                          ":YEAR: %^{1999}"
                          ":PAGES: %^{321}"
                          ":RATING: %^{rating|*|**|***|****|*****}"
                          ":CAPTURED: %<%Y-%m-%d %H:%M>"
                          ":END:"))

              ("call" :keys "c"
               :file "~/Org/Zettelkasten/todo.org"
               ;:prepend t
               :template ("* CALL  %{name-state} :call:"
                          ":PROPERTIES:"
                          ":SCHEDULED: %U"
                          ":PHONE: %^{Phone}"
                          ":E-MAIL: %^{E-Mail}"
                          ":Created: %U"
                          ":END:"
                          ""
                          "%?")
               :children (
                          ("Call"  :keys "c"
                           :name-state "%^{Name}"
                           :ort "%{Location}")
                          ("ISB-Call"  :keys "i"
                           :olp ("ISB-Tuch" "Telefondienst Mai")
                           ;:olp ("ISB-Tuch" "Telefondienst %(car(cdr(split-string (current-time-string) " "))) ")
                           :name-state "%^{}"
                           :ort "%{Location}")))

              ("Clip Link" :keys "K"
               :file ""
               :template ("* %(org-cliplink-capture)"
                          ":SCHEDULED: %t")
               :empty-lines 1)

              ("Protocol Link" :keys "L"
               :file "~/Org/Zettelkasten/inbox.org"
               :headline "Inbox"
               :template ("* %?[[%:link][%:description]]"
                          ":Captured On: %U")
               :immediate-finish t)

              ("Protocol" :keys "p"
               :file "~/Org/Zettelkasten/inbox.org"
               :headline "Inbox"
               :template ("* %^{Description} "
                          ":PROPERTIES:"
                          ":LINK: %:link"
                          ":CAPTURED ON: %U"
                          ":END:"
                          "#+BEGIN_QUOTE\n%i\n#+END_QUOTE"
                          )
               :immediate-finish t)
              )))


(use-package org
  :config
  ;; Kill the frame if one was created for the capture
  (defvar kk/delete-frame-after-capture 0 "Whether to delete the last frame after the current capture")

  (defun kk/delete-frame-if-neccessary (&rest r)
    (cond
     ((= kk/delete-frame-after-capture 0) nil)
     ((> kk/delete-frame-after-capture 1)
      (setq kk/delete-frame-after-capture (- kk/delete-frame-after-capture 1)))
     (t
      (setq kk/delete-frame-after-capture 0)
      (delete-frame))))

  (advice-add 'org-capture-finalize :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-kill :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-refile :after 'kk/delete-frame-if-neccessary)

  ;;-----------------------------------------------
  :preface
  (defvar my/org-appointment
    (concat "* MEETING %^{Appointment} :appointment:\n"
            "SCHEDULED: %U\n"
            ;"%^{SCHEDULED}p"
            ":PROPERTIES:\n"
            ":LOCATION: %^{Ort}\n"
            ":END:") "Template for appointment task.")

  (defvar my/org-basic-task-template
     (concat "* %^{prompt|TODO|NEXT|PROG|PROJ|INFO|WAIT} %^{Task} %^G\n"
            ":PROPERTIES:\n"
            ":Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n"
            ":CAPTURED: %<%Y-%m-%d %H:%M>\n"
            ":END:") "Template for basic task.")

  (defvar my/org-isb-task-template
     (concat "* %^{prompt|TODO|NEXT|PROG|PROJ|INFO|WAIT} %^{Task} :work:isb:\n"
            ":PROPERTIES:\n"
            ":Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}\n"
            ":CAPTURED: %<%Y-%m-%d %H:%M>\n"
            ":END:") "Template for isb task.")

  (defvar my/org-book-template
    (concat "* %^{Title}\n"
            ":PROPERTIES:\n"
            ":AUTHOR: %^{NAME, VORNAME}\n"
            ":YEAR: %^{1999}\n"
            ":PAGES: %^{321}\n"
            ":RATING: %^{rating|*|**|***|****|*****}\n"
            ":CAPTURED: %<%Y-%m-%d %H:%M>\n"
            ":END:") "Template for a book.")

  (defvar my/org-contacts-template
    (concat "* %(org-contacts-template-name)\n"
            ":PROPERTIES:\n"
            ":ADDRESS: %^{289 Cleveland St. Brooklyn, 11206 NY, USA}\n"
            ":BIRTHDAY: %^{YYYY-MM-DD}\n"
            ":PHONE: %^{Nummer}\n"
            ":Email: %^{Email}\n"
            ":CAPTURED: %<%Y-%m-%d %H:%M>\n"
            ":END:") "Template for a contact.")

  (defvar my/org-phone-calls-template
    (concat "* %^{Name}\n"
            ":PROPERTIES:\n"
            ":PHONE: %^{Nummer}\n"
            ":Email: %^{Email}\n"
            ":CAPTURED: %<%Y-%m-%d %H:%M>\n"
            ":END: %^{prompt|** My Review}") "Template for a phone call.")

  :custom
  (org-capture-templates
   `    (

     ;; [x]
     ;; APPOINTMENT
     ("a" "Appointment" entry (file "~/Org/Zettelkasten/todo.org" ),
      my/org-appointment
      :empty-lines 1)

     ;; BOOK
     ("b" "BOOK ENTRY" entry (file+headline "~/Org/Zettelkasten/books.org" "Books"),
      my/org-book-template  %(org-set-tags \"book\")
      :immediate-finish t)

     ;; CALL
     ("c" "Call" entry (file+headline "~/Org/Zettelkasten/todo.org" "Telefonat"),
      my/org-phone-calls-template  :clock-in t :clock-resume t
      :empty-lines 1)

     ;; IDEE
     ("i" "Idee" entry (file+headline ,(concat org-directory "Zettelkasten/todo.org") "Idee")
     "* %^{Title}           %^g\n%^{Îdee} ")

     ;; CONTACT
     ("k" "Contact" entry (file+headline "~/Org/Zettelkasten/contacts.org" "Kontakte"),
      my/org-contacts-template
      :immediate-finish t)

     ;; LEARNING
     ("l" "Learning" checkitem (file+headline "~/Org/Zettelkasten/todo.org" "Lernen")
      "- [ ] %^{Thing} :@home:"
      :immediate-finish t)

     ;; PASSWORD
     ("s" "Password" entry (file "~/projects/org/passwords.org.gpg")
         "* %^{Title}\n  %^{PASSWORD}p %^{USERNAME}p")

     ;; BASIC TODO
     ("t" "Todo" entry (file "~/Org/Zettelkasten/todo.org"),
      my/org-basic-task-template
      :immediate-finish t)

     ;; [x]
     ;; WEBSEITEN LINK(ORG_CLIPLINK)
     ("K" "Cliplink capture task" entry (file "")
         "* %(org-cliplink-capture) \n  SCHEDULED: %t\n" :empty-lines 1)

     ;; [x]WEBSEITEN(ORG_CAPTURE)
     ;; ("p" "Protocol" entry (file+headline ,(concat org-directory "Zettelkasten/inbox.org") "Inbox")
     ;; "* %^{Title} \nSource: %t, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
     ;;  :immediate-finish t)
     ;; [x]
     ("p" "Protocol" entry (file+headline ,(concat org-directory "Zettelkasten/inbox.org") "Inbox")
     "* %^{Title}\nSource: %:link - %t, %c\n  #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
      :immediate-finish t)

     ;; [x]
     ;; WEBSEITEN(ORG_CAPTURE)
     ("L" "Protocol Link" entry (file+headline ,(concat org-directory "Zettelkasten/inbox.org") "Inbox")
     "* %?[[%:link][%:description]] %(progn (setq kk/delete-frame-after-capture 2) \"\")\nCaptured On: %U"
     :immediate-finish t)

     ;; ISB-TUCH
     ("x" "New Item")

     ("xc" "Call" entry (file+headline "~/Org/Zettelkasten/todo.org"  "[[id:ad5a4a76-1a4a-4ff4-82c4-666077b44953][@ISB-Tuch]]\n** Telefondienst"),
      my/org-phone-calls-template  :clock-in t :clock-resume t
      :empty-lines 1)

     ("xi" "ISB-TASK" entry (file+headline "~/Org/Zettelkasten/todo.org" "[[id:ad5a4a76-1a4a-4ff4-82c4-666077b44953][@ISB-Tuch]]"),
      my/org-isb-task-template
      :immediate-finish t)

     ))))
