(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(

    ;; net abbrev
    ("afaik" "as far as i know" )
    ("atm" "at the moment" )
    ("dfb" "difference between" )
    ("ty" "thank you" )
    ("ui" "user interface" )
    ("uns" "understand" )
    ("ur" "you are" )
    ("btw" "by the way" )

    ("cnt" "can't" )
    ("ddnt" "didn't" )
    ("dnt" "don't" )
    ("Dnt" "Don't" )

    ;; english word abbrev
    ("ann" "announcement" )
    ("arg" "argument" )
    ("autom" "automatic" )
    ("bc" "because" )
    ("bg" "background" )
    ("bt" "between" )
   ("math" "mathematics" )

    ;; deutsch
    ("sgdh" "Sehr geehrte Damen und Herren,\n")
    ("sgh" "Sehr geehrter Herr")
    ("sgd" "Sehr geehrte Frau")
    ("mfg" "Mit freundlichen Grüßen")

    ;; computing
    ("ahk" "AutoHotkey" )
    ("cfg" "context-free grammar" )
    ("cj" "Clojure" )
    ("cs" "computer science" )

    ;; tech company
    ("gc" "Google Chrome" )
    ("gm" "Google Map" )
    ("macos" "Mac OS" )
    ("msw" "Microsoft Windows" )

    ;; programing
    ("ev" "environment variable" )
    ("ipa" "IP address" )
    ("subdir" "sub-directory" )
    ("wd" "web development" )

    ("db" "database" )
    ("gui3" "graphical user interface" )
    ("oop3" "object oriented programing" )

    ("os3" "operating system" )

    ;; programing
    ("eq" "==" )
    ("r" "return" )
    ("utf8" "-*- coding: utf-8 -*-" )

    ;; regex
    ("xaz" "\\([A-Za-z0-9]+\\)" )

    ;; unicode
    ("md" "—" )

    ("hr" "--------------------------------------------------" )
    ("bu" "•" )
    ("ra" "→" )

    ;; url
    ("urlemacs" "http://ergoemacs.org/" )

    ;;
    ))

;; define abbrev for specific major mode
;; the first part of the name should be the value of the variable major-mode of that mode
;; e.g. for go-mode, name should be go-mode-abbrev-table

(when (boundp 'go-mode-abbrev-table)
  (clear-abbrev-table go-mode-abbrev-table))

(define-abbrev-table 'go-mode-abbrev-table
  '(
    ("p" "fmt.Printf(\"%v\\n\", hh▮)")
    ))
