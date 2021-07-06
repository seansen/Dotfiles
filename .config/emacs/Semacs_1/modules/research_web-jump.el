;******************************************
; WEB-JUMP
;******************************************

(eval-after-load "webjump"
'(add-to-list 'webjump-sites
              '("Urban Dictionary" .
                [simple-query
                 "www.urbandictionary.com"
                 "http://www.urbandictionary.com/define.php?term="
                 ""])))
;-------------------------------------------------
(eval-after-load "webjump"
'(add-to-list 'webjump-sites
              '("Melpa" .
                [simple-query
                 "https://melpa.org"
                 "http://melpa.org/#/?q="
                 ""])))
;-------------------------------------------------
(eval-after-load "webjump"
'(add-to-list 'webjump-sites
              '("Youtube" .
                [simple-query
                 "https://youtube.com"
                 "https://youtube.com/results?search_query="
                 ""])))


