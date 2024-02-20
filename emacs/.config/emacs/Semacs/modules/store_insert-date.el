(defun saa/today (&optional arg)
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
