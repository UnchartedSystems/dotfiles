;; All credit for this goes to Gary Johnson! (lambdatronic @ gh)

(defun search-line (string)
  (save-excursion
    (beginning-of-line)
    (search-forward string (line-end-position) t)))

(defun search-line-for-require-keywords ()
  (let* ((as-sym-end      (search-line ":as"))
         (refer-sym-end   (search-line ":refer"))
         (as-sym-start    (if as-sym-end (- as-sym-end 3)))
         (refer-sym-start (if refer-sym-end (- refer-sym-end 6))))
    (cond
     ((and as-sym-start refer-sym-start)
      (min as-sym-start refer-sym-start))

     (as-sym-start
      as-sym-start)

     (refer-sym-start
      refer-sym-start))))

(defun get-ns-bounds ()
  (save-excursion
    ;; Jump to start of buffer
    (goto-char (point-min))
    ;; Save the start of the ns form
    (when-let ((ns-sym-end (search-forward "(ns " nil t))
               (ns-start   (- ns-sym-end 4)))
      ;; Jump to start of ns form
      (goto-char ns-start)
      ;; Jump to end of ns form
      (forward-sexp)
      ;; Save the end of the ns form
      (let ((ns-end (point)))
        (cons ns-start ns-end)))))

(defun align-clojure-ns ()
  (interactive)
  (save-excursion
    (when-let ((ns-bounds (get-ns-bounds))
               (ns-start  (car ns-bounds))
               (ns-end    (cdr ns-bounds)))
      ;; Jump to start of ns form
      (goto-char ns-start)
      ;; Collect the positions of all require clause keywords into keyword-positions
      (defvar keyword-positions '())
      (while (< (point) ns-end)
        (if-let ((keyword-start (search-line-for-require-keywords)))
            (push (cons (line-beginning-position) keyword-start) keyword-positions))
        (forward-line))
      ;; Calculate the maximum offset of each keyword from the beginning of its line
      (let ((max-offset (apply 'max
                               (mapcar (lambda (positions)
                                         (let ((bol (car positions))
                                               (bok (cdr positions)))
                                           (- bok bol)))
                                       keyword-positions))))
        ;; Align all keywords to this maximum offset
        (dolist (positions keyword-positions)
          (let* ((bol    (car positions))
                 (bok    (cdr positions))
                 (offset (- bok bol)))
            (when (< offset max-offset)
              (goto-char bok)
              (insert-char ?\s (- max-offset offset)))))))))
