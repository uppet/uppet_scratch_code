;; -*- lexical-binding: t; -*-

(defun pf-fffp1 (dir s)
  ;(message "searching in %s for %s" dir s)
  (let ((this-try (concat dir s)))
    (if (file-exists-p this-try)
        (find-file this-try)
      (if (file-name-parent-directory dir)
          (pf-fffp1 (file-name-parent-directory dir) s)
          (message (format "%s not found" s))))))
(defun pf-fffp (s)
  (interactive "s")
  (pf-fffp1 default-directory s))


(defun pf-split-string-by-substring (sub full)
  "Splits the string FULL into three parts based on the first occurrence of SUB.
Returns a list of (PREFIX SUB SUFFIX).
PREFIX is the part before SUB.
SUFFIX is the part after SUB.
If SUB is not found in FULL, returns nil."
  (let ((start (string-match-p (regexp-quote sub) full)))
    ;; Check if the substring was found
    (if start
        (let* (;; Get the length of the substring
               (sub-len (length sub))
               ;; Get the end position of the match
               (end (+ start sub-len))
               ;; Extract the prefix (from start of full up to start of match)
               (prefix (substring full 0 start))
               ;; Extract the suffix (from end of match to end of full)
               (suffix (substring full end nil)))
          ;; Return the list of (prefix sub suffix)
          (list sub prefix suffix))
      ;; Substring not found, return nil
      nil)))

(defun pf-ffp1-buffer (dir s)
  (let ((this-try (concat dir s)))
    (if (file-exists-p this-try)
        (save-excursion
          (find-file-literally this-try)
          (current-buffer))
      (if (file-name-parent-directory dir)
          (pf-ffp1-buffer (file-name-parent-directory dir) s)
        nil))))

(defun pf-loclist ()
  (let ((pf-list-buf (pf-ffp1-buffer default-directory "full_list.txt")))
    (switch-to-buffer (current-buffer))
    (if (not (eq pf-list-buf (current-buffer)))
        pf-list-buf
      nil)))

(defconst pf-limit 50 "limit the search count of pf-find")

(defun pf-head-col (buf word)
  (save-excursion
    (with-current-buffer buf
      (goto-char (point-min))
      (let ((ret '())
            (limit pf-limit))
        (while (not (eobp))
          (if (search-forward word (point-max) t)
              (progn (add-to-list
                      'ret (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                     (setq limit (1- limit))
                     (if (< limit 0)
                         (goto-char (point-max))
                       (goto-char (point-at-eol))))
            (goto-char (point-max)))
          )
        ret))))

(defun pf-collection (ctx)
  (let ((word (alist-get 'word ctx))
        (buf (pf-loclist)))
    (if buf
        (pf-head-col buf word))))

(defun pf-complete-test (v pred ctx)
  (let ((ret (seq-filter #'(lambda (x) (string-match (regexp-quote v) x))
                         (pf-collection (cons `(word . ,v) ctx)))))
    (if pred
        (seq-filter pred ret)
      ret)))

(defun pf-make-complete-aff (v)
  (lambda (completions)
    ;; (message "[[%s]]" completions)
    (seq-filter 'identity
                (mapcar (lambda (x) (pf-split-string-by-substring v x)) completions))))

(defun pf-make-complete-colfun (ctx)
  (lambda (rstring pred flag)
    (cond ((eq flag nil)
           (let ((ret
                  (pf-complete-test rstring pred ctx)))
             (cond ((eq ret nil) nil)
                   ((and (eq (length ret) 1) (eq (car ret) rstring)) t)
                   ((eq (length ret) 1)  (car ret))
                   (t rstring))))
          ((eq flag t)
           (pf-complete-test rstring pred ctx))
          ((eq flag 'lambda)
           (let ((ret
                  (pf-complete-test rstring pred ctx)))
             (cond ((eq ret nil) nil)
                   ((and (eq (length ret) 1) (eq (car ret) rstring)) t)
                   (t nil))))
          ((eq flag 'metadata)
           ;; (message "returning [%s]" (cons
           ;;                            'metadata
           ;;                            `((affixation-function
           ;;                               . ,(pf-make-complete-aff rstring)))))
           (cons 'metadata `((affixation-function
                                         . ,(pf-make-complete-aff rstring)))))
           ;; (cons 'metadata `((affixation-function
           ;;                               . ,#'make-complete-col-aff-fun))))
          ((and (consp flag) (eq (car flag) 'boundaries))
           (cons 'boundaries  (cons 0 0))))))


(defun pf-find ()
  (interactive)
  (pf-fffp (completing-read ":" (pf-make-complete-colfun '()))))

;;; (global-set-key (kbd "C-c p") 'pf-find)
