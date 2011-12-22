
(defcustom w3c-idle-timer-delay 0.2
  "Delay in secs before re-parsing after user makes changes.
Multiplied by `w3c-dynamic-idle-timer-adjust', which see."
  :type 'number
  :group 'w3c-mode)
(make-variable-buffer-local 'w3c-idle-timer-delay)
(defcustom w3c-dynamic-idle-timer-adjust 0
  "Positive to adjust `w3c-idle-timer-delay' based on file size.
The idea is that for short files, parsing is faster so we can be
more responsive to user edits without interfering with editing.
The buffer length in characters (typically bytes) is divided by
this value and used to multiply `w3c-idle-timer-delay' for the
buffer.  For example, a 21k file and 10k adjust yields 21k/10k
== 2, so w3c-idle-timer-delay is multiplied by 2.
If `w3c-dynamic-idle-timer-adjust' is 0 or negative,
`w3c-idle-timer-delay' is not dependent on the file size."
  :type 'number
  :group 'w3c-mode)


(defmacro w3c-deflocal (name value &optional comment)
  "Define a buffer-local variable NAME with VALUE and COMMENT."
  `(progn
     (defvar ,name ,value ,comment)
     (make-variable-buffer-local ',name)))

(w3c-deflocal w3c-mode-parse-timer nil "Private variable.")
(w3c-deflocal w3c-mode-parsing nil "Private variable.")

(defvar w3c-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    table)
  "Syntax table used in w3c-mode buffers.")

(defvar w3c-mode-map
  (let ((map (make-sparse-keymap))
        keys)
    (define-key map [mouse-1] #'js2-mode-show-node)
    (define-key map [menu-bar javascript]
      (cons "JavaScript" (make-sparse-keymap "JavaScript")))

    (define-key map [menu-bar javascript customize-js2-mode]
      '(menu-item "Customize js2-mode" js2-mode-customize
                  :help "Customize the behavior of this mode"))
    map)
  "Keymap used in `w3c-mode' buffers.")

(defvar w3c-mode-abbrev-table nil
  "Abbrev table in use in `w3c-mode' buffers.")
(define-abbrev-table 'w3c-mode-abbrev-table ())


(defun w3c-indent-line ()
  "Indent the current line according to W3C context."
  (interactive)
  )
(defun w3c-indent-region (start end)
  "Indent the region, but don't use bounce indenting."
  )
  ; nil for byte-compiler


(defvar w3c-comment-prefix-regexp
  "//+\\|\\**")

(defvar w3c-paragraph-start
  "\\(@[a-zA-Z]+\\>\\|$\\)")

(defvar w3c-comment-start-skip
  "\\(//+\\|/\\*+\\)\\s *")

(defvar w3c-syntactic-ws-start
  "\\s \\|/[*/]\\|[\n\r]\\|\\\\[\n\r]\\|\\s!\\|<!--\\|^\\s-*-->")

(defvar w3c-syntactic-ws-end
  "\\s \\|[\n\r/]\\|\\s!")

(defvar w3c-syntactic-eol
  (concat "\\s *\\(/\\*[^*\n\r]*"
          "\\(\\*+[^*\n\r/][^*\n\r]*\\)*"
          "\\*+/\\s *\\)*"
          "\\(//\\|/\\*[^*\n\r]*"
          "\\(\\*+[^*\n\r/][^*\n\r]*\\)*$"
          "\\|\\\\$\\|$\\)")
  "Copied from `java-mode'.  Needed for some cc-engine functions.")

(defvar w3c-mode-hook nil)

(defun w3c-mode ()
  "Major mode for editing W3C file(HTML/CSS/JS/PHP?/JSP?."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table w3c-mode-syntax-table)
  (use-local-map w3c-mode-map)
  (setq major-mode 'w3c-mode
        mode-name "W3C-IDE"
        comment-start "/*"  ; used by comment-region; don't change it
        comment-end "*/")
  (setq local-abbrev-table w3c-mode-abbrev-table)
  (set (make-local-variable 'max-lisp-eval-depth)
       (max max-lisp-eval-depth 3000))
  (set (make-local-variable 'indent-line-function) #'w3c-indent-line)
  (set (make-local-variable 'indent-region-function) #'w3c-indent-region)

  (set (make-local-variable 'fill-paragraph-function) #'c-fill-paragraph)

  (set (make-local-variable 'before-save-hook) #'w3c-before-save)
  (set (make-local-variable 'next-error-function) #'w3c-next-error)
  (set (make-local-variable 'beginning-of-defun-function) #'w3c-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) #'w3c-end-of-defun)

  ;; we un-confuse `parse-partial-sexp' by setting syntax-table properties
  ;; for characters inside regexp literals.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  ;; this is necessary to make `show-paren-function' work properly
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; needed for M-x rgrep, among other things
  (put 'w3c-mode 'find-tag-default-function #'w3c-mode-find-tag)

  ;; some variables needed by cc-engine for paragraph-fill, etc.
  (setq c-buffer-is-cc-mode t
        c-comment-prefix-regexp w3c-comment-prefix-regexp
        c-comment-start-regexp "/[*/]\\|\\s|"
        c-paragraph-start w3c-paragraph-start
        c-paragraph-separate "$"
        comment-start-skip w3c-comment-start-skip
        c-syntactic-ws-start w3c-syntactic-ws-start
        c-syntactic-ws-end w3c-syntactic-ws-end
        c-syntactic-eol w3c-syntactic-eol)

  ;; (setq js2-default-externs
  ;;       (append js2-ecma-262-externs
  ;;               (if js2-include-browser-externs
  ;;                   js2-browser-externs)
  ;;               (if js2-include-gears-externs
  ;;                   js2-gears-externs)
  ;;               (if js2-include-rhino-externs
  ;;                    js2-rhino-externs)))

  ;; We do our own syntax highlighting based on the parse tree.
  ;; However, we want minor modes that add keywords to highlight properly
  ;; (examples:  doxymacs, column-marker).  We do this by not letting
  ;; font-lock unfontify anything, and telling it to fontify after we
  ;; re-parse and re-highlight the buffer.  (We currently don't do any
  ;; work with regions other than the whole buffer.)
  (dolist (var '(font-lock-unfontify-buffer-function
                 font-lock-unfontify-region-function))
    (set (make-local-variable var) (lambda (&rest args) t)))

  ;; Don't let font-lock do syntactic (string/comment) fontification.
  (set (make-local-variable #'font-lock-syntactic-face-function)
       (lambda (state) nil))

  ;; Experiment:  make reparse-delay longer for longer files.
  (if (plusp w3c-dynamic-idle-timer-adjust)
      (setq w3c-idle-timer-delay
            (* js2-idle-timer-delay
               (/ (point-max) js2-dynamic-idle-timer-adjust))))

  (add-hook 'change-major-mode-hook #'w3c-mode-exit nil t)
  (add-hook 'after-change-functions #'w3c-mode-edit nil t)
  (setq imenu-create-index-function #'w3c-mode-create-imenu-index)
  (imenu-add-to-menubar (concat "IM-" mode-name))
  ;; (when js2-mirror-mode
  ;;   (js2-enter-mirror-mode))
  ;; (add-to-invisibility-spec '(js2-outline . t))
  (set (make-local-variable 'line-move-ignore-invisible) t)
  ;; (set (make-local-variable 'forward-sexp-function) #'w3c-mode-forward-sexp)
  (setq w3c-mode-functions-hidden nil
        w3c-mode-comments-hidden nil
        w3c-mode-buffer-dirty-p t
        w3c-mode-parsing nil)
  (w3c-reparse)
  (run-hooks 'w3c-mode-hook))


(defun w3c-reparse (&optional force)
  (message "Parsed OK %S" (current-time-string))
  t)

(defun w3c-mode-create-imenu-index ()
  nil)

(defun w3c-mode-edit (beg end len)
  "Schedule a new parse after buffer is edited.
Buffer edit spans from BEG to END and is of length LEN.
Also clears the `w3c-magic' bit on autoinserted parens/brackets
if the edit occurred on a line different from the magic paren."

  ;; (let* ((magic-pos (next-single-property-change (point-min) 'js2-magic)) **/
  ;;        (line (if magic-pos (line-number-at-pos magic-pos)))) **/
  ;;   (and line **/
  ;;        (or (/= (line-number-at-pos beg) line) **/
  ;;            (and (> 0 len) **/
  ;;                 (/= (line-number-at-pos end) line))) **/
  ;;        (js2-mode-mundanify-parens))) **/
  ;; (setq js2-mode-buffer-dirty-p t) **/
  ;; (js2-mode-hide-overlay) **/
  ;; (js2-mode-reset-timer) **/
  (w3c-mode-reset-timer)
)

(defun w3c-before-save ()
  "Clean up whitespace before saving file.
You can disable this by customizing `w3c-cleanup-whitespace'."
  ;; (when js2-cleanup-whitespace **/
  ;;   (let ((col (current-column))) **/
  ;;     (delete-trailing-whitespace) **/
  ;;     ;\;\ don't change trailing whitespace on current line **/
  ;;     (unless (eq (current-column) col) **/
  ;;       (indent-to col)))) **/
  )

(defun w3c-mode-exit ()
  "Exit `w3c-mode' and clean up."
  (interactive))

(defsubst w3c-mode-reset-timer ()
  "Cancel any existing parse timer and schedule a new one."
  (if w3c-mode-parse-timer
      (cancel-timer w3c-mode-parse-timer))
  (setq w3c-mode-parsing nil)
  (setq w3c-mode-parse-timer
        (run-with-idle-timer w3c-idle-timer-delay nil #'w3c-reparse)))

(provide 'w3c-mode)
