
;;; portable-find.el ---  -*- lexical-binding: t -*-
(defun portable-find--recursively (current-path name-pattern root)
  "Recursively search and print files from CURRENT-PATH matching NAME-PATTERN.
This is a private helper function for `eshell/portable-find`."
  (dolist (file (condition-case nil (directory-files current-path t)
                  (error '())))
    (let ((full-path (expand-file-name file current-path))
          (file-name (file-name-nondirectory file)))
      (cond
        ;; 1. 如果是目录，则递归调用 (排除 . 和 ..)
        ((and (file-directory-p full-path)
              (not (member file-name '("." ".."))))
         (portable-find--recursively full-path name-pattern root))

        ;; 2. 如果是文件且符合 -name 模式，则打印
        ((file-regular-p full-path)
         (when (or (null name-pattern)
                   (string-match-p name-pattern file-name))
           ;; 使用 message 模拟标准输出到 Eshell
           (eshell-buffered-print
            (format "%s\n" (file-relative-name full-path root)))))))))


(defun eshell/portable-find (dir &rest args)
  "A basic 'find' analogue for Eshell, written in Elisp.
Usage: portable-find [directory] [-name pattern]

[directory] : The directory to start searching from. Defaults to '.' (current directory).
-name pattern : Only display files whose name matches the glob PATTERN (e.g., '*.el').

Example:
$ portable-find . -name '*.el'
$ portable-find ~/docs -name 'report*.pdf'"
  (interactive "sDirectory to search: ")

  (let* (;; 解析参数
         (search-dir (if (file-directory-p dir) dir "."))
         (name-pattern nil)

         ;; 提取 -name 模式
         ;; 注意：Elisp 的 `nth` 和 `member` 足够处理，无需 `cl-position`
         (name-pos (cl-position "-name" args :test 'string-equal)))

    ;; 如果找到 -name 选项，则提取其后的模式
    (when name-pos
      (when (> (length args) (1+ name-pos))
        (setq name-pattern (nth (1+ name-pos) args))))

    ;; 确保搜索目录是绝对路径
    (setq search-dir (expand-file-name search-dir))

    ;; 打印起始信息
    (eshell-buffered-print
     (format "eshell portable-find in: %s for: %s\n" search-dir name-pattern))
    (eshell-flush)

    ;; 调用递归函数开始查找
    (portable-find--recursively search-dir name-pattern search-dir)
    (eshell-flush)))

;;; End of portable-find.el
