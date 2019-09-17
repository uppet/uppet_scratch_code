

;;; packages
(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")))))

;;; optional haskell


;;;must key
(global-set-key (kbd "M-h") (lambda () (interactive) (find-file "~/.emacs")))
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-3") 'server-edit)
(global-set-key (kbd "C-'") 'server-edit)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "M-o") 'jr-find-alt-buf-or-file)

;;must font
(setq vera-font (create-fontset-from-ascii-font "Bitstream Vera Sans Mono 12"))
(set-fontset-font vera-font 'unicode (font-spec :name "Hiragino Sans GB W3") nil 'append)
(set-default-font vera-font)

;;;must mode
(ido-mode)
(transient-mark-mode 0)
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq ring-bell-function 'ignore)

;;;must UI
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;must jr code
(defun jr-flip-file-name (fn)
  (cond ((string-match ".*\\.cpp" fn)
         (replace-regexp-in-string "\\.cpp" ".h" fn))
        ((string-match ".*\\.h" fn)
         (replace-regexp-in-string "\\.h" ".cpp" fn))
        (t "")))
(defun jr-find-alt-buf-or-file ()
  "switch between .h .cpp"
  (interactive)
  (let* ((fn (buffer-name))
         (ffn (buffer-file-name))
         (fn-new (jr-flip-file-name fn))
         (ffn-new (jr-flip-file-name ffn)))
    (cond ((memq (get-buffer fn-new)
                 (buffer-list))
           (switch-to-buffer (get-buffer fn-new)))
          ((and (> (length ffn-new) 0) (file-exists-p ffn-new))
           (find-file ffn-new)))))
;;;;;;;;;;;;;;;;;;;;;;;;jr code end

;;;must hook
(defun my-c-mode-hook ()
  (c-set-style "stroustrup"))
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(setq jr-customize-bg-color "black")
(when window-system
  (setq jr-customize-bg-color "#314f4f"))
;(server-start)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default-tab-width 4 t)
 '(frame-title-format (quote ("%b || Emacs ||" (:eval (current-time-string)))) t)
 '(inhibit-startup-screen t)
 '(truncate-lines t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:background "#314f4f" :height 110)))))


(when (not window-system)
  (custom-set-faces
 '(default ((t (:background "black"))))))
