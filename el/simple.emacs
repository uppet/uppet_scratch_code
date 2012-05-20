
;customize path 路径设定
(setenv "HOME" "e:/emacswd")
;(setenv "HOME" "C:/Users/ivy/AppData/Roaming/")
;shortcuts
;(fancy-startup-screen)
;set path=g:\green\mingw\bin;E:\Soft\7-Zip;%PATH%
;configure --without-tiff  --with-gcc --without-gnutls --cflags "-I ../../libs/libXpm-3.5.10/include/ -I ../../libs/libXpm-3.5.10/src -D FOR_MSW  -I ../../libs/libpng-1.2.49 -I ../../libs/zlib-1.2.6 -I ../../libs/jpeg-6b-4-lib/include -I ../../libs/giflib-4.1.5/lib" 
;configure --without-gif --without-jpeg --without-tiff --without-png --without-xpm --with-msvc --without-gnutls
;(print (frame-parameters nil))


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

;;;must mode
(ido-mode 1)
(transient-mark-mode 0)
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(global-hl-line-mode 1)
(setq truncate-lines t)   ;不要卷行显示
(blink-cursor-mode 0) ;不闪的才是健康的
(setq default-tab-width 4)      ;看得更多一些
(setq ring-bell-function 'ignore)

;;;must UI
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(set-default-font "Bitstream Vera Sans Mono 10")


;;;;;;;;;;;;;;;;;;;;;;;;must jr code
(defun jr-flip-file-name (fn)
  (cond ((string-match ".*\\.cpp" fn)
         (replace-regexp-in-string "\\.cpp" ".h" fn))
        ((string-match ".*\\.c" fn)
         (replace-regexp-in-string "\\.c" ".h" fn))
        ((string-match ".*\\.h" fn)
         (if (file-exists-p (replace-regexp-in-string "\\.h" ".cpp" fn))
             (replace-regexp-in-string "\\.h" ".cpp" fn)
           (replace-regexp-in-string "\\.h" ".c" fn)))
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

(require 'server)
(when (not (server-running-p))
  (server-start))

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
 '(default ((t (:background "#314f4f")))))


(when (not window-system)
  (custom-set-faces
 '(default ((t (:background "black"))))))

