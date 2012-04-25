(defun g9 ()
  (interactive)
  (delete-other-windows)
  (let ((w1 (selected-window))
        w2
        w3
        (3rd (lambda (x) (round (* x 0.333)))))
    (split-window-right (funcall 3rd (window-width)))
    (select-window (window-next-sibling))
    (split-window-right)
    (setq w2 (selected-window))
    (select-window (window-next-sibling))
    (setq w3 (selected-window))
    (dolist (w (list w1 w2 w3))
      (select-window w)
      (split-window (selected-window) (funcall 3rd (window-height)))
      (split-window (window-next-sibling)))))