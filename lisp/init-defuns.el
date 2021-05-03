;;; init-defuns.el --- Some useful defuns

;;; Commentary:
;;

(defun my-insert-current-time ()
  (interactive)
  (let ((time-str))
    (insert (format-time-string "%R "))))

(defun my-paste-to-new-buffer ()
  (interactive)
  (let ((content (current-kill 0))
        (buffer (generate-new-buffer-name "*clipboard contents*")))
    (switch-to-buffer-other-window buffer)
    (insert content)
    (set-text-properties (point-min) (point-max) nil)))

(defun my-org-scratch-buffer ()
  (interactive)
  (let ((buffer (generate-new-buffer-name "*org-scratch*")))
    (if current-prefix-arg
        (switch-to-buffer buffer)
      (switch-to-buffer-other-window buffer))
    (org-mode)))

(provide 'init-defuns)

;;; init-defuns.el ends here
