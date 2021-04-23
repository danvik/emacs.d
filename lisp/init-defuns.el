;;; init-defuns.el --- Some useful defuns

;;; Commentary:
;;

(defun my-insert-current-time ()
  (interactive)
  (let ((time-str (format-time-string "%R ")))
    (if current-prefix-arg
        (message time-str)
      (insert time-str))))

(defun my-paste-to-new-buffer ()
  (interactive)
  (let ((content (current-kill 0))
        (buffer (generate-new-buffer-name "*clipboard contents*")))
    (switch-to-buffer-other-window buffer)
    (insert content)
    (set-text-properties (point-min) (point-max) nil)))

(defun my-org-scratch-buffer ()
  (interactive)
  (let ((default-directory (expand-file-name "~" )))
    (let ((buffer (generate-new-buffer-name "*org-scratch*")))
      (if current-prefix-arg
          (switch-to-buffer buffer)
        (switch-to-buffer-other-window buffer))
      (org-mode)
      (when (bound-and-true-p evil-mode)
        (evil-insert-state)))))

(defun my--statistic-count-p ()
  (string-match "\\[[0-9]+/[0-9]+\\]$" (thing-at-point 'line t)))

(defun my--statistic-percent-p ()
  (string-match "\\[[[:digit:]]\\{0,3\\}%\\]$" (thing-at-point 'line t)))


(defun my-toggle-statistic-cookie-type ()
  (interactive)
  (save-excursion
    (org-previous-visible-heading 1)
    (cond ((my--statistic-count-p) (progn
                                    (end-of-line)
                                    (kill-sexp -1)
                                    (insert "[%]")
                                    (org-update-statistics-cookies nil)))
          ((my--statistic-percent-p) (progn
                                      (end-of-line)
                                      (kill-sexp -1)))
          (t (progn
               (end-of-line)
               (just-one-space)
               (insert "[/]")
               (org-update-statistics-cookies nil))))))


(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1))

(bind-keys
 ("C-M-=" . align-to-equals)
 :map my-custom-key-map
 ("t" . my-insert-current-time)
 ("c" . my-paste-to-new-buffer)
 ("o" . my-org-scratch-buffer)
 ("s" . my-toggle-statistic-cookie-type))



(provide 'init-defuns)

;;; init-defuns.el ends here
