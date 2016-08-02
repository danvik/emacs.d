;;; defuns

(defun my--projectile-active ()
  (condition-case nil
      (projectile-project-root)
    (error nil)))

(defun my-find-file-or-projectile-find-file ()
  (interactive)
  (if (my--projectile-active)
      (call-interactively
       'projectile-find-file)
    (call-interactively 'find-file)))

(defun my-org-link-at-point ()
  "Generate org link with line number to current buffer position"
  (interactive)
  (save-excursion
    (let ((filename (buffer-file-name)))
      (when filename
        (kill-new (format "[[%s::%s][%s]]"
                          filename
                          (line-number-at-pos)
                          (concat (file-name-base filename) "." (file-name-extension filename))))))))

(defun my-init-terminal (&optional frame)
  (interactive)
  (unless (window-system frame)
    (progn
      (menu-bar-mode -1)
      (require 'mouse)
      (xterm-mouse-mode t)
      (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
      (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 1)))
      (defun track-mouse (e))
      (setq mouse-sel-mode t))))

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

(defun my-count-words-in-org-subtree ()
  (interactive)
  (let ((count nil))
    (save-excursion
      (outline-previous-heading)
      (save-restriction
        (org-narrow-to-subtree)
        (setq count (format "%s" (count-words (point-min) (point-max))))))
    (if current-prefix-arg
        (insert count)
      (message count))))

(defun my--statistic-count-p ()
  (string-match "\\[[0-9]+/[0-9]+\\]$" (thing-at-point 'line t)))

(defun my--statistic-percent-p ()
  (string-match "\\[[[:digit:]]\\{1,3\\}%\\]$" (thing-at-point 'line t)))

(defun my-toggle-statistic-cookie-type ()
  (interactive)
  (save-excursion
    (outline-up-heading 1)
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


;; http://oremacs.com/2015/01/26/occur-dwim/
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push
   (if (region-active-p)
       (buffer-substring-no-properties
        (region-beginning)
        (region-end))
     (let ((sym (thing-at-point 'symbol)))
       (when (stringp sym)
         (regexp-quote sym))))
   regexp-history)
  (call-interactively 'occur))

;; http://stackoverflow.com/a/3633840
(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1))

;; http://rejeep.github.io/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun my-find-all-go-files ()
  (interactive)
  (my-find-all-files-from-root (getenv "GOPATH") "*.go"))

(defun my-find-all-projectile-files ()
  (interactive)
  (my-find-all-files-from-root (projectile-project-root) "*.*"))

(defun my-find-all-files-from-root (root pattern)
  (interactive)
  (ivy-read
   "File: "
   (process-lines "find" root "-type" "file" "-name" pattern)
   :action #'find-file-other-window))

(provide 'defuns)
