;;; init-ui.el --- emacs ui settings

;;; Commentary:
;;

(use-package simple
  :config
  (column-number-mode 1)
  (line-number-mode 1))

(use-package frame
  :config (blink-cursor-mode -1))

(use-package minions
  :straight t
  :config (minions-mode 1))

(use-package hl-todo
  :straight t
  :init (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package highlight-numbers
  :straight t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  (add-hook 'yaml-mode-hook 'highlight-numbers-mode))

(use-package hl-line
  :bind (:map my-toggle-prefix-map ("h" . hl-line-mode)))

(use-package display-line-numbers
  :if (version<= "26.1" emacs-version)
  :bind (:map my-toggle-prefix-map ("l" . my-line-numbers-toggle))
  :config (setq display-line-numbers-type 'visual)
  :init
  (setq my-line-numbers-toggle-types '(visual t))

  (defun my--line-numbers-toggle ()
    (setq my-line-numbers-toggle-types (-rotate 1 my-line-numbers-toggle-types))
    (let ((display-line-numbers-type (car my-line-numbers-toggle-types)))
      (display-line-numbers-mode)))

  (defun my-line-numbers-toggle ()
    (interactive)
    (if (bound-and-true-p display-line-numbers-mode)
        (display-line-numbers-mode -1)
      (progn
        (my--line-numbers-toggle)
        (set-transient-map
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "l")
             (lambda () (interactive)
               (my--line-numbers-toggle)))
           map)
         t)))))

(use-package olivetti
  :straight t
  :config (setq-default olivetti-body-width 0.7)
  :bind (:map my-toggle-prefix-map
              ("o" . olivetti-mode)))


(use-package doom-modeline
  :straight t
  :config (setq doom-modeline-minor-modes t))

(use-package all-the-icons
  :straight t
  :config (setq all-the-icons-scale-factor 1.1)
  :init
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

(when (and (display-graphic-p) (member "Iosevka" (font-family-list)))
  (set-frame-font "Iosevka-14")
  (add-to-list 'default-frame-alist '(font . "Iosevka-14")))

(provide 'init-ui)

;;; init-ui.el ends here
