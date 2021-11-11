;;; init-utils.el --- utility packages

;;; Commentary:
;;

(use-package deft
  :straight t
  :init
  (setq deft-extensions '("org")
        deft-text-mode 'org-mode
        deft-directory "~/.deft/"
        deft-auto-save-interval 5.0)
  (unless (file-exists-p deft-directory)
    (when (yes-or-no-p (format "Directory %s does not exists, create?" deft-directory))
      (mkdir deft-directory))))

(straight-use-package 'elfeed)

(use-package god-mode
  :straight t
  :bind (("<escape>" . god-local-mode)
         :map my-toggle-prefix-map
         ("g" . god-local-mode)
         :map god-local-mode-map
         ("i" . god-local-mode)
         ("[" . backward-paragraph)
         ("]" . forward-paragraph)

         ("." . repeat))
  :config
  (add-hook 'god-mode-enabled-hook (lambda () (hl-line-mode 1)))
  (add-hook 'god-mode-disabled-hook (lambda () (hl-line-mode -1))))

(use-package which-key
  :straight t
  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom)
  (which-key-mode))

(use-package crux
  :straight t
  :bind (("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c D" . crux-duplicate-and-comment-current-line-or-region)))


(provide 'init-utils)

;;; init-utils.el ends here
