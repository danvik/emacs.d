;;; init-utils.el --- utility packages

;;; Commentary:
;;

(use-package deft
  :straight t
  :bind (:map my-custom-key-map ("d" . deft))
  :init
  (setq deft-extensions '("org")
        deft-text-mode 'org-mode
        deft-directory "~/.deft/"
        deft-auto-save-interval 5.0)
  (unless (file-exists-p deft-directory)
    (when (yes-or-no-p (format "Directory %s does not exists, create?" deft-directory))
      (mkdir deft-directory))))

(straight-use-package 'elfeed)

(use-package exec-path-from-shell
  :straight t
  :if system-type-darwin
  :config (exec-path-from-shell-initialize))

(use-package god-mode
  :straight t
  :bind (("<escape>" . god-local-mode)
         :map my-toggle-prefix-map
         ("g" . god-local-mode)
         :map god-local-mode-map
         ("i" . god-local-mode)
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
  :straight t)


(provide 'init-utils)

;;; init-utils.el ends here
