;;; init-dired.el --- Dired extensions

;;; Commentary:
;;

;;; Code:

(use-package dired
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package dired-x
  :bind ("C-c j" . dired-jump-other-window)
  :init
  (setq dired-bind-jump nil))

(use-package dired-subtree
  :straight t
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package dired-filter
  :straight t
  :after dired)

(use-package dired-narrow
  :straight t
  :after dired
  :bind (:map dired-mode-map ("\\" . dired-narrow)))

(provide 'init-dired)

;;; init-dired.el ends here
