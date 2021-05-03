;;; init-vc.el --- Version control packages

;;; Commentary:
;;

;;; Code:

(use-package magit
  :straight t
  :bind ("C-c v" . magit-status)
  :config
  (setq magit-save-repository-buffers 'dontask))

(use-package git-link
  :straight t
  :config (setq git-link-open-in-browser t))

(provide 'init-vc)

;;; init-vc.el ends here
