;;; init-vc.el --- Version control packages

;;; Commentary:
;;

;;; Code:

(require 'project)

(use-package magit
  :straight t
  :demand t
  :bind ("C-c v" . magit-status)
  :config
  (setq magit-save-repository-buffers 'dontask))


;; (eval-after-load 'magit
;;   (require 'magit-extras))

(use-package git-link
  :straight t
  :config (setq git-link-open-in-browser t))

(provide 'init-vc)

;;; init-vc.el ends here
