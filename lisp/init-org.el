;;; init-org.el --- Org mode settings and packages

;;; Commentary:
;;

;;; Code:

(use-package org
  :straight t
  :config
  (setq org-hide-emphasis-markers t
        org-log-done 'time
        org-src-fontify-natively t)
  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t)
                               (emacs-lisp . t))))

(use-package ox-md)

(use-package orglink
  :straight t
  :config (global-orglink-mode))

(use-package worf
  :straight t
  :config (add-hook 'org-mode-hook #'worf-mode))
(straight-use-package 'htmlize)
(straight-use-package 'ox-twbs)
;; (require 'ox)


(straight-use-package 'ox-reveal)

(provide 'init-org)

;;; init-org.el ends here
