;;; init-completing.el --- completion and selection packages

;;; Commentary:
;;

(use-package embark
  :straight t
  :bind ("C-c e" . embark-act))

(use-package marginalia
  :straight (:host github :repo "minad/marginalia" :branch "main")
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode t))

(use-package consult
  :straight (:host github :repo "minad/consult" :branch "main"))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(provide 'init-completing)


;;; init-completing.el ends here
