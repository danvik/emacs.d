;;; init-completing.el --- completion and selection packages

;;; Commentary:
;;

(use-package embark
  :straight t
  :bind ("C-." . embark-act))

(use-package marginalia
  :straight (:host github :repo "minad/marginalia" :branch "main")
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode t))

(use-package consult
  :straight (:host github :repo "minad/consult" :branch "main"))

(straight-use-package
 '(vertico :files (:defaults "extensions/*")
           :includes (vertico-buffer
                      vertico-directory
                      vertico-flat
                      vertico-grid
                      vertico-indexed
                      vertico-mouse
                      vertico-multiform
                      vertico-quick
                      vertico-repeat
                      vertico-reverse
                      vertico-unobtrusive)))

;; checkout `vertico-multiform-categories'
;; (setq vertico-multiform-commands
;;       '((consult-imenu buffer)
;;         (consult-imenu-multi buffer)
;;         (consult-grep buffer)
;;         (switch-to-buffer unobtrusive)
;;         (find-file indexed)
;;         ;; (project-find-file buffer)
;;         ))
;; https://github.com/minad/vertico/issues/168
(vertico-mode t)
;; (vertico-multiform-mode t)


;; (straight-use-package 'vertico-multiform)

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :straight t
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.5
        corfu-count 10)
  (setq tab-always-indent 'complete)
  :init
  (corfu-global-mode))

(use-package cape
  :straight t)


(use-package savehist
  :init
  (savehist-mode))

(provide 'init-completing)


;;; init-completing.el ends here
