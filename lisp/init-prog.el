;;; init-prog.el --- Programming modes and utils

;;; Commentary:
;;

;;; Code:

(use-package erlang
  :straight t
  :disabled t
  :config
  (when (and system-type-darwin (executable-find "erl") (executable-find "brew"))
    (setq erlang-root-dir (replace-regexp-in-string "\n$" "" (shell-command-to-string "brew --prefix erlang")))))

(use-package dumb-jump
  :straight t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(progn
  (use-package yard-mode
    :straight t
    :config
    (add-hook 'ruby-mode-hook 'yard-mode))

  (use-package rvm
    :straight t
    :config
    (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby))

  (straight-use-package 'inf-ruby))

(use-package lispy
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode))

(use-package lsp-mode
  :straight t
  ;; :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))


(use-package lsp-ui
  :straight t)

;; (lsp-ui-mode +1)

;; (use-package company-lsp
;;   :straight t
;;   :commands company-lsp)

(setq lsp-enable-links nil
      lsp-flycheck-live-reporting nil
      lsp-signature-auto-activate t
      lsp-signature-doc-lines 1
      lsp-signature-render-documentation t
      lsp-ui-peek-enable nil)


(use-package flycheck
  :straight t
  :bind (:map my-toggle-prefix-map
              ("f" . flycheck-mode))
  :init (setq flycheck-checker-error-threshold 500
              flycheck-check-syntax-automatically '(mode-enabled save)))

(straight-use-package 'dockerfile-mode)

(straight-use-package 'yaml-mode)
(straight-use-package 'php-mode)
(straight-use-package 'know-your-http-well)
(straight-use-package 'fish-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'nim-mode)





(progn
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

  ;; go get golang.org/x/tools/gopls@latest

  ;; (setq lsp-gopls-staticcheck t)
  ;; (setq lsp-eldoc-render-all t)
  ;; (setq lsp-gopls-complete-unimported t)

  (use-package go-mode
    :straight t
    ;; :bind (:map go-mode-map
    ;;             ("C-c C-p" . godoc-at-point)
    ;;             ("C-c C-e" . go-gopath-set-gopath)
    ;;             ("C-c C-r" . go-remove-unused-imports))
    :config
    ;; (setq gofmt-command "goimports")
    ;; (add-hook 'before-save-hook #'gofmt-before-save)
    ;; (add-hook 'go-mode-hook (lambda ()
    ;;                           (set (make-local-variable 'company-backends) '(company-go))
    ;;                           (company-mode)))
    ;; (add-hook 'go-mode-hook 'go-eldoc-setup)
    )

  ;; (straight-use-package 'go-eldoc)
  ;; (straight-use-package 'company-go)
  ;; (straight-use-package 'go-rename)

  ;; (straight-use-package 'go-playground)

  (straight-use-package 'gotest))

(use-package company
  :straight t
  :bind ("M-2" . company-complete)
  :config
  (setq company-tooltip-align-annotations t
        company-show-numbers t)
  (global-company-mode))

(straight-use-package 'lice)

(provide 'init-prog)

;;; init-prog.el ends here
