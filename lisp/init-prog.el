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

(use-package lispy
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode))

(use-package lsp-mode
  :straight t
  :config
  (setq lsp-enable-links nil
        lsp-signature-auto-activate t
        lsp-signature-doc-lines 1
        lsp-signature-render-documentation t
        lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :straight t
  :config
  (setq lsp-ui-peek-enable nil))


(use-package flycheck
  :straight t
  :bind (:map my-toggle-prefix-map
              ("f" . flycheck-mode))
  :init
  (setq flycheck-checker-error-threshold 500
        flycheck-check-syntax-automatically '(mode-enabled save)))


(straight-use-package 'dockerfile-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'php-mode)
(straight-use-package 'know-your-http-well)
(straight-use-package 'fish-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'nim-mode)

(use-package go-mode
  :straight t
  :requires lsp-mode
  :hook (go-mode . lsp-deferred)
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(straight-use-package 'gotest)

(straight-use-package 'lice)
(straight-use-package 'impatient-mode)

(provide 'init-prog)

;;; init-prog.el ends here
