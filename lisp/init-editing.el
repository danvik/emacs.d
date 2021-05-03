;;; init-editing.el --- text editing settings

;;; Commentary:
;;

;;; Code:

(show-paren-mode 1)

(set-language-environment 'utf-8)

(setq delete-selection-mode t
      sentence-end-double-space nil
      save-interprogram-paste-before-kill t)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 78)

(use-package easy-kill
  :straight t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark)
  (setq easy-kill-unhighlight-key (kbd "RET")))

(use-package editorconfig
  :straight t
  :config (editorconfig-mode +1))

(use-package ispell
  :config
  (when (executable-find "aspell")
    (setq-default ispell-program-name "aspell")))

(use-package smartparens
  :straight t
  :init
  (require 'smartparens-config)
  (smartparens-global-mode t))

(use-package browse-kill-ring
  :straight t
  :bind ("M-y" . browse-kill-ring))

(use-package comment-dwim-2
  :straight t
  :bind ("M-;" . comment-dwim-2))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package iedit
  :straight t
  :bind ("C-;" . iedit-mode))

(use-package goto-last-change
  :straight t
  :bind ("M-g i" . goto-last-change))

(use-package undo-tree
  :straight t
  :config (global-undo-tree-mode t))

(use-package multiple-cursors
  :straight t
  :bind
  (("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)
   ("C-S-c C-S-c" . mc/edit-lines)))

(use-package hippie-exp
  :bind ("C-\\" . hippie-expand))

(use-package avy
  :straight t
  :bind (("M-g w" . avy-goto-word-1)
         ("M-g l" . avy-goto-line)
         ("M-g f" . avy-goto-char-in-line)
         ("M-g M-l" . avy-copy-line)
         ("M-g g" . avy-goto-line))
  :config (setq avy-all-windows nil))

(provide 'init-editing)

;;; init-editing.el ends here
