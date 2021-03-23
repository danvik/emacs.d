;;; init-editing.el --- text editing settings

;;; Commentary:
;;

;;; Code:

(use-package easy-kill
  :straight t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark)
  (setq easy-kill-unhighlight-key (kbd "RET"))
  (bind-keys :map easy-kill-base-map
             ("DEL" . (lambda ()
                        (interactive)
                        (easy-kill-mark-region)
                        (call-interactively #'delete-region)))
             (";" . (lambda ()
                      (interactive)
                      (easy-kill-mark-region)
                      (call-interactively #'comment-dwim-2)))))

(use-package editorconfig
  :straight t
  :config (editorconfig-mode +1))


(setq delete-selection-mode t
      sentence-end-double-space nil
      save-interprogram-paste-before-kill t)

(set-language-environment 'utf-8)
;; (prefer-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 78)

(use-package ispell
  :config (when (executable-find "aspell")
            (setq-default ispell-program-name "aspell")))

(use-package smartparens
  :straight t
  :bind (:map my-toggle-prefix-map
              ("s" . smartparens-global-mode))
  :init
  (require 'smartparens-config)
  (smartparens-global-mode t))


(use-package browse-kill-ring
  :straight t
  :bind
  ("M-y" . browse-kill-ring))

(use-package browse-kill-ring
  :straight t
  :bind
  ("M-y" . browse-kill-ring))(use-package comment-dwim-2
  :straight t
  :bind
  ("M-;" . comment-dwim-2))

(use-package expand-region
  :straight t
  :bind
  ("C-=" . er/expand-region))

(use-package iedit
  :straight t
  :bind ("C-;" . iedit-mode))

(use-package goto-last-change
  :straight t
  :bind ("M-g i" . goto-last-change))

(use-package undo-tree
  :straight t
  :config (global-undo-tree-mode t))

(provide 'init-editing)

;;; init-editing.el ends here
