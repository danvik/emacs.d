;;; init.el --- Emacs configuration

;;; Commentary:
;;

;;; Code:

;;; bootstrap `straight-use-package'

(progn
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; https://github.com/raxod502/straight.el/issues/700
  (setq straight-recipes-emacsmirror-use-mirror nil)

  (straight-use-package 'use-package))

;;; local settings and variables

(defconst system-type-darwin (eq system-type 'darwin)
  "Mac or not.")

(defconst at-work (file-directory-p (concat (expand-file-name "~") "/work"))
  "What computer is this.")

(defvar local-settings-file "~/.priv/elisp/local.el"
  "File used for private and local settings.")

(defvar org-bookmarks-file nil
  "File used for org capture of book")

(when (file-exists-p local-settings-file)
  (load local-settings-file ))

;;; custom

(straight-use-package 'no-littering)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file t)

;;; key settings

(define-prefix-command 'my-toggle-prefix-map)
(global-set-key (kbd "C-c t") my-toggle-prefix-map)

(when system-type-darwin
  (setq mac-option-modifier 'none
        mac-command-modifier 'meta))

;;; visuals

(column-number-mode 1)
(line-number-mode 1)
(blink-cursor-mode -1)

(when (and (display-graphic-p) (member "Iosevka" (font-family-list)))
  (set-frame-font "Iosevka-16")
  (add-to-list 'default-frame-alist '(font . "Iosevka-16")))

;;; general emacs settings

(setq gc-cons-threshold 100000000
      ring-bell-function 'ignore
      ffap-machine-p-known 'reject
      confirm-kill-emacs #'yes-or-no-p)

(setq recenter-positions '(top middle bottom))

(defalias 'yes-or-no-p 'y-or-n-p)

;;; emacs security

(setq enable-local-variables t
      network-security-level 'paranoid
      ffap-machine-p-known 'reject
      epa-pinentry-mode 'loopback)

;;; editing

(set-language-environment 'utf-8)

(setq delete-selection-mode t
      sentence-end-double-space nil
      save-interprogram-paste-before-kill t)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 78)

;;; visual aid

(use-package page-break-lines
  :straight t
  :hook (emacs-lisp-mode . page-break-lines-mode))

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode))

(use-package highlight-numbers
  :straight t
  :hook ((prog-mode yaml-mode) . highlight-numbers-mode))

(use-package hl-line
  :bind (:map my-toggle-prefix-map ("h" . hl-line-mode)))

(use-package display-line-numbers
  :bind (:map my-toggle-prefix-map ("l" . display-line-numbers-mode)))

(use-package olivetti
  :straight t
  :config (setq-default olivetti-body-width 0.5)
  :bind (:map my-toggle-prefix-map
              ("o" . olivetti-mode)))

(use-package all-the-icons
  :straight t
  :config (setq all-the-icons-scale-factor 1.1)
  :init
  (when (and (display-graphic-p) (not (member "all-the-icons" (font-family-list))))
    (all-the-icons-install-fonts nil)))

(use-package doom-modeline
  :requires all-the-icons
  :straight t
  :config (setq doom-modeline-buffer-encoding 'nondefault)
  :init (doom-modeline-mode))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))


;;; buffers / windows

(straight-use-package 'rotate)
(straight-use-package 'bufler)

(winner-mode t)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

;;; dired

(use-package dired
  :hook (dired-mode . dired-hide-details-mode))

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

;;; editing

(show-paren-mode 1)

(use-package easy-kill
  :straight t
  :bind (:map easy-kill-base-map
              ("DEL" . (lambda ()
                         (interactive)
                         (easy-kill-mark-region)
                         (call-interactively #'delete-region)))
              (";" . (lambda ()
                       (interactive)
                       (easy-kill-mark-region)
                       (call-interactively #'comment-dwim-2)))
              ("j" . easy-kill-expand)
              ("k" . easy-kill-shrink))
  :config
  (setq easy-kill-unhighlight-key (kbd "RET"))

  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(use-package editorconfig
  :straight t
  :config (editorconfig-mode +1))

(use-package drag-stuff
  :straight t
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package ispell
  :config
  (when (executable-find "aspell")
    (setq-default ispell-program-name "aspell")))

(use-package smartparens
  :straight t
  :init
  (require 'smartparens-config)
  (smartparens-global-mode t))

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
  :bind ("M-g ." . goto-last-change))

(use-package multiple-cursors
  :straight t
  :bind
  (("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)
   ("C-S-c C-S-c" . mc/edit-lines)))

(use-package avy
  :straight t
  :bind (("M-g w" . avy-goto-word-1)
         ("M-g l" . avy-goto-line)
         ("M-g f" . avy-goto-char-in-line)
         ("M-g M-l" . avy-copy-line)
         ("M-g g" . avy-goto-line))
  :config (setq avy-all-windows nil))

;;; files

(use-package recentf
  :config
  (setq recentf-exclude '("deft/" ".gpg")
        recentf-max-saved-items 500)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  :init
  (recentf-mode t))

(use-package super-save
  :straight t
  :config
  (setq super-save-auto-save-when-idle nil
        super-save-exclude '(".gpg")
        auto-save-list-file-prefix nil
        auto-save-default nil
        make-backup-files nil)
  (add-to-list 'super-save-triggers 'other-window)
  :init
  (super-save-mode +1))

;;; lsp

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

;;; flycheck

(use-package flycheck
  :straight t
  :bind (:map my-toggle-prefix-map
              ("f" . flycheck-mode))
  :init
  (setq flycheck-checker-error-threshold 500
        flycheck-check-syntax-automatically '(mode-enabled save)))

;;; projects / vc

(straight-use-package 'git-link)
(straight-use-package 'project)

(use-package projectile
  :straight t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-use-git-grep t
        projectile-completion-system 'default
        projectile-switch-project-action #'projectile-commander)
  (projectile-mode))

(use-package magit
  :straight t
  :bind ("C-c v" . magit-status)
  :config
  (setq magit-save-repository-buffers 'dontask))

;;; completion

(use-package hippie-exp
  :bind ("C-\\" . hippie-expand))

(use-package consult
  :straight t
  :bind (("C-x b" . consult-buffer)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g o" . consult-outline)
         ("C-c r" . consult-register)
         ("C-c R" . consult-register-store)
         ("M-s l" . consult-line)
         ("M-s M-l" . consult-line-multi)
         ("M-g SPC" . consult-mark)
         :map org-mode-map
         ("M-g o" . consult-org-heading))
  :config
  (defvar my-consult--source-edited-buffers
    `(:name "Edited"
            :narrow ?e
            :category buffer
            :face consult-buffer
            :history buffer-name-history
            :state ,#'consult--buffer-state
            :items
            ,(lambda ()
               (consult--buffer-query :sort 'visibility
                                      :as #'buffer-name
                                      :predicate (lambda (buffer)
                                                   (with-current-buffer buffer
                                                     (and buffer-undo-list (buffer-file-name)))))))
    "Edited buffers candidate source for `consult-buffer'.")

  (add-to-list 'consult-buffer-sources 'my-consult--source-edited-buffers)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))


(use-package embark-consult
  :after (embark consult))

(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


(use-package embark
  :straight t
  :bind ("C-." . embark-act))

(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode t))

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :config (setq vertico-resize 'grow-only)
  :init
  (vertico-mode))

(progn
  (vertico-multiform-mode t)
  ;; (setq vertico-multiform-categories
  ;;       '((file buffer)
  ;;         (imenu buffer)
  ;;         (grep buffer)))

  (setq vertico-multiform-commands
        '((consult-imenu buffer)
          (consult-grep buffer)
          (consult-outline buffer)
          (consult-git-grep buffer)
          (execute-extended-command unobtrusive))))

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

;;; org

(use-package org
  :straight t
  :config
  (setq org-hide-emphasis-markers t
        org-log-done 'time
        org-src-fontify-natively t)
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages '((shell . t)
                                 (emacs-lisp . t)))))

(use-package ox-md)

;; TODO: check `orglink-activate-in-modes'

(use-package orglink
  :straight t
  :config (global-orglink-mode))

(use-package worf
  :straight t
  :hook (org-mode . worf-mode))

;; brew install pandoc
;; brew cask install basictex
(dolist (export-package '(htmlize ox-twbs ox-reveal ox-pandoc))
  (straight-use-package export-package))

(unless at-work
  (use-package org-web-tools
    :straight t)

  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda)

  (setq org-capture-templates
        '(
          ("t" "Task"
           entry (file+headline "" "Tasks")
           "* TODO %?\n  %u\n  %a")

          ("b" "Bookmark (Clipboard)" entry (file+headline  "new")
           "** %(org-web-tools--org-link-for-url)\n:PROPERTIES:\n:TIMESTAMP: %t\n:END:%?\n" :empty-lines 1 :prepend t))))

;;; search and grep

(use-package ctrlf
  :straight t
  :init
  (ctrlf-mode +1))

(use-package deadgrep
  :straight t
  :bind ("C-c g" . deadgrep))

;;; util apps

(use-package deft
  :straight t
  :init
  (setq deft-extensions '("org")
        deft-text-mode 'org-mode
        deft-directory "~/.deft/"
        deft-auto-save-interval 5.0)
  (unless (file-exists-p deft-directory)
    (when (yes-or-no-p (format "Directory %s does not exists, create?" deft-directory))
      (mkdir deft-directory))))

(straight-use-package 'elfeed)

;;; god

(use-package god-mode
  :straight t
  :bind (("<escape>" . god-local-mode)
         :map my-toggle-prefix-map
         ("g" . god-local-mode)
         :map god-local-mode-map
         ("i" . god-local-mode)
         ("[" . backward-paragraph)
         ("]" . forward-paragraph)

         ("." . repeat))
  :config
  (add-hook 'god-mode-enabled-hook (lambda () (hl-line-mode 1)))
  (add-hook 'god-mode-disabled-hook (lambda () (hl-line-mode -1))))

;;; help

(use-package which-key
  :straight t
  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom)
  (which-key-mode))

;;; crux

(use-package crux
  :straight t
  :bind (("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c D" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c <tab>" . crux-indent-defun)
         ("C-c j" . crux-top-join-line)))


;;; defuns

(defun my-insert-current-time ()
  (interactive)
  (let ((time-str))
    (insert (format-time-string "%R "))))

(defun my-paste-to-new-buffer ()
  (interactive)
  (let ((content (current-kill 0))
        (buffer (generate-new-buffer-name "*clipboard contents*")))
    (switch-to-buffer-other-window buffer)
    (insert content)
    (set-text-properties (point-min) (point-max) nil))
  (text-mode))

(defun my-org-scratch-buffer ()
  (interactive)
  (let ((buffer (generate-new-buffer-name "*org-scratch*")))
    (if current-prefix-arg
        (switch-to-buffer buffer)
      (switch-to-buffer-other-window buffer))
    (org-mode)))

;;; programming


(add-hook 'ruby-mode-hook 'superword-mode)

(dolist (package
         '(dockerfile-mode
           fish-mode
           impatient-mode
           know-your-http-well
           lice
           markdown-mode
           nim-mode
           php-mode
           yaml-mode))
  (straight-use-package package))

(use-package dumb-jump
  :straight t
  :hook (xref-backend-functions . dumb-jump-xref-activate))

(use-package lispy
  :straight t
  :hook (emacs-lisp-mode . lispy-mode))

(use-package go-mode
  :straight t
  :hook (go-mode . lsp-deferred)
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(use-package erlang
  :straight t
  :disabled t
  :config
  (when (and system-type-darwin (executable-find "erl") (executable-find "brew"))
    (setq erlang-root-dir (replace-regexp-in-string "\n$" "" (shell-command-to-string "brew --prefix erlang")))))

(use-package exec-path-from-shell
  :if system-type-darwin
  :straight t
  :config (exec-path-from-shell-initialize))

(repeat-mode)

;;; themes

(dolist (package
         '(dracula-theme
           nord-theme
           solarized-theme
           doom-themes
           modus-themes))
  (straight-use-package package))

(load-theme 'modus-operandi t)
