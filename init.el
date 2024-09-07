;;; init.el --- Emacs configuration

;;; Commentary:
;;

;;; Code:

;;; bootstrap `straight-use-package'

(defvar bootstrap-version)
(setq straight-check-for-modifications '(check-on-save))

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-to-list 'straight-built-in-pseudo-packages 'project)

;;; local settings and variables

(defconst system-type-darwin (eq system-type 'darwin)
  "Mac or not.")

(defconst at-work (file-directory-p (file-name-concat (expand-file-name "~") "/work"))
  "What computer is this.")

(defvar local-settings-file "~/.priv/elisp/local.el"
  "File used for private and local settings.")

(defvar org-bookmarks-file nil
  "File used for org capture of book")

(load local-settings-file t)

(use-package esup
  :straight t
  :init
  (setq esup-depth 0))

(use-package use-package
  :custom (use-package-enable-imenu-support t))

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
      recenter-positions '(top middle bottom)
      confirm-kill-emacs #'yes-or-no-p)

(defalias 'yes-or-no-p 'y-or-n-p)

;;; emacs security

(setq enable-local-variables t
      network-security-level 'paranoid
      ffap-machine-p-known 'reject
      epa-pinentry-mode 'loopback)

;;; env

(use-package exec-path-from-shell
  :if system-type-darwin
  :straight t
  :config (exec-path-from-shell-initialize))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;;; editing

(repeat-mode)

(set-language-environment 'utf-8)
(delete-selection-mode t)
(electric-pair-mode)

(setq sentence-end-double-space nil
      save-interprogram-paste-before-kill t)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 78)

(show-paren-mode 1)

(use-package comment-dwim-2
  :straight t
  :bind ("M-;" . comment-dwim-2))

(use-package editorconfig
  :straight t
  :config (editorconfig-mode +1))

(use-package goto-last-change
  :straight t
  :bind ("M-g ." . goto-last-change))

(use-package avy
  :straight t
  :bind (("M-g w" . avy-goto-word-1)
         ("M-g l" . avy-goto-line)
         ("M-g f" . avy-goto-char-in-line)
         ("M-g M-l" . avy-copy-line)
         ("M-g g" . avy-goto-line))
  :config (setq avy-all-windows nil))

;;; visual aid

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode))

(use-package highlight-numbers
  :straight t
  :hook ((prog-mode yaml-mode) . highlight-numbers-mode))

(use-package display-line-numbers
  :bind (:map my-toggle-prefix-map ("l" . display-line-numbers-mode)))

(use-package olivetti
  :straight t
  :config (setq-default olivetti-body-width 0.5)
  :bind (:map my-toggle-prefix-map
              ("o" . olivetti-mode)))

(use-package mood-line
  :straight t
  :custom (mood-line-format mood-line-format-default)
  :config (mood-line-mode))

(use-package spacious-padding
  :straight t
  :config (spacious-padding-mode))

(use-package page-break-lines
  :straight t
  :config (global-page-break-lines-mode))

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

(use-package ace-window
  :straight t
  :bind ("C-x O" . ace-window))

;;; dired

(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :init
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
    :bind (:map dired-mode-map ("\\" . dired-narrow))))

;;; files

(setq delete-by-moving-to-trash t)

(use-package recentf
  :config
  (setq recentf-exclude '("deft/" ".gpg")
        recentf-max-saved-items 500)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  :init
  (recentf-mode t)
  (use-package consult
    :bind ("C-x C-r" . consult-recent-file)))

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


;;; flymake

(use-package flymake
  :bind (:map my-toggle-prefix-map
              ("f" . flymake-mode))
  :init
  (use-package consult
    :bind ("C-c f" . consult-flymake)))

;;; projects / vc

(setq project-vc-extra-root-markers '(".project.el"))

(use-package git-link
  :straight t)

(use-package magit
  :straight t
  :bind ("C-c v" . magit-status)
  :config
  (setq magit-save-repository-buffers 'dontask
        magit-display-buffer-function #'display-buffer))

;;; completion

(use-package hippie-exp
  :bind ("C-\\" . hippie-expand))

(use-package consult
  :straight t
  :bind (("C-x b" . consult-buffer)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g o" . consult-outline)
         ("M-s l" . consult-line)
         ("M-s M-l" . consult-line-multi)
         ("M-g SPC" . consult-mark)
         ("M-g M-g" . consult-goto-line)
         :map org-mode-map
         ("M-g o" . consult-org-heading)
         :map minibuffer-local-map
         ("M-r" . consult-history)
         :map project-prefix-map
         ("b" . consult-project-buffer))
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

  ;; NOTE: feels like always `edited' is on top of the list but `buffer' would
  ;; be nicer?
  (add-to-list 'consult-buffer-sources 'my-consult--source-edited-buffers t)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (use-package consult-dir
    :straight t
    :config (setq consult-dir-default-command #'dired)
    :bind (("C-x C-d" . consult-dir)
           :map vertico-map
           ("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file)))

  (straight-use-package 'embark-consult))

(use-package embark
  :straight t
  :bind ("C-." . embark-act))

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :config
  (setq vertico-resize 'grow-only)
  ;; https://github.com/minad/consult#miscellaneous
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (setq vertico-multiform-commands
        '((consult-imenu buffer)
          (consult-grep buffer)
          (consult-outline buffer)
          (consult-git-grep buffer)
          (consult-line buffer)
          (execute-extended-command unobtrusive)))
  :init
  (vertico-mode)
  (vertico-multiform-mode t)

  (use-package orderless
    :straight t
    :init
    (setq completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion)))))

  (use-package marginalia
    :straight t
    :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
    :init
    (marginalia-mode t)))






(use-package corfu
  ;; Enable `corfu-popupinfo-mode' to get info popup about selected candidate
  :straight t
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.5
        corfu-count 10)
  (setq tab-always-indent 'complete)
  :init
  (global-corfu-mode))

(savehist-mode)

;;; org

(use-package org
  :straight t
  :config
  (setq org-log-done 'time
        org-src-fontify-natively t)

  (use-package ox-pandoc
    :if (executable-find "pandoc")
    :straight t)

  (unless at-work
    (use-package org-web-tools
      :straight t)

    (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

    (bind-keys :prefix "C-c o"
               :prefix-map my-org-prefix-map
               ("c" . org-capture)
               ("a" . org-agenda))

    (setq org-capture-templates
          '(
            ("t" "Task"
             entry (file+headline "" "Tasks")
             "* TODO %?\n  %u\n  %a")

            ("b" "Bookmark (Clipboard)" entry (file+headline org-bookmarks-file "new")
             "** %(org-web-tools--org-link-for-url)\n:PROPERTIES:\n:TIMESTAMP: %t\n:END:%?\n" :empty-lines 1 :prepend t)))))

;;; grep

(use-package deadgrep
  :straight t
  :bind ("C-c g" . deadgrep))

(use-package wgrep
  :straight t)

;;; util apps

(use-package elfeed
  :straight t
  :bind (:map elfeed-search-mode-map ("l" . recenter-top-bottom)))

;;; defuns

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
           know-your-http-well
           markdown-mode
           nim-mode
           erlang
           yaml-mode
           lua-mode))
  (straight-use-package package))

(use-package lispy
  :straight t
  :hook (emacs-lisp-mode . lispy-mode))

(use-package go-mode
  :straight t
  :hook (go-mode . eglot-ensure))

(use-package rust-mode
  :straight t
  :hook (rust-mode . eglot-ensure)
  :config
  (use-package cargo
    :straight t
    :hook (rust-mode . cargo-minor-mode)))

;;; themes

(straight-use-package 'ef-themes)
(straight-use-package 'doom-themes)

(load-theme 'ef-frost t)

;;; meow

(use-package meow
  :straight t
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
        meow-use-clipboard t)
  (meow-leader-define-key
   ;; bindings for `SPC'
   ;; SPC runs the command meow-keypad
   '("j" . dired-jump)
   '("k" . kill-buffer)
   '("w" . avy-goto-word-1)
   '("b" . consult-buffer)
   '("i" . consult-imenu)
   '("o" . other-window)
   '("." . embark-act)
   (cons "p" project-prefix-map)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))

  ;; NOTE: this thing messes with `magit'
  ;; (meow-motion-overwrite-define-key
  ;;  '("j" . meow-next)
  ;;  '("k" . meow-prev)
  ;;  '("<escape>" . ignore))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))

  :init
  (meow-global-mode 1))

(defun my-move-to-scratch ()
  (interactive)
  (when (region-active-p)
    (kill-region (region-beginning) (region-end))
    (let ((scratch-file (expand-file-name "scratch.el" user-emacs-directory)))
      (with-current-buffer (or (get-file-buffer scratch-file) (find-file-other-window scratch-file))
        (goto-char (point-max))
        (insert "\n")
        (yank)))))

(use-package activities
  :disabled
  :straight t
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

(use-package substitute
  :straight t)
