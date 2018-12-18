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
  (add-to-list 'load-path (concat user-emacs-directory "lisp"))


  ;; Specifying :straight t is unnecessary if you set
  ;; straight-use-package-by-default to a non-nil value.
  (straight-use-package 'use-package)
  (straight-use-package 'diminish)

  ;; (setq straight-use-package-by-default t)
  )

;;; emacs settings

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq column-number-mode t
      inhibit-startup-message t
      line-number-mode t
      ns-pop-up-frames nil
      ring-bell-function 'ignore
      delete-by-moving-to-trash t
      recenter-positions '(top middle bottom)
      save-interprogram-paste-before-kill t)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 78)

(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell"))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode t)

(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)

(when (member "Inconsolata" (font-family-list))
  (set-frame-font "Inconsolata-14")
  (add-to-list 'default-frame-alist '(font . "Inconsolata-14")))

(defalias 'yes-or-no-p 'y-or-n-p)




;;; custom prefix

(define-prefix-command 'my-custom-key-map)
(global-set-key (kbd "C-c u") my-custom-key-map)

(define-prefix-command 'my-toggle-prefix-map)
(global-set-key (kbd "C-c t") my-toggle-prefix-map)

(define-prefix-command 'my-section-sign-prefix-map)
(global-set-key (kbd "§") my-section-sign-prefix-map)

(let ((control-c-prefix (lookup-key global-map (kbd "C-c")))
      (control-x-prefix (lookup-key global-map (kbd "C-x"))))
  (bind-key "c" control-c-prefix my-section-sign-prefix-map)
  (bind-key "x" control-x-prefix my-section-sign-prefix-map))

;;; defuns

(use-package defuns
  :load-path "lisp/"
  :demand                             ;
  :bind
  (("M-s o" . occur-dwim)
   ("C-M-=" . align-to-equals)
   ("C-c d" . duplicate-current-line-or-region)
   :map my-custom-key-map
   ("t" . my-insert-current-time)
   ("c" . my-paste-to-new-buffer)
   ("o" . my-org-scratch-buffer)
   ("q" . my-count-words-in-org-subtree)
   ("s" . my-toggle-statistic-cookie-type)))



;;; built in's

(use-package eldoc
  :diminish eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package recentf
  :config
  (setq recentf-exclude '("deft/" ".gpg")
        recentf-max-saved-items 500)
  :init
  (recentf-mode t))

(use-package server
  :config
  (when (and (fboundp 'server-running-p)
           (not (server-running-p)))
      (server-start)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package winner
  :config (winner-mode t))

(use-package saveplace
  :config
  (setq save-place-ignore-files-regexp "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\|.gpg\\)$")
  (if (fboundp 'save-place-mode)
      (save-place-mode 1)
    ;; for (version< emacs-version "25.1")
    (setq-default save-place t))
  (setq save-place-file (concat user-emacs-directory "places")))

(use-package dired-x
  :bind ("C-c j" . dired-jump-other-window)
  :init
  (setq dired-bind-jump nil))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;;; ivy completion / swiper / counsel

(straight-use-package 'amx)
(straight-use-package 'ivy-hydra)

(use-package ivy
  :straight t
  :diminish ivy-mode
  :config
  (eval-after-load "eyebrowse"
    '(ivy-add-actions
      'ivy-switch-buffer
      '(("e"
         (lambda (buffer)
           (call-interactively 'eyebrowse-create-window-config)
           (ivy--switch-buffer-action buffer))
         "new eyebrowse window config"))))
  :init
  (setq ivy-use-virtual-buffers t
        ivy-display-style 'fancy
        ivy-wrap t)
  (ivy-mode))

(use-package swiper
  :straight t
  :bind ("C-s" . swiper))

(use-package counsel
  :straight t
  :diminish counsel-mode
  :bind
  (("M-x" . counsel-M-x)
   ("C-c C-m" . counsel-M-x)
   ("C-x C-m" . counsel-M-x)
   ("C-c M-x" . execute-extended-command)
   ("C-h v" . counsel-describe-variable)
   ("C-h f" . counsel-describe-function)
   ("C-x C-f" . counsel-find-file))
  :config

  (bind-keys :prefix-map my-counsel-prefix-map
             :prefix "C-c c"
             ("i" . counsel-imenu)
             ("f" . counsel-git)
             ("r" . counsel-rg)
             ("y" . counsel-yank-pop)
             ("m" . counsel-mark-ring)
             ("o" . counsel-outline)
             ("j" . counsel-file-jump))

  (setq counsel-find-file-at-point t)

  (defun my-counsel-rg ()
    "Run `counsel-rg' with a preset initial input. If
region active use that, if point is on a symbol use that
otherwise start with empty initial input."
    (interactive)
    (let ((initial-input (if (region-active-p)
                             (buffer-substring-no-properties (region-beginning) (region-end))
                           (thing-at-point 'symbol t))))
      (counsel-rg initial-input)))


  (bind-key "C-c g" #'my-counsel-rg)

  :init (counsel-mode 1))

;;; org mode

(use-package orglink
  :straight t
  :diminish orglink-mode
  :init (global-orglink-mode))

(straight-use-package 'htmlize)
(straight-use-package 'ox-twbs)
(straight-use-package 'ox-reveal)
(straight-use-package 'worf)
(straight-use-package 'org-bullets)

(use-package org
  :straight t
  :config
  (add-hook 'org-mode-hook #'worf-mode)
  (add-hook 'org-mode-hook #'org-bullets-mode)

  ;; Markdown export http://stackoverflow.com/a/22990257
  (eval-after-load "org" '(require 'ox-md nil t))
  (setq org-hide-emphasis-markers t
        org-log-done 'time)
  (org-babel-do-load-languages 'org-babel-load-languages (quote
                                                          ((ruby . t)
                                                           (clojure . t)
                                                           (shell . t)
                                                           (emacs-lisp . t))))
  (setq org-src-fontify-natively t))

;;; markdown

(straight-use-package 'markdown-mode)

;;; company

(use-package company
  :straight t
  :diminish company-mode
  :config
  (setq company-tooltip-align-annotations t
        company-show-numbers t)
  (global-company-mode))

;;; editing / text related

(use-package comment-dwim-2
  :straight t
  :bind
  ("M-;" . comment-dwim-2))

(use-package drag-stuff
  :straight t
  :diminish drag-stuff-mode
  :bind (:map my-toggle-prefix-map
              ("d" . drag-stuff-mode)
              ("D" . drag-stuff-global-mode))
  :config
  (drag-stuff-define-keys)
  :init
  (add-hook 'prog-mode-hook 'drag-stuff-mode)
  (add-hook 'yaml-mode-hook 'drag-stuff-mode))

(use-package expand-region
  :straight t
  :bind
  ("C-=" . er/expand-region))

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
             ("SPC" . easy-kill-mark-region)))

(use-package iedit
  :straight t
  :bind (("C-c i" . iedit-mode)
          ("C-;" . iedit-mode)))

(use-package multiple-cursors
  :straight t
  :bind
  (("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)
   ("C-S-c C-S-c" . mc/edit-lines)))

(use-package smartparens
  :straight t
  :diminish (smartparens-mode . "()")
  :bind (:map my-toggle-prefix-map
              ("s" . smartparens-global-mode))
  :init
  (require 'smartparens-config)
  (bind-keys
   :map smartparens-mode-map
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-k"   . sp-kill-hybrid-sexp)
   ("M-k"   . sp-backward-kill-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("C-M-d" . delete-sexp)
   ("M-["   . sp-backward-unwrap-sexp)
   ("M-]"   . sp-unwrap-sexp))

  (smartparens-global-mode t))

;;; utils

(use-package deft
  :straight t
  :bind (:map my-custom-key-map ("d" . deft))
  :init
  (setq deft-extensions '("org")
        deft-text-mode 'org-mode
        deft-directory "~/.deft/"
        deft-auto-save-interval 5.0))

(use-package highlight-numbers
  :straight t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  (add-hook 'yaml-mode-hook 'highlight-numbers-mode))

(use-package which-key
  :straight t
  :diminish which-key-mode
  :init
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom)
  (which-key-mode))

;;; eyebrowse

(use-package eyebrowse
  :straight t
  :init
  (defun my-close-all-other-slots ()
    (interactive)
    (let ((all-slots (mapcar 'car (eyebrowse--get 'window-configs)))
          (current-slot (eyebrowse--get 'current-slot)))
      (dolist (slot all-slots)
        (unless (= slot current-slot)
          (eyebrowse--delete-window-config slot)))))

  (defun my-close-slots-to-the-right ()
    (interactive)
    (let ((all-slots (mapcar 'car (eyebrowse--get 'window-configs)))
          (current-slot (eyebrowse--get 'current-slot)))
      (dolist (slot all-slots)
        (unless (<= slot current-slot)
          (eyebrowse--delete-window-config slot)))))

  (bind-keys :prefix-map my-eyebrowse-prefix-map
             :prefix "C-c l"
             ("c" . eyebrowse-create-window-config)
             ("ko" . my-close-all-other-slots)
             ("kr" . my-close-slots-to-the-right)
             ("n" . eyebrowse-next-window-config)
             ("p" . eyebrowse-prev-window-config)
             ("l" . eyebrowse-last-window-config)
             ("kk" . eyebrowse-close-window-config)
             ("j" . eyebrowse-switch-to-window-config)
             ("r" . eyebrowse-rename-window-config)

             ("0" . eyebrowse-switch-to-window-config-0)
             ("1" . eyebrowse-switch-to-window-config-1)
             ("2" . eyebrowse-switch-to-window-config-2)
             ("3" . eyebrowse-switch-to-window-config-3)
             ("4" . eyebrowse-switch-to-window-config-4)
             ("5" . eyebrowse-switch-to-window-config-5)
             ("6" . eyebrowse-switch-to-window-config-6)
             ("7" . eyebrowse-switch-to-window-config-7)
             ("8" . eyebrowse-switch-to-window-config-8)
             ("9" . eyebrowse-switch-to-window-config-9))

  (setq eyebrowse-mode-line-style 'always)
  (setq eyebrowse-close-window-config-prompt t)
  (setq eyebrowse-new-workspace t)

  (setq eyebrowse-keymap-prefix (kbd "C-c l"))
  (setq eyebrowse-wrap-around t)

  (eyebrowse-mode t))

;;; projectile

(use-package projectile
  :straight t
  :diminish projectile-mode
  :bind (("M-7" . projectile-switch-to-buffer-other-window)
         ("M-8" . projectile-switch-to-buffer))
  :config
  (add-to-list 'projectile-globally-ignored-directories "_build")
  (add-to-list 'projectile-globally-ignored-directories "deps")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (setq projectile-use-git-grep t
        projectile-completion-system 'ivy
        projectile-switch-project-action #'projectile-commander)
  (def-projectile-commander-method ?G
    "Run `counsel-rg' in project."
    (call-interactively #'counsel-rg))
  (def-projectile-commander-method ?n
    "Run `neotree-projectile-action' in project."
    (call-interactively #'neotree-projectile-action))

  :init
  (projectile-global-mode))

;;; hydra

(use-package hydra
  :straight t
  :init
  (defhydra hydra-my-compilation (global-map "M-g" :color red :columns 2)
    "Compilation"
    ("p" previous-error "Previous error")
    ("n" next-error "Next error")
    ("l" recenter-top-bottom "recenter")
    ("L" reposition-window "reposition")
    ("0" first-error "First error")
    ("q" nil "quit"))


  (defhydra hydra-my-utils (:color blue :columns 2)
    "Utils"
    ;; row 1
    ("s" save-buffer "save buffer")
    ("pf" projectile-find-file "projectile-find-file")

    ;; row 2
    ("f" my-find-file-or-projectile-find-file "find file")
    ("pr" projectile-recentf "projectile-recentf" )

    ;; row 3
    ("x" counsel-M-x "M-x")
    ("pb" projectile-switch-to-buffer "projectile-switch-to-buffer")

    ;; row 4
    ("/" query-replace-regexp "query-replace-regexp")
    ("pv" projectile-vc "projectile-vc")

    ("v" magit-status "magit-status")
    ("r" counsel-recentf "counsel-recentf")
    ("b" ivy-switch-buffer "ivy-switch-buffer")

    ("q" nil "cancel"))

  (bind-key "M-§" 'hydra-my-utils/body)
  (bind-key "C-0" 'hydra-my-utils/body)
  (bind-key "h" 'hydra-my-utils/body my-custom-key-map))

;;; avy



(use-package avy
  :straight t
  :bind
  (("M-g e" . avy-goto-word-0)
   ("M-g w" . avy-goto-word-1)
   ("M-g l" . avy-goto-line)
   ("M-g c" . avy-goto-char)
   ("M-g f" . avy-goto-char-in-line)
   ("M-g M-l" . avy-copy-line)
   ("M-g M-m" . avy-move-line)
   ("M-g M-k" . avy-kill-whole-line)
   ("M-g g" . avy-goto-line)
   ("M-g s" . avy-goto-char-timer))
  :config (setq avy-timeout-seconds 0.2))

;;; ace-window

(use-package ace-window
  :straight t
  :bind ("C-x o" . ace-window))

;;; writeroom-mode

(use-package writeroom-mode
  :straight t
  :bind (:map my-custom-key-map ("w" . writeroom-mode))
  :init (setq writeroom-width 0.7))

;;; rotate

(use-package rotate
  :straight t
  :bind
  (:map my-custom-key-map
        ("SPC" . rotate-layout)
        ("r" . rotate-window)))

;;; browse-kill-ring

(use-package browse-kill-ring
  :straight t
  :bind
  ("M-y" . browse-kill-ring))

;;; editorconfig

(use-package editorconfig
  :straight t
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

;;; exec-path-from-shell

(use-package exec-path-from-shell
  :straight t
  :if (eq system-type 'darwin)
  :init
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

;;; flycheck

(use-package flycheck
  :straight t
  :bind (:map my-toggle-prefix-map
              ("f" . flycheck-mode))
  :init (setq flycheck-checker-error-threshold 500
              flycheck-check-syntax-automatically '(mode-enabled save)))

;;; git

(use-package magit
  :straight t
  :defer 5
  :bind ("C-c v" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read
        magit-save-repository-buffers 'dontask))

(use-package git-link
  :straight t
  :config (setq git-link-open-in-browser t))

;;; rainbow-mode

(use-package rainbow-mode
  :straight t
  :config
  (add-hook 'css-mode-hook 'rainbow-mode))

;;; undo-tree

(use-package undo-tree
  :straight t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode t))

;;; ibuffer

(use-package ibuffer
  :straight t
  :bind
  (("C-x C-b" . ibuffer)
   ("M-3" . ibuffer)))

(use-package ibuffer-vc
  :straight t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;;; neotree

(use-package neotree
  :straight t
  :bind ("C-c n" . my-neotree-toggle)
  :config
  (defun my-neotree-toggle ()
    "Toggle neotree window. Hide it if already showing, else set
root node to either `projectile-root', file directory or
`default-directory'."
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (cond ((projectile-project-p) (neotree-projectile-action))
            ((buffer-file-name) (neotree-find))
            (t (neotree-dir default-directory)))))

  ;; From https://github.com/jaypei/emacs-neotree/pull/110
  (defun neotree-resize-window (&rest _args)
    "Resize neotree window.
https://github.com/jaypei/emacs-neotree/pull/110"
    (interactive)
    (neo-buffer--with-resizable-window
     (let ((fit-window-to-buffer-horizontally t))
       (fit-window-to-buffer))))

  (defun my-neo-window-toggle-size ()
    "Toggle neotree window size between `neo-window-width' and
using `fit-window-to-buffer'."
    (interactive)
    (if (eq (window-body-width) neo-window-width)
        (call-interactively #'neotree-resize-window)
      (neo-window--zoom 'minimize)))

  (defun my-neo-file-view (full-path &optional arg)
    "Open file at at point in neotree in `view-mode'."
    (neo-global--select-mru-window arg)
    (find-file full-path)
    (view-mode t))

  (bind-key "w" #'my-neo-window-toggle-size neotree-mode-map)
  (bind-key "v" (neotree-make-executor :file-fn 'my-neo-file-view) neotree-mode-map)

  (setq neo-window-fixed-size nil
        neo-theme 'nerd
        neo-window-width 35))

;;; golang

(use-package go-mode
  :straight t
  :bind (:map go-mode-map
              ("C-c C-p" . godoc-at-point)
              ("C-c C-e" . go-gopath-set-gopath)
              ("C-c C-r" . go-remove-unused-imports))
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  (add-hook 'go-mode-hook 'go-eldoc-setup))


;;; programming

(straight-use-package 'clojure-mode)
(use-package erlang
  :straight t
  :config
  (setq erlang-root-dir "/usr/local/lib/erlang"))
(use-package lispy
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode))

;;; ruby

(use-package yard-mode
  :straight t
  :diminish yard-mode
  :config
  (add-hook 'ruby-mode-hook 'yard-mode))
(use-package rvm
  :straight t
  :config
  (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby))

;;; keyfreq

(use-package keyfreq
  :straight t
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;; popwin

(use-package popwin
  :straight t
  :config
  (popwin-mode t)
  (global-set-key (kbd "C-c w") popwin:keymap)
  (push '(godoc-mode :noselect t) popwin:special-display-config)
  (push '(flycheck-error-list-mode :noselect t) popwin:special-display-config))

;;; hl-todo

(use-package hl-todo
  :straight t
  :bind (:map hl-todo-mode-map
              ("C-c h n" . hl-todo-next)
              ("C-c h p" . hl-todo-previous)
              ("C-c h o" . hl-todo-occur))

  :init (add-hook #'prog-mode-hook #'hl-todo-mode))

;;; mode-line

(use-package smart-mode-line
  :straight t
  :init
  (setq sml/theme 'light)
  (sml/setup)
  (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:")))

;;; wgrep

(use-package wgrep
  :straight t
  :config (setq wgrep-enable-key "e"))

;;; elixir

(straight-use-package 'alchemist)

;;; misc

(use-package god-mode
  :straight t
  :bind (("C-c u g" . god-local-mode)
         :map god-local-mode-map
         ("i" . god-local-mode)
         ("w" . avy-goto-word-1)
         ("." . repeat))
  :init (when (memq window-system '(mac ns))
          (global-set-key (kbd "<escape>") 'god-local-mode))
  :config
  (add-hook 'god-mode-enabled-hook (lambda () (hl-line-mode 1)))
  (add-hook 'god-mode-disabled-hook (lambda () (hl-line-mode -1))))


(use-package goto-last-change
  :straight t
  :bind ("M-g i" . goto-last-change))


(use-package ranger
  :straight t
  :config
  (defun my-projectile-ranger ()
    (interactive)
    (if (projectile-project-p)
        (ranger (projectile-project-root))
      (ranger)))
  (bind-keys ("C-c R" . my-projectile-ranger)
             ("C-c r" . ranger)))

(use-package super-save
  :straight t
  :diminish super-save-mode
  :config
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-list-file-prefix nil
        auto-save-default nil
        make-backup-files nil)
  (add-to-list 'super-save-triggers 'ace-window)
  :init
  (super-save-mode +1))

(use-package my-no-repeat-mode
  :load-path "lisp/"
  :demand
  :bind (:map my-toggle-prefix-map
              ("q" . my-no-repeat-mode)
              ("Q" . my-no-repeat-global-mode))

  ;; (setq my-no-repeat-show-hint 't)
  ;; (setq my-no-repeat-show-hint nil)
  :config
  (mapc
   'my-no-repeat-kbd
   '("C-n"
     "C-p"
     "C-f"
     "C-b"
     "C-v"
     "M-v"
     "M-f"
     "M-b"
     "M-a"
     "M-e"
     "C-M-a"
     "C-M-e"
     "C-k"
     "C-d"
     "M-d")))


(bind-keys
 ("C-\\"  . hippie-expand)
 ("M-9"   . previous-buffer)
 ("M-0"   . next-buffer)
 ("M-1"   . delete-other-windows)
 ("M-2"   . company-complete)
 ("C-M-0" . delete-window)
 ("C-x O" . other-frame))

(bind-keys :prefix-map my-file-stuff-prefix-map
           :prefix "C-c f"
           ("u" . revert-buffer)
           ("f" . counsel-find-file)
           ("r" . counsel-recentf)
           ("s" . save-buffer)
           ("b" . bookmark-set)
           ("o" . find-file-other-window))

(bind-keys :map my-toggle-prefix-map
           ("l" . linum-mode)
           ("h" . hl-line-mode)
           ("b" . blink-cursor-mode)
           ("v" . visual-line-mode)
           ("w" . whitespace-mode))

(progn
  (straight-use-package 'apropospriate-theme)
  (straight-use-package 'creamsody-theme)
  (straight-use-package 'darktooth-theme)
  (straight-use-package 'dracula-theme)
  (straight-use-package 'gruvbox-theme)
  (straight-use-package 'leuven-theme)
  (straight-use-package 'nord-theme)
  (straight-use-package 'solarized-theme)
  (straight-use-package 'tango-plus-theme)
  (straight-use-package 'zerodark-theme)
  (load-theme 'tango-plus t))


(require 'local nil t)
