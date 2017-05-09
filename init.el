(package-initialize)


;;; packages setup

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))


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

(set-frame-font "Inconsolata-14")
(add-to-list 'default-frame-alist '(font . "Inconsolata-14"))

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))

(defalias 'yes-or-no-p 'y-or-n-p)




;;; custom prefix

(define-prefix-command 'my-custom-key-map)
(global-set-key (kbd "C-c u") my-custom-key-map)

(define-prefix-command 'my-toggle-prefix-map)
(global-set-key (kbd "C-c t") my-toggle-prefix-map)



;;; defuns

(use-package defuns
  :load-path "lisp/"
  :demand
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

;;; dired-details

(use-package dired-details
  :config
  (setq-default dired-details-hidden-string "--- ")
  (dired-details-install))

;;; ivy completion / swiper / counsel

(use-package ivy
  :diminish ivy-mode
  :config
  (eval-after-load "eyebrowse"
    '(ivy-set-actions
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
  :bind ("C-s" . swiper))

(use-package counsel
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
             ("g" . counsel-git-grep)
             ("i" . counsel-imenu)
             ("f" . counsel-git)
             ("a" . counsel-ag)
             ("y" . counsel-yank-pop)
             ("m" . counsel-mark-ring)
             ("o" . counsel-outline)
             ("j" . counsel-file-jump))

  (setq counsel-find-file-at-point t)
  (defun my-counsel-git-grep ()
    "Run `counsel-git-grep' with a preset initial input. If
region active use that, if point is on a symbol use that
otherwise start with empty initial input."
    (interactive)
    (let ((initial-input (if (region-active-p)
                             (buffer-substring-no-properties (region-beginning) (region-end))
                           (thing-at-point 'symbol t))))
      (counsel-git-grep nil initial-input)))
  (bind-key "C-c g" #'my-counsel-git-grep)
  :init (counsel-mode 1))

;;; org mode

(use-package orglink
  :diminish orglink-mode
  :init (global-orglink-mode))

(use-package htmlize)
(use-package ox-twbs)
(use-package ox-reveal)

(use-package org
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
                                                           (sh . t)
                                                           (shell . t)
                                                           (emacs-lisp . t))))
  (setq org-src-fontify-natively t))

;;; company

(use-package company
  :diminish company-mode
  :config
  (setq company-tooltip-align-annotations t
        company-show-numbers t)
  (global-company-mode))

;;; editing / text related

(use-package comment-dwim-2
  :bind
  ("M-;" . comment-dwim-2))

(use-package drag-stuff
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
  :bind
  ("C-=" . er/expand-region))

(use-package easy-kill
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
  :bind (("C-c i" . iedit-mode)
          ("C-;" . iedit-mode)))

(use-package multiple-cursors
  :bind
  (("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)
   ("C-S-c C-S-c" . mc/edit-lines)))

(use-package smartparens
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
  :bind (:map my-custom-key-map ("d" . deft))
  :init
  (setq deft-extensions '("org")
        deft-text-mode 'org-mode
        deft-directory "~/.deft/"
        deft-auto-save-interval 5.0))

(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  (add-hook 'yaml-mode-hook 'highlight-numbers-mode))

(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom)
  (which-key-mode))

;;; eyebrowse

(use-package eyebrowse
  :init

  (defun my-ivy-eyebrowse ()
    (interactive)
    (let* ((candidates (--map (cons (eyebrowse-format-slot it)
                                    (car it))
                              (eyebrowse--get 'window-configs))))
      (ivy-read "Enter slot: " candidates
                :action (lambda (slot)
                          (eyebrowse-switch-to-window-config (cdr slot)))
                :preselect (my-eyebrowse-current-tag)
                :caller #'my-ivy-eyebrowse)))
  (ivy-set-actions
   'my-ivy-eyebrowse
   '(("k"
      (lambda (slot)
        (eyebrowse--delete-window-config (cdr slot)))
      "kill")
     ("r"
      (lambda (slot)
        (eyebrowse-rename-window-config (cdr slot) nil))
      "rename")))

  (defun my-move-buffer-to-next-free-slot (&optional buffer)
    (interactive "b")
    (call-interactively 'eyebrowse-create-window-config)
    (switch-to-buffer buffer))

  (defun my-projectile-eyebrowse (p)
    (interactive)
    (eyebrowse-create-window-config)
    (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) (projectile-default-project-name p))
    (projectile-switch-project-by-name p))

  (defun my-eyebrowse-current-tag ()
    (interactive)
    (let ((current-slot (eyebrowse--get 'current-slot))
          (window-configs (eyebrowse--get 'window-configs)))
      (nth 2 (assoc current-slot window-configs))))

  (defun ivy-switch-project-with-eyebrowse ()
    (interactive)
    (ivy-read
     "Switch to project: "
     (if (projectile-project-p)
         (cons (abbreviate-file-name (projectile-project-root))
               (projectile-relevant-known-projects))
       projectile-known-projects)
     :action  #'my-projectile-eyebrowse))

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

  (setq eyebrowse-mode-line-style 'smart)
  (setq eyebrowse-close-window-config-prompt t)
  (setq eyebrowse-new-workspace t)

  (setq eyebrowse-keymap-prefix (kbd "C-c l"))
  (setq eyebrowse-wrap-around t)

  (eyebrowse-mode t))

;;; projectile

(use-package projectile
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
    "Run `counsel-git-grep' in project."
    (call-interactively #'counsel-git-grep))
  (def-projectile-commander-method ?n
    "Run `neotree-projectile-action' in project."
    (call-interactively #'neotree-projectile-action))

  :init
  (projectile-global-mode))

;;; hydra

(use-package hydra
  :init
  (defhydra hydra-my-compilation (global-map "M-g"  :color red)
    "Compilation"
    ("p" previous-error "Previous error")
    ("n" next-error "Next error")
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
  :bind ("C-x o" . ace-window))

;;; writeroom-mode

(use-package writeroom-mode
  :bind (:map my-custom-key-map ("w" . writeroom-mode))
  :init (setq writeroom-width 0.7))

;;; rotate

(use-package rotate
  :bind
  (:map my-custom-key-map
        ("SPC" . rotate-layout)
        ("r" . rotate-window)))

;;; browse-kill-ring

(use-package browse-kill-ring
  :bind
  ("M-y" . browse-kill-ring))

;;; editorconfig

(use-package editorconfig
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

;;; exec-path-from-shell

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :init
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

;;; flycheck

(use-package flycheck
  :bind (:map my-toggle-prefix-map
              ("f" . flycheck-mode))
  :init (setq flycheck-checker-error-threshold 500
              flycheck-check-syntax-automatically '(mode-enabled save)))

;;; git

(use-package magit
  :defer 5
  :bind ("C-c v" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package git-link
  :config (setq git-link-open-in-browser t))

;;; ag

(use-package ag
  :if (executable-find "ag")
  :config
  (require 'grep)
  (setq ag-highlight-search t))

;;; rainbow-mode

(use-package rainbow-mode
  :config
  (add-hook 'css-mode-hook 'rainbow-mode))

;;; undo-tree

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode t))

;;; ibuffer

(use-package ibuffer
  :bind
  (("C-x C-b" . ibuffer)
   ("M-3" . ibuffer)))

(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;;; neotree

(use-package neotree
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
  :bind (:map go-mode-map
              ("C-c C-p" . godoc-at-point)
              ("C-c C-e" . go-gopath-set-gopath)
              ("C-c C-r" . go-remove-unused-imports))
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  (add-hook 'go-mode-hook 'go-eldoc-setup))


;;; programming

(use-package clojure-mode)
(use-package erlang
  :config
  (setq erlang-root-dir "/usr/local/lib/erlang"))
(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode))

;;; ruby

(use-package yard-mode
  :diminish yard-mode
  :config
  (add-hook 'ruby-mode-hook 'yard-mode))
(use-package rvm
  :config
  (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby))

;;; keyfreq

(use-package keyfreq
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;; popwin

(use-package popwin
  :config
  (popwin-mode t)
  (global-set-key (kbd "C-c w") popwin:keymap)
  (push '(godoc-mode :noselect t) popwin:special-display-config)
  (push '(flycheck-error-list-mode :noselect t) popwin:special-display-config))

;;; hl-todo

(use-package hl-todo
  :bind (:map hl-todo-mode-map
              ("C-c h n" . hl-todo-next)
              ("C-c h p" . hl-todo-previous)
              ("C-c h o" . hl-todo-occur))

  :init (add-hook #'prog-mode-hook #'hl-todo-mode))

;;; mode-line

(use-package smart-mode-line
  :init
  (setq sml/theme 'light)
  (sml/setup)
  (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:")))

;;; wgrep

(use-package wgrep
  :config (setq wgrep-enable-key "e"))

;;; elixir

(use-package alchemist)

;;; misc

(use-package god-mode
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
  (bind-keys :prefix-map my-section-sign-key-map
             :prefix "§"
             ("b" . ivy-switch-buffer)
             ("f" . my-file-stuff-prefix-map)
             ("g" . my-counsel-git-grep)
             ("l" . my-eyebrowse-prefix-map)
             ("t" . my-toggle-prefix-map)
             ("u" . my-custom-key-map)
             ("p" . projectile-command-map)
             ("§" . ace-window)
             (";" . iedit-mode)
             ("1" . delete-other-windows))

  (let ((control-c-prefix (lookup-key global-map (kbd "C-c")))
        (control-x-prefix (lookup-key global-map (kbd "C-x"))))
    (bind-key "c" control-c-prefix my-section-sign-key-map)
    (bind-key "x" control-x-prefix my-section-sign-key-map)))

(load-theme 'tango-plus t)
(require 'local nil t)
