(package-initialize)


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
      recenter-positions '(top middle bottom))

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

;; FIXME: Fix when starting with `emacs --daemon' or similar
;; (when (member "Inconsolata" (font-family-list)))
(set-frame-font "Inconsolata-14")
(add-to-list 'default-frame-alist '(font . "Inconsolata-14"))

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))

(defalias 'yes-or-no-p 'y-or-n-p)




;;; packages setup

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;;; custom prefix

(define-prefix-command 'my-custom-key-map)
(global-set-key (kbd "C-c u") my-custom-key-map)

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
   ("s" . my-toggle-statistic-cookie-type))
  :config
  (add-hook 'after-init-hook #'my-init-terminal)
  (add-hook 'after-make-frame-functions #'my-init-terminal))

;;; built in's

(use-package eldoc
  :diminish eldoc-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package outline
  :diminish outline-minor-mode
  :config (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

  (bind-keys :prefix-map my-outline-prefix-map
             :prefix "C-c o"
             ("a" . outline-show-all)
             ("b" . outline-backward-same-level)
             ("c" . outline-hide-entry)
             ("d" . outline-hide-subtree)
             ("e" . outline-show-entry)
             ("f" . outline-forward-same-level)
             ("TAB" . outline-show-children)
             ("k" . outline-show-branches)
             ("l" . outline-hide-leaves)
             ("RET" . outline-insert-heading)
             ("n" . outline-next-visible-heading)
             ("o" . outline-hide-other)
             ("p" . outline-previous-visible-heading)
             ("q" . outline-hide-sublevels)
             ("s" . outline-show-subtree)
             ("t" . outline-hide-body)
             ("u" . outline-up-heading)
             ("j" . outline-move-subtree-down)
             ("k" . outline-move-subtree-up)
             ("@" . outline-mark-subtree)
             ("<" . outline-promote)
             (">" . outline-demote)))

(use-package recentf
  :bind
  ("C-x C-r" . ivy-recentf)
  :config
  (setq recentf-exclude '("deft/" ".gpg"))
  (setq recentf-max-saved-items 500)
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

;;; dired-details

(use-package dired-details
  :config
  (require 'dired-x)
  (setq-default dired-details-hidden-string "--- ")
  (dired-details-install))

;;; ivy completion / swiper / counsel

(use-package ivy
  :diminish ivy-mode
  :bind (:map my-custom-key-map ("p" . ivy-switch-project))
  :config
  (defun my-eshell-from-dir (dir)
    (interactive)
    (let ((default-directory dir))
      (eshell)))

  (defun my-projectile-switch-to-buffer (project)
    (interactive)
    (let ((default-directory project))
      (projectile-switch-to-buffer)))

  (defun my-ag-from-dir (dir)
    (interactive)
    (let ((input (read-from-minibuffer "Search string: ")))
      (ag input dir)))

  (defun ivy-switch-project ()
    (interactive)
    (ivy-read
     "Switch to project: "
     (if (projectile-project-p)
         (cons (abbreviate-file-name (projectile-project-root))
               (projectile-relevant-known-projects))
       projectile-known-projects)
     :action #'projectile-switch-project-by-name))

  :init
  (ivy-mode)
  (setq ivy-display-style 'fancy)

  (setq ivy-wrap t)

  (defun my-counsel-git-grep-in-directory (directory)
    (interactive)
    (let ((default-directory directory))
      (counsel-git-grep)))

  (ivy-set-actions
   'ivy-switch-project
   '(("d" dired "Open Dired in project's directory")
     ("v" projectile-vc "Open project root in vc-dir or magit")
     ("e" my-eshell-from-dir "Eshell")
     ("a" my-ag-from-dir "Ag in project")
     ("g" my-counsel-git-grep-in-directory "counsel-git-grep")
     ("b" my-projectile-switch-to-buffer "Switch to buffer in project")
     ("r" projectile-remove-known-project "Remove project(s)"))))

(use-package swiper
  ;; TODO: fix a advice before to exlude .gpg files
  ;; :bind ("C-s" . counsel-grep-or-swiper)
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
             ("a" . counsel-ag))

  (setq counsel-find-file-at-point t)
  :init (counsel-mode 1))

;;; org mode

(use-package htmlize)
(use-package worf
  :config (add-hook 'org-mode-hook 'worf-mode))
(use-package org-bullets
  :config (add-hook 'org-mode-hook 'org-bullets-mode))
(use-package ox-twbs)

(use-package org
  :config
  ;; Markdown export http://stackoverflow.com/a/22990257
  (eval-after-load "org" '(require 'ox-md nil t))

  :init
  (setq org-hide-emphasis-markers t)
  (setq org-log-done 'time)
  (setq org-babel-load-languages (quote
                                  ((ruby . t)
                                   (clojure . t)
                                   (sh . t)
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
  :config
  (drag-stuff-global-mode t))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark)
  (bind-keys :map easy-kill-base-map
             ("j" . easy-kill-expand)
             ("k" . easy-kill-shrink)))

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
        deft-directory "~/.deft/"))

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
  (setq my-skip-slot-0 t)
  (defun my--next-free-slot ()
    (let ((slot (if my-skip-slot-0
                    1
                  0))
          (free-slot nil))
      (while (and (not free-slot) (> 10 slot))
        (if (eyebrowse--window-config-present-p slot)
            (setq slot (1+ slot))
          (setq free-slot slot)))
      free-slot))

  (defun my-switch-to-next-free-slot ()
    (interactive)
    (let ((slot (my--next-free-slot)))
      (when slot
        (let ((new-tag nil))
          (when current-prefix-arg
            (setq new-tag (read-string "Tag: ")))
          (funcall (intern (format "%s%d" "eyebrowse-switch-to-window-config-" slot)))
          (when new-tag (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) new-tag))))))

  (defun my-move-buffer-to-next-free-slot (&optional buffer)
    (interactive "b")
    (call-interactively 'my-switch-to-next-free-slot)
    (switch-to-buffer buffer))

  (defun my-projectile-eyebrowse (p)
    (interactive)
    (my-switch-to-next-free-slot)
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
             ("c" . my-switch-to-next-free-slot)
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
  ;; NOTE: handled by smart-mode-line
  :diminish projectile-mode
  :bind (("M-7" . projectile-switch-to-buffer-other-window)
         ("M-8" . projectile-switch-to-buffer))
  :config
  (add-to-list 'projectile-globally-ignored-directories "_build")
  (add-to-list 'projectile-globally-ignored-directories "deps")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  :init

  ;; TODO: add support for this when not in git project
  (setq projectile-use-git-grep t
        projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'projectile-commander)
  (projectile-global-mode)
  (def-projectile-commander-method ?G
    "Run `counsel-git-grep' in project."
    (call-interactively #'counsel-git-grep)))

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
    ("r" ivy-recentf "ivy-recentf")
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
   ("M-g M-k" . my-kill-line-with-avy)
   ("M-g g" . avy-goto-line))
  :init
  (defun my-kill-line-with-avy ()
    (interactive)
    (save-excursion
      (avy-goto-line)
      (kill-whole-line))))

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
  :if (executable-find "editorconfig")
  :config (editorconfig-mode 1))

;;; exec-path-from-shell

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-initialize))

;;; flycheck

(use-package flycheck
  :bind (:map my-custom-key-map ("f" . flycheck-mode))
  :init (setq flycheck-checker-error-threshold 500
              flycheck-check-syntax-automatically '(mode-enabled save)))

;;; magit

(use-package magit
  :bind
  (:map my-custom-key-map ("m" . magit-status))
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

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
                (ibuffer-do-sort-by-alphabetic))))
  (setq ibuffer-formats
        ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-ibuffer.el
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)
          (mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process))))

;;; neotree

(use-package neotree
  :bind ("C-c n" . my-neotree-toggle)
  :config
  (defun my-neotree-toggle ()
    (interactive)
    (cond ((my--projectile-active) (neotree-projectile-action))
          ((buffer-file-name) (neotree-find))
          ((neotree-dir default-directory))))

  (setq neo-theme 'nerd)
  (setq neo-window-width 35)

  ;; https://www.emacswiki.org/emacs/NeoTree#toc8
  (when neo-persist-show
    (add-hook 'popwin:before-popup-hook
              (lambda () (setq neo-persist-show nil)))
    (add-hook 'popwin:after-popup-hook
              (lambda () (setq neo-persist-show t)))))

;;; golang

(use-package company-go)
(use-package go-eldoc)
(use-package go-gopath)
(use-package go-mode
  :bind (:map go-mode-map ("C-c C-p" . godoc-at-point))
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (define-key go-mode-map (kbd "C-c C-e") #'go-gopath-set-gopath)

  (bind-key "C-c C-r" 'go-remove-unused-imports go-mode-map))


;;; programming

(use-package clojure-mode)
(use-package css-mode)
(use-package erlang
  :config
  (setq erlang-root-dir "/usr/local/lib/erlang"))

(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode))

(use-package feature-mode)
(use-package json-mode)
(use-package markdown-mode)
(use-package yaml-mode)

;;; ruby

(use-package bundler)
(use-package yard-mode)
(use-package inf-ruby)
(use-package rvm)
(use-package rubocop)
(use-package ruby-mode
  :diminish yard-mode

  :bind (:map ruby-mode-map
              ;; ("C-c m b" . my-insert-pry-binding)
              ("<C-return>" . my-insert-ruby-end))

  :config
  (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)

  (add-hook 'ruby-mode-hook 'yard-mode)

  (defun empty-line ()
    (= 0 (length (replace-regexp-in-string "^[ \t]*\n$" "" (thing-at-point 'line t)))))

  (defun my-insert-ruby-end ()
    (interactive)
    (unless (empty-line)
      (progn
        (end-of-line)
        (newline)))
    (insert "end")
    (indent-according-to-mode))

  (defun my-insert-pry-binding ()
    "Insert the string `require pry;binding.pry' at point. When line is non-empty insert
  on a new line below."
    (interactive)
    (save-excursion
      (if (string= "\n" (thing-at-point 'line))
          (insert "require 'pry'; binding.pry")
        (progn
          (end-of-line)
          (newline-and-indent)
          (insert "require 'pry'; binding.pry")))))

  (defun my-rubyfy-name (name)
    (mapconcat 'identity (mapcar 'capitalize (split-string name "_" t)) ""))

  (defun my-generate-ruby-class-or-module-name (buffer type)
    (let ((filename (file-name-base (buffer-file-name buffer))))
      (format "%s %s\n\nend" (symbol-name type) (my-rubyfy-name filename))))


  (defun my-insert-ruby-class-or-module-name (buffer type)
    (insert (my-generate-ruby-class-or-module-name buffer type))
    (forward-line -1))

  (defun my-insert-ruby-class-name ()
    (interactive)
    (my-insert-ruby-class-or-module-name (current-buffer) 'class))

  (defun my-insert-ruby-module-name ()
    (interactive)
    (my-insert-ruby-class-or-module-name (current-buffer) 'module)))

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
  :config (add-hook #'prog-mode-hook #'hl-todo-mode))

;;; mode-line

(use-package smart-mode-line
  :config
  (setq sml/theme 'automatic)
  (sml/setup)
  (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:")))

;;; wgrep

(use-package wgrep-ag
  :if (executable-find "ag"))

(use-package wgrep
  :config (setq wgrep-enable-key "e"))

;;; elixir

(use-package elixir-mode)

(use-package alchemist
  :config
  ;; https://github.com/tonini/alchemist.el/issues/71
  ;; (setq alchemist-goto-erlang-source-dir "/usr/local/Cellar/erlang/18.2.1/lib/erlang/")
  (setq alchemist-goto-erlang-source-dir "/Users/daniel/Downloads/otp_src_18.3")
  ;; (setq alchemist-goto-elixir-source-dir "/usr/local/Cellar/elixir/1.2.3/")
  (setq alchemist-goto-elixir-source-dir "/users/daniel/Downloads/elixir-1.2.3/")
  )

;;; misc

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
           ("r" . ivy-recentf)
           ("s" . save-buffer)
           ("b" . bookmark-set)
           ("o" . find-file-other-window))

(bind-keys :prefix-map my-jump-prefix-map
           :prefix "C-c j"
           ("e" . avy-goto-word-0)
           ("w" . avy-goto-word-1)
           ("l" . avy-goto-line)
           ("c" . avy-goto-char)
           ("j" . avy-goto-line-below)
           ("k" . avy-goto-line-above)
           ("f" . avy-goto-char-in-line))

(bind-keys :prefix-map my-toggle-prefix-map
           :prefix "C-c t"
           ("l" . linum-mode)
           ("h" . hl-line-mode)
           ("s" . scroll-bar-mode)
           ("b" . blink-cursor-mode)
           ("v" . visual-line-mode)
           ("w" . whitespace-mode))
