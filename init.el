;;; initialize straight.el
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

  (straight-use-package 'use-package))

;;;

(defconst system-type-darwin (eq system-type 'darwin)
  "Mac or not.")

(defvar local-settings-file "~/.priv/elisp/local.el"
  "File used for private and local settings.")

(when (file-exists-p local-settings-file)
  (load local-settings-file))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(progn
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))


(require 'init-buffer)
(require 'init-dired)
(require 'init-editing)
(require 'init-files)
(require 'init-gui)
(require 'init-keys)
(require 'init-prog)
(require 'init-themes)
(require 'init-vc)
(require 'init-window)
(require 'init-ui)
(require 'init-completing)

(require 'init-search)
(require 'init-utils)

(setq ring-bell-function 'ignore
      inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)



(setq enable-local-variables t
      network-security-level 'paranoid
      ffap-machine-p-known 'reject
      epa-pinentry-mode 'loopback)


(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(straight-use-package 'project)

(use-package embark
  :straight t
  :bind ("C-c e" . embark-act))


(use-package avy
  :straight t
  :bind
  (("M-g w" . avy-goto-word-1)
   ("M-g l" . avy-goto-line)
   ("M-g f" . avy-goto-char-in-line)
   ("M-g M-l" . avy-copy-line)
   ("M-g g" . avy-goto-line))
  :config (setq avy-all-windows nil))


(use-package company
  :straight t
  :bind ("M-2" . company-complete)
  :config
  (setq company-tooltip-align-annotations t
        company-show-numbers t)
  (global-company-mode))



(defun my-insert-current-time ()
  (interactive)
  (let ((time-str (format-time-string "%R ")))
    (if current-prefix-arg
        (message time-str)
      (insert time-str))))

(defun my-paste-to-new-buffer ()
  (interactive)
  (let ((content (current-kill 0))
        (buffer (generate-new-buffer-name "*clipboard contents*")))
    (switch-to-buffer-other-window buffer)
    (insert content)
    (set-text-properties (point-min) (point-max) nil)))

(defun my-org-scratch-buffer ()
  (interactive)
  (let ((default-directory (expand-file-name "~" )))
    (let ((buffer (generate-new-buffer-name "*org-scratch*")))
      (if current-prefix-arg
          (switch-to-buffer buffer)
        (switch-to-buffer-other-window buffer))
      (org-mode)
      (when (bound-and-true-p evil-mode)
        (evil-insert-state)))))

(defun my-count-words-in-org-subtree ()
  (interactive)
  (let ((count nil))
    (save-excursion
      (outline-previous-heading)
      (save-restriction
        (org-narrow-to-subtree)
        (setq count (format "%s" (count-words (point-min) (point-max))))))
    (if current-prefix-arg
        (insert count)
      (message count))))

(defun my--statistic-count-p ()
  (string-match "\\[[0-9]+/[0-9]+\\]$" (thing-at-point 'line t)))

(defun my--statistic-percent-p ()
  (string-match "\\[[[:digit:]]\\{0,3\\}%\\]$" (thing-at-point 'line t)))


(defun my-toggle-statistic-cookie-type ()
  (interactive)
  (save-excursion
    (org-previous-visible-heading 1)
    (cond ((my--statistic-count-p) (progn
                                    (end-of-line)
                                    (kill-sexp -1)
                                    (insert "[%]")
                                    (org-update-statistics-cookies nil)))
          ((my--statistic-percent-p) (progn
                                      (end-of-line)
                                      (kill-sexp -1)))
          (t (progn
               (end-of-line)
               (just-one-space)
               (insert "[/]")
               (org-update-statistics-cookies nil))))))
;; http://oremacs.com/2015/01/26/occur-dwim/

(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1))

(bind-keys
 ("C-M-=" . align-to-equals)
 :map my-custom-key-map
 ("t" . my-insert-current-time)
 ("c" . my-paste-to-new-buffer)
 ("o" . my-org-scratch-buffer)
 ("s" . my-toggle-statistic-cookie-type))

(use-package exec-path-from-shell
  :straight t
  :if system-type-darwin
  :init
  (progn
    (exec-path-from-shell-initialize)))





(use-package god-mode
  :straight t
  :bind (("C-c u g" . god-local-mode)
         :map god-local-mode-map
         ("i" . god-local-mode)
         ("." . repeat))
  :init (when (memq window-system '(mac ns))
          (global-set-key (kbd "<escape>") 'god-local-mode))
  :config
  (add-hook 'god-mode-enabled-hook (lambda () (hl-line-mode 1)))
  (add-hook 'god-mode-disabled-hook (lambda () (hl-line-mode -1))))

(use-package hippie-exp
  :bind ("C-\\" . hippie-expand))




(use-package hl-line
  :bind (:map my-toggle-prefix-map ("h" . hl-line-mode)))

(use-package simple
  :bind (:map my-toggle-prefix-map ("v" . visual-line-mode)))

(when (version<= "26.1" emacs-version)
  (setq display-line-numbers-type 'visual)
  (bind-key "l" 'my-line-numbers-toggle 'my-toggle-prefix-map))

(setq my-line-numbers-toggle-types '(visual t))

(defun my--line-numbers-toggle ()
  (setq my-line-numbers-toggle-types (-rotate 1 my-line-numbers-toggle-types))
  (let ((display-line-numbers-type (car my-line-numbers-toggle-types)))
    (display-line-numbers-mode)))


(defun my-line-numbers-toggle ()
  (interactive)
  (if (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1)
    (progn
      (my--line-numbers-toggle)
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "l")
           (lambda () (interactive)
             (my--line-numbers-toggle)))
         map)
       t))))


;; NOTE: `lispy' will end causing `counsel' to load




(use-package minions
  :straight t
  :config (minions-mode 1))


(use-package multiple-cursors
  :straight t
  :bind
  (("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)
   ("C-S-c C-S-c" . mc/edit-lines)))


(use-package olivetti
  :straight t
  :config (setq-default olivetti-body-width 0.7)
  :bind (:map my-toggle-prefix-map
              ("o" . olivetti-mode)))


(straight-use-package 'dockerfile-mode)


(straight-use-package 'yaml-mode)
(straight-use-package 'php-mode)
(straight-use-package 'know-your-http-well)
(straight-use-package 'fish-mode)
(straight-use-package 'markdown-mode)





(straight-use-package 'nim-mode)


(use-package projectile
  :straight t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-use-git-grep t
        projectile-completion-system 'default
        projectile-switch-project-action #'projectile-commander)
  (projectile-mode))

(use-package hl-todo
  :straight t
  :init (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package highlight-numbers
  :straight t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  (add-hook 'yaml-mode-hook 'highlight-numbers-mode))

(use-package which-key
  :straight t
  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom)
  (which-key-mode))



(find-file user-init-file)


(bind-key "§" 'recompile)

(progn
  (straight-use-package 'marginalia)
  (marginalia-mode t))
(progn
  (straight-use-package
   '(consult :host github :repo "minad/consult"
             :branch "main")))

;; NOTE: `today.el' keep track of buffers, files, "events", layouts for today?




;; (find-file-other-window (expand-file-name "commonplace.org" user-emacs-directory))

(straight-use-package 'lice)


;; NOTE: pacakge dev
;; https://github.com/alphapapa/emacs-package-dev-handbook#template
