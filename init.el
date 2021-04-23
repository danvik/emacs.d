
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

(defconst system-type-darwin (eq system-type 'darwin)
  "Mac or not.")

(defvar local-settings-file "~/.priv/elisp/local.el"
  "File used for private and local settings.")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(load local-settings-file nil)
(load custom-file t)

(require 'init-ui)
(require 'init-gui)
(require 'init-emacs)
(require 'init-buffer)
(require 'init-dired)
(require 'init-editing)
(require 'init-files)
(require 'init-keys)
(require 'init-prog)
(require 'init-themes)
(require 'init-vc)
(require 'init-window)
(require 'init-completing)
(require 'init-org)
(require 'init-search)
(require 'init-utils)
(require 'init-defuns)



(use-package avy
  :straight t
  :bind
  (("M-g w" . avy-goto-word-1)
   ("M-g l" . avy-goto-line)
   ("M-g f" . avy-goto-char-in-line)
   ("M-g M-l" . avy-copy-line)
   ("M-g g" . avy-goto-line))
  :config (setq avy-all-windows nil))

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

(use-package which-key
  :straight t
  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom)
  (which-key-mode))

(use-package crux
  :straight t)
