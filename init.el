;;; init.el --- Emacs configuration

;;; Commentary:
;;

;;; Code:

;;; init `straight-use-package'

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

(defvar local-settings-file "~/.priv/elisp/local.el"
  "File used for private and local settings.")

(when (file-exists-p local-settings-file)
  (load local-settings-file))

(straight-use-package 'no-littering)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p local-settings-file)
  (load custom-file))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; early key settings

(define-prefix-command 'my-toggle-prefix-map)
(global-set-key (kbd "C-c t") my-toggle-prefix-map)

(when system-type-darwin
  (require 'init-mac))

(require 'init-ui)
(require 'init-emacs)
(require 'init-buffer)
(require 'init-dired)
(require 'init-editing)
(require 'init-files)
(require 'init-prog)
(require 'init-project)
(require 'init-themes)
(require 'init-vc)
(require 'init-window)
(require 'init-completing)
(require 'init-org)
(require 'init-search)
(require 'init-utils)
(require 'init-defuns)

(provide 'init)

;; Whether to colorize a bookmarked line.
(setq bookmark-fontify nil)

(defun my--buffer-gpg-p ()
  (and (buffer-file-name)
       (string= "gpg" (file-name-extension (buffer-file-name)))))

(use-package dogears
  :straight t
  :config
  (add-to-list 'dogears-hooks 'after-save-hook)
  (add-to-list 'dogears-ignore-places-functions #'my--buffer-gpg-p)
  :init
  (dogears-mode))
