
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

(load local-settings-file t)
(load custom-file t)

(require 'init-keys)
(require 'init-ui)
(require 'init-gui)
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
