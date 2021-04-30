;;; init-emacs.el --- Basic Emacs settings

;;; Commentary:
;;

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

(provide 'init-emacs)

;;; init-emacs.el ends here
