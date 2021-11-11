;;; init-emacs.el --- Basic Emacs settings

;;; Commentary:
;;

(setq gc-cons-threshold 100000000)
(setq ring-bell-function 'ignore)
(setq ffap-machine-p-known 'reject)
(setq confirm-kill-emacs #'yes-or-no-p)

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
