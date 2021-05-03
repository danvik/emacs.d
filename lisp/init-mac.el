;;; init-mac.el --- Mac settings

;;; Commentary:
;;

(setq mac-option-modifier 'none
      mac-command-modifier 'meta)

(use-package exec-path-from-shell
  :straight t
  :config (exec-path-from-shell-initialize))

(provide 'init-mac)

;;; init-mac.el ends here
