;;; init-files.el --- settings and modes for files

;;; Commentary:
;;

(use-package recentf
  :config
  (setq recentf-exclude '("deft/" ".gpg")
        recentf-max-saved-items 500)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  :init
  (recentf-mode t))

(use-package super-save
  :straight t
  :config
  (setq super-save-auto-save-when-idle nil
        super-save-exclude '(".gpg")
        auto-save-list-file-prefix nil
        auto-save-default nil
        make-backup-files nil)
  (add-to-list 'super-save-triggers 'ace-window)
  :init
  (super-save-mode +1))


(provide 'init-files)

;;; init-files.el ends here
