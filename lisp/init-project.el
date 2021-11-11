;;; init-project.el --- Project things

;;; Commentary:
;;


(use-package project
  :straight t)

(use-package projectile
  :straight t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-use-git-grep t
        projectile-completion-system 'default
        projectile-switch-project-action #'projectile-commander)
  (projectile-mode))

(provide 'init-project)

;;; init-project.el ends here
