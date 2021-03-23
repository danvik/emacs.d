;;; init-themes.el --- themes

;;; Commentary:
;;

;;; Code:

(mapc 'straight-use-package
      '(nord-theme
        solarized-theme
        doom-themes
        modus-operandi-theme
        modus-vivendi-theme
        apropospriate-theme))

(use-package challenger-deep-theme
  :straight (:host github :repo "challenger-deep-theme/emacs" :local-repo "challenger-deep-theme"))

(use-package dracula-theme
  :straight (:host github :repo "dracula/emacs" :local-repo "dracula-theme"))



(load-theme 'modus-operandi t)



(provide 'init-themes)

;;; init-themes.el ends here
