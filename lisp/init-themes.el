;;; init-themes.el --- themes

;;; Commentary:
;;

;;; Code:

(straight-use-package 'nord-theme)
(straight-use-package 'solarized-theme)
(straight-use-package 'doom-themes)
(straight-use-package 'modus-themes)

(straight-use-package
 '(challenger-deep-theme :host github :repo "challenger-deep-theme/emacs" :local-repo "challenger-deep-theme"))

(straight-use-package
 '(dracula-theme :host github :repo "dracula/emacs" :local-repo "dracula-theme"))

(load-theme 'modus-operandi t)

(provide 'init-themes)

;;; init-themes.el ends here
