;;; early-init.el --- Early init file

;;; Commentary:
;;

(setq package-enable-at-startup nil
      package-quickstart nil
      frame-inhibit-implied-resize t
      inhibit-startup-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(provide 'early-init)

;;; early-init.el ends here
