;;; early-init.el --- Early init file

;;; Commentary:
;;

(setq package-enable-at-startup nil
      package-quickstart nil
      frame-inhibit-implied-resize t
      inhibit-startup-screen t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)

;;; early-init.el ends here
