;;; init-gui.el --- gui settings

;;; Commentary:
;;

(when (and (display-graphic-p) (member "Iosevka" (font-family-list)))
  (set-frame-font "Iosevka-18")
  (add-to-list 'default-frame-alist '(font . "Iosevka-18")))

(provide 'init-gui)

;;; init-gui.el ends here
