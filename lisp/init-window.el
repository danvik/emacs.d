;;; init-window.el --- settings for window handling

;;; Commentary:
;;

(use-package ace-window
  :straight t
  :bind ("C-x o" . ace-window))

(use-package rotate
  :straight t
  :bind ("C-c r" . rotate-layout))

(use-package winner
  :config (winner-mode t))

(setq recenter-positions '(top middle bottom))


(provide 'init-window)

;;; init-window.el ends here
