;;; init-buffer.el --- Bufler

;;; Commentary:
;;

;;; Code:

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package bufler
  :straight (:host github :repo "alphapapa/bufler.el")
  :config (bind-keys :prefix-map my-bufler-prefix-map
           :prefix "C-c b"
           ("l" . bufler-list)
           ("b" . bufler-switch-buffer)
           ("c" . bufler-workspace-focus-buffer)
           ("f" . bufler-workspace-frame-set))
  :init (bufler-workspace-mode +1))
;; bufler-workspace-switch-buffer-sets-workspace

(provide 'init-buffer)

;;; init-buffer.el ends here
