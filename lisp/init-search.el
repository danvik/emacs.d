;;; init-search.el --- text search

;;; Commentary:
;;

(use-package ctrlf
  :straight '(ctrlf :host github :repo "raxod502/ctrlf")
  :init
  (ctrlf-mode +1))

(use-package deadgrep
  :straight t
  :bind ("C-c g" . deadgrep))

(provide 'init-search)

;;; init-search.el ends here
