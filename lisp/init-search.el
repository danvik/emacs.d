;;; init-search.el --- text search

;;; Commentary:
;;

(progn
  (straight-use-package
   '(ctrlf :host github :repo "raxod502/ctrlf"))

  (ctrlf-mode +1))

(use-package deadgrep
  :straight t
  :bind ("C-c g" . deadgrep))

(provide 'init-search)

;;; init-search.el ends here
