;;; init-completing.el --- completion and selection packages

;;; Commentary:
;;

(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :defer t
  :init
  (selectrum-mode +1))

(use-package prescient
  :straight t
  :config
  (prescient-persist-mode +1)
  (setq prescient-history-length 1000))

(use-package selectrum-prescient
  :straight (:host github :repo "raxod502/prescient.el"
                   :files ("selectrum-prescient.el"))
  :demand t
  :after selectrum
  :config
  (selectrum-prescient-mode +1))

(setq
 ;; https://github.com/raxod502/selectrum/issues/100
 enable-recursive-minibuffers t)

(provide 'init-completing)

;;; init-completing.el ends here
