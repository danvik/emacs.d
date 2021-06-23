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
  :after selectrum
  :config
  (selectrum-prescient-mode +1))

(use-package embark
  :straight t
  :bind ("C-c e" . embark-act))

(use-package marginalia
  :straight (:host github :repo "minad/marginalia" :branch "main")
  :config (setq marginalia-annotators '(marginalia-annotators-heavy nil marginalia-annotators-light))
  :init
  (marginalia-mode t))

(use-package consult
  :straight (:host github :repo "minad/consult" :branch "main"))

(provide 'init-completing)


;;; init-completing.el ends here
