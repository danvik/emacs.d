;;; init-keys.el --- key bindings etc

;;; Commentary:
;;

(define-prefix-command 'my-custom-key-map)
(global-set-key (kbd "C-c u") my-custom-key-map)

(define-prefix-command 'my-toggle-prefix-map)
(global-set-key (kbd "C-c t") my-toggle-prefix-map)

(setq mac-option-modifier 'none
      mac-command-modifier 'meta)


(provide 'init-keys)

;;; init-keys.el ends here
