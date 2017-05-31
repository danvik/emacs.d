;; -*- lexical-binding: t; -*-
(defvar my-no-repeat-mode-map (make-sparse-keymap))
(defvar my-no-repeat-current-char nil)
(defvar my-no-repeat-current-count nil)
(defvar my-no-repeat-show-hint 't)

(defun my-current-key-binding (key)
  (prog2 (my-no-repeat-mode -1) (key-binding (kbd key)) (my-no-repeat-mode +1)))

(defun my-lookup-binding (key)
  (let ((key-as-emacs-key (condition-case nil
                              (kbd key)
                            (error nil))))
    (when key-as-emacs-key
      (my-current-key-binding key))))

(defun my-last-char-binding (binding)
  (substring binding -1))

(defun my-get-char (key char)
  (let ((actual-char (or char (my-last-char-binding key))))
    actual-char))

(defun my-no-repeat-kbd (key &rest args)
  (let ((char (plist-get args :char))
        (block-key (plist-get args :block-key))
        (block-after (plist-get args :block-after))
        (after-commands (plist-get args :after))
        (repeat-key (plist-get args :repeat-key)))
    (let ((bind-to-char (or repeat-key (my-get-char key char))))
      (define-key
        my-no-repeat-mode-map
        (kbd key)
        (defalias (make-symbol "my-no-repeat-command")
          (lambda ()
            (interactive)
            (let ((key-target (my-lookup-binding key)))
              (call-interactively key-target)
              (when my-no-repeat-show-hint
                (message (format "Press %s to repeat command %s" bind-to-char key-target)))
              (make-local-variable 'my-no-repeat-current-count)
              (setq my-no-repeat-current-count 0)
              (set-transient-map
               (let ((map (make-sparse-keymap)))
                 (define-key map (kbd bind-to-char)
                   (lambda () (interactive)
                     (call-interactively key-target)
                     (when my-no-repeat-show-hint
                       (message (format "Press %s to repeat command %s" bind-to-char key-target)))
                     (setq my-no-repeat-current-count (1+ my-no-repeat-current-count))
                     (when (and block-after (= my-no-repeat-current-count block-after))
                       (define-key map (kbd bind-to-char) 'keyboard-quit))
                     (let ((after-command (assoc my-no-repeat-current-count after-commands)))
                       (when after-command
                         (call-interactively (cdr after-command))))))
                 (when block-key
                   (define-key map (kbd key) 'keyboard-quit))
                 map)
               t))))))))

(defun my-no-repeat-lighter-fun ()
  (if my-no-repeat-current-char
      (format " [no-repeat: %s]" my-no-repeat-current-char)
    " [no-repeat]"))

(define-minor-mode my-no-repeat-mode
  "No repeat mode."
  :keymap my-no-repeat-mode-map
  :lighter (:eval (my-no-repeat-lighter-fun)))

(defun my-no-repeat-maybe-activate ()
  (unless (minibufferp)
    (my-no-repeat-mode 1)))

(define-globalized-minor-mode my-no-repeat-global-mode my-no-repeat-mode my-no-repeat-maybe-activate)

(provide 'my-no-repeat-mode)
