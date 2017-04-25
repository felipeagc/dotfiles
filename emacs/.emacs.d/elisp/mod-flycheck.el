;;
;; Flycheck
;;
(use-package flycheck
  :init
  (setq flycheck-highlighting-mode 'symbols)
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  :config
  (global-flycheck-mode)
  (use-package flycheck-pos-tip
    :config
    (flycheck-pos-tip-mode)))

