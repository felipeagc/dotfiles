;;
;; Yasnippet
;;
(use-package yasnippet
  :init
  (setq yas-snippet-dirs
        '("~/.emacs.d/yasnippet-snippets"
          "~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1))
