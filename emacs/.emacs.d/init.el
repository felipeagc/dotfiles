(package-initialize)

(add-to-list 'load-path "~/.emacs.d/elisp")

(load-library "mod-init")
(load-library "mod-deps")
(load-library "mod-misc")
(load-library "mod-evil")
(load-library "mod-ui")
(load-library "mod-modeline")
(load-library "mod-projectile")
(load-library "mod-snippets")
(load-library "mod-flycheck")
(load-library "mod-company")
(load-library "mod-languages")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("227e2c160b0df776257e1411de60a9a181f890cfdf9c1f45535fc83c9b34406b" "34c6da8c18dcbe10d34e3cf0ceab80ed016552cb40cee1b906a42fd53342aba3" default)))
 '(package-selected-packages
   (quote
    (intero evil-magit powerline helm-projectile projectile git-gutter-fringe magit git-gutter gitignore-mode gitconfig-mode nim-mode go-eldoc glsl-mode scss-mode pug-mode web-mode tide elisp-format company-ghc haskell-mode clang-format irony-eldoc flycheck-irony company-irony irony js2-mode elpy cargo flycheck-pos-tip golint flycheck-rust flycheck company-lua racer company-go company helm-themes helm which-key evil-nerd-commenter evil-leader doom-themes exec-path-from-shell shackle evil-vimish-fold eyebrowse nlinum use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
