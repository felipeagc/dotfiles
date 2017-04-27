(package-initialize)

(org-babel-load-file "~/.emacs.d/config.org")

(custom-set-variables
 '(custom-safe-themes
   (quote
    ("227e2c160b0df776257e1411de60a9a181f890cfdf9c1f45535fc83c9b34406b" "34c6da8c18dcbe10d34e3cf0ceab80ed016552cb40cee1b906a42fd53342aba3" default)))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(package-selected-packages
   (quote
    (intero evil-magit powerline helm-projectile projectile git-gutter-fringe magit git-gutter gitignore-mode gitconfig-mode nim-mode go-eldoc glsl-mode scss-mode pug-mode web-mode tide elisp-format company-ghc haskell-mode clang-format irony-eldoc flycheck-irony company-irony irony js2-mode elpy cargo flycheck-pos-tip golint flycheck-rust flycheck company-lua racer company-go company helm-themes helm which-key evil-nerd-commenter evil-leader doom-themes exec-path-from-shell shackle evil-vimish-fold eyebrowse nlinum use-package))))
(custom-set-faces)
