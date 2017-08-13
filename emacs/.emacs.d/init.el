(package-initialize)

(org-babel-load-file "~/.emacs.d/config.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0ee3fc6d2e0fc8715ff59aed2432510d98f7e76fe81d183a0eb96789f4d897ca" "2f0a552a9d14fe8ddaaacdb7b82a0eee1ea1f7f5d0850789915e5b04a1b9669f" "a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "5310b88333fc64c0cb34a27f42fa55ce371438a55f02ac7a4b93519d148bd03d" "8d737627879eff1bbc7e3ef1e9adc657207d9bf74f9abb6e0e53a6541c5f2e88" "0f0022c8091326c9894b707df2ae58dd51527b0cf7abcb0a310fb1e7bda78cd2" "4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "227e2c160b0df776257e1411de60a9a181f890cfdf9c1f45535fc83c9b34406b" "34c6da8c18dcbe10d34e3cf0ceab80ed016552cb40cee1b906a42fd53342aba3" default)))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(package-selected-packages
   (quote
    (markdown-mode neotree evil-org dumb-jump evil-surround writeroom-mode intero evil-magit powerline helm-projectile projectile git-gutter-fringe magit git-gutter gitignore-mode gitconfig-mode nim-mode go-eldoc glsl-mode scss-mode pug-mode web-mode tide elisp-format company-ghc haskell-mode clang-format irony-eldoc flycheck-irony company-irony irony js2-mode elpy cargo flycheck-pos-tip golint flycheck-rust flycheck company-lua racer company-go company helm-themes helm which-key evil-nerd-commenter evil-leader doom-themes exec-path-from-shell shackle evil-vimish-fold eyebrowse nlinum use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-delete-face ((t (:inherit (quote diff-removed)))))
 '(evil-goggles-paste-face ((t (:inherit (quote diff-added)))))
 '(evil-goggles-yank-face ((t (:inherit (quote diff-changed))))))
