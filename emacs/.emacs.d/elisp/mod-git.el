;;
;; Git
;;

(use-package gitconfig-mode
  :mode ("/\\.?git/?config$" "/\\.gitmodules$")
  :init (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package gitignore-mode
  :mode ("/\\.gitignore$"
         "/\\.git/info/exclude$"
         "/git/ignore$"))

(use-package git-gutter
  :commands (git-gutter-mode)
  :init
  (add-hook! (text-mode prog-mode conf-mode) 'git-gutter-mode)
  :config

  (use-package git-gutter-fringe
    :config
    (def-popup! "^\\*git-gutter.+\\*$" :align below :size 15 :noselect t :regexp t)

    ;; NOTE If you want the git gutter to be on the outside of the margins (rather
    ;; than inside), `fringes-outside-margins' should be non-nil.

    ;; colored fringe "bars"
    (define-fringe-bitmap 'git-gutter-fr:added
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:modified
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:deleted
      [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
      nil nil 'center)

    ;; Refreshing git-gutter
    (advice-add 'evil-force-normal-state :after 'git-gutter)
    (add-hook 'focus-in-hook 'git-gutter:update-all-windows)))


(use-package magit
  :after evil-leader
  :commands (magit-status)
  :config
  (use-package evil-magit
    :after evil)

  (evil-leader/set-key
    "gg" 'magit-status
    "gs" 'magit-stage
    "gu" 'magit-unstage
    "gr" 'magit-unstage-all
    "gp" 'magit-push
    "gc" 'magit-commit)

  (def-popup! "^\\*magit.+" :align below :regexp t)
  ;; Prevent magit + evil-snipe conflicts
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))
