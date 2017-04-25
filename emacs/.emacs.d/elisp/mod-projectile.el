;;
;; Projectile
;;
(use-package projectile)

(use-package helm-projectile
  :after projectile
  :config
  (evil-leader/set-key
    "pp" 'helm-projectile-switch-project
    "pf" 'helm-projectile-find-file))
