(setq package-enable-at-startup nil ; don't auto-initialize!
      package--init-file-ensured t  ; don't add that `custom-set-variables' block to my init.el!
      gc-cons-threshold 100000000)
