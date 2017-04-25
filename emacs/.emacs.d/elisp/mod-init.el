;;
;; Package management
;;
(when (>= emacs-major-version 24)
  (setq-default
   load-prefer-newer t
   package-enable-at-startup nil)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(setq use-package-always-ensure t)
