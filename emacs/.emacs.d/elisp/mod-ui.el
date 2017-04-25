;;
;; Which-key
;;
(use-package which-key
  :init
  (setq which-key-idle-delay 0.3)
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    "<SPC> t" "Theme"
    "<SPC> tt" "Switch theme"
    "<SPC> b" "Buffer"
    "<SPC> bb" "Find buffer"
    "<SPC> bn" "Next buffer"
    "<SPC> bp" "Previous buffer"
    "<SPC> bd" "Close buffer"
    "<SPC> z" "Zoom"
    "<SPC> zi" "Zoom in"
    "<SPC> zo" "Zoom out"
    "<SPC> w" "Window"
    "<SPC> w/" "Split vertically"
    "<SPC> w-" "Split horizontally"
    "<SPC> e" "Error"
    "<SPC> en" "Next error"
    "<SPC> ep" "Previous error"
    "<SPC> c" "Comment"
    "<SPC> m" "Major Mode"
    "<SPC> g" "Git"
    "<SPC> f" "Files/folding"
    "<SPC> ff" "Find file"
    "<SPC> fb" "Toggle fold"
    "<SPC> p" "Projectile"
    "<SPC> pp" "Switch project"
    "<SPC> pf" "Find file"))

;;
;; Helm
;;
(use-package helm
  :init
  (setq helm-split-window-in-side-p t)
  :config
  (helm-mode 1)
  (define-key helm-find-files-map "\t" 'helm-execute-persistent-action))

(use-package helm-themes
  :after helm)

;;
;; Theme
;;
(use-package doom-themes
  :after nlinum
  :init
  (setq doom-enable-bold nil    ; if nil, bolding are universally disabled
    doom-enable-italic t  ; if nil, italics are universally disabled

    ;; doom-one specific settings
    doom-one-brighter-modeline t
    doom-one-brighter-comments nil)
  :config
  ;; brighter source buffers (that represent files)
  ;; (add-hook 'find-file-hook 'doom-buffer-mode-maybe)
  ;; if you use auto-revert-mode
  ;; (add-hook 'after-revert-hook 'doom-buffer-mode-maybe)
  ;; you can brighten other buffers (unconditionally) with:
  ;; (add-hook 'ediff-prepare-buffer-hook 'doom-buffer-mode)

  ;; brighter minibuffer when active
  ;; (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  (load-theme 'doom-one t)
  (require 'doom-nlinum))

;;
;; Font
;;
(set-frame-font "Hack 11")
