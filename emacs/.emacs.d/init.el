;; TODO:
;; Projectile


;;; Code:

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


;;
;; General
;;

(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq-default indent-tabs-mode nil)

(setq show-paren-style 'parenthesis)

(when window-system
  (menu-bar-mode 0)                              ; Disable the menu bar
  (blink-cursor-mode 0)                          ; Disable the cursor blinking
  (scroll-bar-mode 0)                            ; Disable the scroll bar
  (tool-bar-mode 0)                              ; Disable the tool bar
  (tooltip-mode 0))                              ; Disable the tooltips

(fringe-mode 0)


;; Stop emacs from making a mess
(setq auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "auto-save/") t))) 
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(use-package nlinum
  :config
  (global-nlinum-mode 1)
  (defun nlinum-mode-margin-hook ()
    (when nlinum-mode
      (setq-local nlinum-format "%d ")))
  (add-hook 'nlinum-mode-hook #'nlinum-mode-margin-hook))

(use-package eyebrowse
  :config
  (eyebrowse-mode t))

(use-package evil-nerd-commenter)

(use-package shackle
  :init
  (setq helm-display-function 'pop-to-buffer) ; make helm play nice
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4)))
  (setq shackle-default-rule '(:same t)))


;;
;; Font
;;

(set-frame-font "Hack 11")


;;
;; Theme
;;

(use-package doom-themes
  :after nlinum
  :config
  (require 'doom-nlinum)
  (setq doom-enable-bold nil    ; if nil, bolding are universally disabled
    doom-enable-italic nil  ; if nil, italics are universally disabled
    ;; doom-one specific settings
    doom-one-brighter-modeline nil
    doom-one-brighter-comments nil)

  (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)

  (load-theme 'doom-one t))


;;
;; Evil
;;

(use-package evil
  :init
  (setq evil-shift-width 2)
  :config
  (fset 'evil-visual-update-x-selection 'ignore)
  (evil-mode 1)

  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
      (interactive)
      (if (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  (global-set-key [escape] 'evil-exit-emacs-state))

(use-package evil-leader
  :after evil
  :init
  (defun f-edit-config()
      (interactive)
      (find-file "~/.emacs.d/init.el"))

    (defun f-reload-config()
      (interactive)
      (load-file "~/.emacs.d/init.el")
      (evil-leader-mode))
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
  "1" 'eyebrowse-switch-to-window-config-1
  "2" 'eyebrowse-switch-to-window-config-2
  "3" 'eyebrowse-switch-to-window-config-3
  "4" 'eyebrowse-switch-to-window-config-4
  "5" 'eyebrowse-switch-to-window-config-5
  "6" 'eyebrowse-switch-to-window-config-6
  "7" 'eyebrowse-switch-to-window-config-7
  "8" 'eyebrowse-switch-to-window-config-8
  "9" 'eyebrowse-switch-to-window-config-9
  "zz" 'text-scale-adjust
  "zi" 'text-scale-increase
  "zo" 'text-scale-decrease
  "ff" 'helm-find-files
  "fed" 'f-edit-config
  "fer" 'f-reload-config
  "bb" 'helm-buffers-list
  "bd" 'kill-this-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "w/" 'split-window-right
  "w-" 'split-window-below
  "wd" 'delete-window
  "en" 'flycheck-next-error
  "ep" 'flycheck-previous-error
  "cl" 'evilnc-comment-or-uncomment-lines))

(use-package evil-org
  :after evil-leader)


;;
;; Which-key
;;

(use-package which-key
  :init
  (setq which-key-idle-delay 0.3)
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    "<SPC> b" "Buffer"
    "<SPC> z" "Zoom"
    "<SPC> w" "Window"
    "<SPC> e" "Error"
    "<SPC> c" "Comment"
    "<SPC> m" "Major Mode"
    "<SPC> g" "Git"
    "<SPC> f" "Files"
    "<SPC> 1" "Workspace 1"
    "<SPC> 2" "Workspace 2"
    "<SPC> 3" "Workspace 3"
    "<SPC> 4" "Workspace 4"
    "<SPC> 5" "Workspace 5"
    "<SPC> 6" "Workspace 6"
    "<SPC> 7" "Workspace 7"
    "<SPC> 8" "Workspace 8"
    "<SPC> 9" "Workspace 9"))


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
;; Electric pairs
;;

(electric-pair-mode)


;;
;; Company
;;

(use-package company
  :init
  (setq company-tooltip-align-annotations t)
  :config
  (global-company-mode)

  (use-package racer
    :after rust-mode
    :config
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode))
  (use-package company-anaconda
    :config
    (add-to-list 'company-backends 'company-anaconda))
  (use-package company-lua
    :config
    (add-to-list 'company-backends 'company-lua)))


;;
;; Flycheck
;;

(use-package flycheck
  :config
  (global-flycheck-mode)
  (use-package flycheck-rust
    :after rust-mode
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package flycheck-pos-tip
  :config
  (flycheck-pos-tip-mode))


;;
;; Rust
;;

(use-package rust-mode
  :after evil-leader
  :config
  (evil-leader/set-key-for-mode 'rust-mode
      "mb" 'cargo-process-build
      "mr" 'cargo-process-run
      "mf" 'rust-format-buffer)

  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))

(use-package cargo
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))


;;
;; Python
;;

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))


;;
;; Lua
;;

(use-package lua-mode
  :after evil-leader
  :config
  (evil-leader/set-key-for-mode 'lua-mode
      "mr" (lambda ()
    (interactive)
    (let ((app-root (locate-dominating-file (buffer-file-name) "main.lua")))
    (shell-command (format "love %s &" app-root))))))


;;
;; Javascript
;;

(use-package js2-mode
  :init
  (setq js2-highlight-level 3)
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))


;;
;; C/C++
;;

(use-package clang-format
  :after evil-leader
  :config
  (evil-leader/set-key-for-mode 'c++-mode
    "mf" 'clang-format-buffer
    "ms" 'ff-find-other-file))


;;
;; Haskell
;;

(use-package haskell-mode)


;;
;; Elisp
;;

(use-package elisp-format)


;;
;; Typescript
;;

(use-package tide
  :after company
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup))

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode))


;;
;; Web mode
;;

(use-package web-mode)


;;
;; Pug
;;

(use-package pug-mode)


;;
;; Sass
;;

(use-package scss-mode)


;;
;; Magit
;;

(use-package magit
  :after evil-leader
  :config
  (use-package evil-magit)
  (evil-leader/set-key
    "gg" 'magit-status
    "gs" 'magit-stage
    "gu" 'magit-unstage
    "gr" 'magit-unstage-all
    "gp" 'magit-push
    "gc" 'magit-commit))


;;
;; Projectile
;;

(use-package projectile)


;;
;; Modeline
;;

(use-package powerline
  :config
  (load-file "~/.emacs.d/modeline.el"))


;;
;; Indentation
;;

(add-hook 'rust-mode-hook
  (function (lambda ()
    (setq tab-width 4)
    (setq evil-shift-width 4))))

(add-hook 'python-mode-hook
  (function (lambda ()
    (setq tab-width 4)
    (setq evil-shift-width 4))))

(add-hook 'emacs-lisp-mode-hook
  (function (lambda ()
    (setq tab-width 2)
    (setq evil-shift-width 2))))

(add-hook 'js2-mode-hook
  (function (lambda ()
    (setq js2-basic-offset 2)
    (setq js-indent-level 2)
    (setq evil-shift-width 2))))

(add-hook 'js-mode-hook
  (function (lambda ()
    (setq js2-basic-offset 2)
    (setq js-indent-level 2)
    (setq evil-shift-width 2))))

(add-hook 'c++-mode
  (function (lambda ()
    (setq tab-width 2)
    (setq c-basic-offset 2)
    (setq evil-shift-width 2))))


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-org-mode scss-mode pug-mode web-mode tide helm-themes powerline projectile evil-magit magit elisp-format haskell-mode clang-format js2-mode cargo flycheck-pos-tip flycheck-rust flycheck company-lua company-anaconda racer company evil-smartparens smartparens helm which-key evil-leader evil doom-themes evil-nerd-commenter eyebrowse nlinum use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
