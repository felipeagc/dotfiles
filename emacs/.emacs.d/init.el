(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq inhibit-startup-message t
      visible-bell nil
      use-package-always-ensure t
      straight-use-package-by-default t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)

;; Fix scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-margin 6
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;; Font
(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-11"))

(setq show-paren-delay 0.0)
(show-paren-mode 1)

(use-package no-littering)

(defun my-vc-off-if-remote ()
  (if (file-remote-p (buffer-file-name))
      (setq-local vc-handled-backends nil)))
(add-hook 'find-file-hook 'my-vc-off-if-remote)

(use-package magit)

(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)

  ;; Overload shifts so that they don't lose the selection
  (defun felipe/evil-shift-left-visual ()
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun felipe/evil-shift-right-visual ()
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (define-key evil-visual-state-map (kbd ">") 'felipe/evil-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'felipe/evil-shift-left-visual)

  (define-key key-translation-map (kbd "ESC") (kbd "C-g"))

  (define-key evil-normal-state-map (kbd "C-b") 'switch-to-buffer)
  (define-key evil-normal-state-map (kbd "C-p") 'find-file-in-project)
  (define-key evil-normal-state-map (kbd "C-a") 'ff-find-other-file)

  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-next)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-prev)

  (define-key evil-insert-state-map (kbd "C-n") 'completion-at-point)
  (define-key evil-insert-state-map (kbd "C-p") 'completion-at-point))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init 'magit))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package general
  :config
  (general-create-definer felipe/leader-def :prefix "SPC")
  (felipe/leader-def
   :keymaps 'normal

   "w/" 'split-window-right
   "w-" 'split-window-below
   "wd" 'delete-window
   "wb" 'balance-windows

   "pp" 'project-switch-project
   "pg" 'consult-ripgrep

   "fed" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))

   "bd" 'kill-this-buffer
   "bn" 'next-buffer
   "bp" 'previous-buffer

   "gs" 'magit-status))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :init
  (global-corfu-mode)
  :config
  (general-def
    :keymaps 'completion-in-region-mode
    :definer 'minor-mode
    :states 'insert
    :predicate 'corfu-mode
    "C-n" 'corfu-next
    "C-p" 'corfu-previous
    "<escape>" 'corfu-quit
    "<return>" 'corfu-insert))

(use-package corfu-terminal
  :after corfu
  :init (corfu-terminal-mode))

(use-package marginalia
  :init (marginalia-mode))

(use-package vertico
  :init (vertico-mode))

(use-package find-file-in-project)

(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t))

(use-package eglot
  :defer t
  :hook ((go-mode . eglot-ensure)
	 (c-mode . eglot-ensure)
	 (c++-mode . eglot-ensure)))

(use-package go-mode
  :config
  (add-hook 'go-mode-hook (lambda () (setq tab-width 4))))
