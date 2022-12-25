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
      straight-use-package-by-default t
      native-comp-async-report-warnings-errors nil
      compilation-read-command nil ;; Don't ask for compile command
      eldoc-echo-area-use-multiline-p nil)

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

(use-package string-inflection
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "grc") 'string-inflection-lower-camelcase)
  (define-key evil-normal-state-map (kbd "grs") 'string-inflection-underscore)
  (define-key evil-normal-state-map (kbd "grp") 'string-inflection-camelcase))

;; Auto-close compilation buffer
(defun felipe/bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                      buffer)))
(add-hook 'compilation-finish-functions 'felipe/bury-compile-buffer-if-successful)

;; Set compile command
(defun felipe/set-compile-command (proj-file command-str)
  (let ((proj-dir (locate-dominating-file default-directory proj-file)))
    (when proj-dir
        (set (make-local-variable 'compile-command) (format command-str proj-dir)))))

;; Remove vc-handled-backends when file is remote
(defun felipe/vc-off-if-remote ()
  (if (file-remote-p (buffer-file-name))
      (setq-local vc-handled-backends nil)))
(add-hook 'find-file-hook 'felipe/vc-off-if-remote)

;; Zooming
(use-package default-text-scale
  :defer t
  :bind (("C-=" . default-text-scale-increase)
         ("C--" . default-text-scale-decrease)
         ("C-0" . default-text-scale-reset)))

(use-package magit
  :defer t
  :config
  (evil-define-key 'normal 'magit-mode-map (kbd "C-j") 'evil-window-next)
  (evil-define-key 'normal 'magit-mode-map (kbd "C-k") 'evil-window-prev))

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

  (define-key evil-normal-state-map (kbd "<f7>") 'compile)

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

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree))

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

   "fg" 'consult-ripgrep
   "fed" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))

   "bd" 'kill-this-buffer
   "bn" 'next-buffer
   "bp" 'previous-buffer

   "gs" 'magit-status

   "mf" 'eglot-format-buffer
   "mr" 'eglot-rename
   "mi" 'eglot-code-actions))

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

;; (use-package modus-themes
;;   :config
;;   (load-theme 'modus-vivendi t))

(use-package doom-themes
  :config
  (load-theme 'doom-opera t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package eglot
  :defer t
  :hook ((go-mode . eglot-ensure)
	 (c-mode . eglot-ensure)
	 (c++-mode . eglot-ensure)
	 (typescript-mode . eglot-ensure)
	 (web-mode . eglot-ensure)))

;; C/C++ config
(defun felipe/c-c++-hook ()
  (felipe/set-compile-command "CMakeLists.txt" "cd %1$s/build && cmake --build %1$s/build")
  (felipe/set-compile-command "meson.build" "ninja -C %s/build")
  (felipe/set-compile-command "Makefile" "make -C %s")
  (felipe/set-compile-command "makefile" "make -C %s"))

(add-hook 'c++-mode-hook 'felipe/c-c++-hook)
(add-hook 'c-mode-hook 'felipe/c-c++-hook)

;; Go config
(use-package go-mode
  :defer t
  :config
  (add-hook 'go-mode-hook (lambda ()
			    (setq tab-width 4)
			    (felipe/set-compile-command "makefile" "make -C %s")
			    (felipe/set-compile-command "Makefile" "make -C %s"))))

(use-package graphql-mode
  :defer t)

(use-package web-mode
  :defer t
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" .  web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.svelte\\'" . web-mode)))
