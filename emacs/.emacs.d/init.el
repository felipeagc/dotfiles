(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Better yes/no questions
;; This makes emacs accept only y/n as answers.
(fset 'yes-or-no-p 'y-or-n-p)

;; Visuals
(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-10"))
(setq font-lock-maximum-decoration 2) ;; Minimize the syntax highlighting a bit

;; Scroll margin
(setq scroll-margin 10)

;; Disable UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

;; Disable scroll bars
(defun felipe/disable-scroll-bars (frame)
  (modify-frame-parameters frame
			   '((vertical-scroll-bars . nil)
			     (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'felipe/disable-scroll-bars)

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              c-default-style "bsd")

;; Auto closing pairs
(electric-pair-mode)

;; Use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; No littering
(use-package no-littering)

;; Evil
(use-package evil
  :init
  :config
  (evil-mode 1)

  ;; :W to save buffer
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "W[rite]" 'evil-write))

  ;; Better window switching keys
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

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
  (define-key evil-visual-state-map (kbd "<") 'felipe/evil-shift-left-visual))

;; Evil surrround
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Evil commentary
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; Theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

;; Ivy
(use-package ivy
  :config
  (ivy-mode 1))

;; Counsel
(use-package counsel
  :after ivy
  :init
  (setenv "FZF_DEFAULT_COMMAND"
          "rg --files --follow")
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)")))

(use-package general
  :config
  (general-create-definer felipe/leader-def
    :prefix "SPC"))

;; Leader bindings
(felipe/leader-def
  :keymaps 'normal
  "a" 'projectile-find-other-file
  "A" 'projectile-find-other-file-other-window

  "w/" 'split-window-right
  "w-" 'split-window-below
  "wd" 'delete-window
  "wb" 'balance-windows
  "wb" 'balance-windows

  "ff" 'counsel-find-file
  "fg" 'counsel-rg
  "fed" (lambda ()
          (interactive)
          (find-file "~/.emacs.d/init.el"))

  "bb" 'counsel-switch-buffer
  "bd" 'kill-this-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer

  "pp" 'projectile-switch-project
  "pf" 'counsel-fzf
  "pa" 'projectile-add-known-project

  "gs" 'magit-status

  "mb" 'projectile-compile-project)

;; Company
(use-package company
  :config
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (global-company-mode))

;; Magit
(use-package magit
  :config
  (use-package evil-magit
    :after evil))

(use-package ssh-agency)

;; Git gutter fringe
(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode +1)

  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX...."))

;; Projectile
(use-package projectile
  :config
  (setq compilation-read-command nil ; Stops projectile from asking for compile command
        projectile-completion-system 'ivy)
  (projectile-global-mode))

;; Highlight
(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; C/C++
(use-package cmake-mode)
(use-package clang-format)

(use-package modern-cpp-font-lock
  :config
  (add-hook 'c-mode-hook #'modern-c++-font-lock-mode)
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(felipe/leader-def 'normal c-mode-map
  "mf" 'clang-format-buffer)
(felipe/leader-def 'normal c++-mode-map
  "mf" 'clang-format-buffer)

;; GLSL
(use-package glsl-mode
  :defer t
  :config
  (autoload 'glsl-mode "glsl-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.glslf\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.glslv\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode)))
