;; -*- lexical-binding: t; -*-

;; Setup straight {{{
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
;; }}}

;; Add ~/.emacs.d/elisp to load path {{{
(add-to-list 'load-path (concat user-emacs-directory "/elisp"))
;; }}}

;; Small tweaks {{{

;; GC tweaks
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))

;; Inhibit electric indent
(setq electric-indent-inhibit t)

;; Disable welcome screen
(setq inhibit-startup-screen t)

;; Better yes/no questions
;; This makes emacs accept only y/n as answers.
(fset 'yes-or-no-p 'y-or-n-p)

;; Pixelwise resizing
;; (setq frame-resize-pixelwise t)

;; Treat underscores as a part of a word
(add-hook 'after-change-major-mode-hook
        (lambda ()
            (modify-syntax-entry ?_ "w")))
  
;; Stop asking to follow symlinks
(setq vc-follow-symlinks t)

;; Visuals
(add-to-list 'default-frame-alist '(font . "Source Code Pro Medium-10.5"))
(setq font-lock-maximum-decoration 3) ;; Minimize the syntax highlighting a bit

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

;; Disable UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

;; Frame settings
(defun felipe/after-frame (frame)
  (modify-frame-parameters frame '((vertical-scroll-bars . nil)
                                   (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'felipe/after-frame)

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              c-default-style "bsd")

;; Auto closing pairs
(electric-pair-mode)

;; Highlight matching brace
(setq show-paren-delay 0.2)
(show-paren-mode 1)

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

;; Highlight current line in programming modes
;; (add-hook 'prog-mode-hook 'hl-line-mode)
;; }}}

;; Use-package {{{
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; }}}

;; Small packages {{{
(use-package no-littering)
(use-package rainbow-mode) ;; Display colors
;; }}}

;; Evil {{{
(use-package evil
  :init
  :config
  (evil-mode 1)

  ;; :W to save buffer
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "W[rite]" 'evil-write))

  ;; Better window switching keys
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-next)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-prev)

  (define-key evil-normal-state-map (kbd "C-]") 'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "gd") 'evil-search-word-forward)
  (define-key key-translation-map (kbd "ESC") (kbd "C-g"))

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

(use-package origami
  :ensure t
  :init
  (setq origami-show-fold-header t)
  :config
  (defun felipe/origami-toggle-node ()
    (interactive)
    (save-excursion                                  ;; leave point where it is
      (goto-char (point-at-eol))                     ;; then go to the end of line
      (origami-toggle-node (current-buffer) (point)) ;; and try to fold
      ))
  (define-key evil-normal-state-map (kbd "za") 'felipe/origami-toggle-node)

  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local origami-fold-style 'triple-braces)
              (origami-mode)
              (origami-close-all-nodes (current-buffer))))
  )

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode))
;; }}}

;; Theme {{{
(use-package autothemer
  :config
    (require 'darktooth-theme)
    (load-theme 'darktooth t)
    (darktooth-modeline-two))
;; }}}

;; Modeline format {{{
(column-number-mode)
(setq-default mode-line-format
      (list "-" 
            'mode-line-mule-info
            'mode-line-modified
            "  "
            'mode-line-buffer-identification
            'vc-mode
            "  ("
            'mode-name
            ")  "
            'mode-line-position))
;; }}}

;; Ivy {{{
(use-package ivy
  :config
  (ivy-mode 1))

(use-package ivy-xref
  :after ivy
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))
;; }}}

;; Counsel {{{
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
         "\\|\\(?:\\`.+?[#~]\\'\\)"))
  :config
  (use-package counsel-projectile
    :after projectile
    :config
    (counsel-projectile-mode))

  (use-package counsel-etags
    :init
    ;; Don't ask before rereading the TAGS files if they have changed
    (setq tags-revert-without-query t)
    ;; Don't warn when TAGS files are large
    (setq large-file-warning-threshold nil)
    (add-hook 'prog-mode-hook
              (lambda ()
                (add-hook 'after-save-hook
                          'counsel-etags-virtual-update-tags 'append 'local)))
    :config
    (setq counsel-etags-update-interval 60)
    (push "build" counsel-etags-ignore-directories))
  )
;; }}}

;; Keybindings {{{
(define-key evil-normal-state-map (kbd "C-p") 'counsel-projectile-find-file)

(use-package general
  :config
  (general-create-definer felipe/leader-def
    :prefix "SPC"))

(evil-define-key '(normal visual) prog-mode-map
 "[q" 'previous-error
 "]q" 'next-error)

(felipe/leader-def
  :keymaps 'normal
  "a" 'projectile-find-other-file
  "A" 'projectile-find-other-file-other-window

  "ir" 'ivy-resume

  "w/" 'split-window-right
  "w-" 'split-window-below
  "wd" 'delete-window
  "wb" 'balance-windows
  "wb" 'balance-windows

  "ff" 'counsel-find-file
  "fg" 'counsel-ag
  "fed" (lambda ()
          (interactive)
          (find-file "~/.emacs.d/init.el"))

  "bb" 'counsel-switch-buffer
  "bd" 'kill-this-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer

  "pp" 'counsel-projectile-switch-project
  "pf" 'counsel-projectile-find-file
  "pa" 'projectile-add-known-project
  "pg" 'counsel-projectile-rg

  "en" 'next-error
  "ep" 'previous-error

  "gs" 'magit-status

  "mb" 'projectile-compile-project)
;; }}}

;; Company {{{
(use-package company
  :config
  (define-key evil-insert-state-map (kbd "C-n") 'company-complete)
  (define-key evil-insert-state-map (kbd "C-p") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (define-key company-active-map (kbd "<ret>") 'company-complete-selection)
  (global-company-mode)
  (setq company-minimum-prefix-length 1
        company-idle-delay nil))

(use-package company-ctags
  :after company
  :config
  (company-ctags-auto-setup))
;; }}}

;; Magit {{{
(use-package magit
  :config
  (use-package evil-magit
    :after evil))

(use-package ssh-agency)
;; }}}

;; Projectile {{{
(use-package projectile
  :config
  (setq compilation-read-command nil ; Stops projectile from asking for compile command
        projectile-completion-system 'ivy)
  (projectile-global-mode)

  (projectile-register-project-type 'dlang-dub '("dub.sdl")
                                    :project-file "dub.sdl"
                                    :compile "dub build")
  )
;; }}}

;; Highlight todo {{{
(use-package hl-todo
  :config
  (global-hl-todo-mode))
;; }}}

;; C/C++ {{{
(use-package meson-mode)
(use-package cmake-mode)
(use-package clang-format)

(add-to-list 'auto-mode-alist '("\\.inl\\'" . c-mode))

(defun felipe/c-c++-hook ()
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    ;; remove clang backend
    (setq company-backends (delete 'company-clang company-backends))
    ))

(add-hook 'c++-mode-hook 'felipe/c-c++-hook)
(add-hook 'c-mode-hook 'felipe/c-c++-hook)

(felipe/leader-def 'normal c-mode-map
  "mf" 'clang-format-buffer)
(felipe/leader-def 'normal c++-mode-map
  "mf" 'clang-format-buffer)
;; }}}

;; D {{{
(use-package d-mode)
;; }}}

;; GLSL {{{
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
;; }}}

;; HLSL {{{
(use-package shader-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.hlsl$" . shader-mode)))
;; }}}

;; Other major modes {{{
(use-package org)
(use-package yaml-mode)
;; }}}
