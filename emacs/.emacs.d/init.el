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

;; Small tweaks {{{

;; GC tweaks
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))

;; Don't ask for compile command
(setq compilation-read-command nil)

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
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11"))
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
(setq show-paren-delay 0.0)
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
(use-package string-inflection
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "grc") 'string-inflection-lower-camelcase)
  (define-key evil-normal-state-map (kbd "grs") 'string-inflection-underscore)
  (define-key evil-normal-state-map (kbd "grp") 'string-inflection-camelcase))
;; }}}

;; Evil {{{
(use-package evil
  :init
  :config
  (evil-mode 1)

  ;; :W to save buffer
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "W[rite]" 'evil-write))

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
;; (use-package seoul256-theme
;;   :config
;;   (setq seoul256-background 234)
;;   (load-theme 'seoul256 t))

(use-package zenburn-theme
  :config
  (setq zenburn-override-colors-alist
        '(("zenburn-bg" . "#111111")
          ("zenburn-bg-1"  . "#555555")
          ("zenburn-bg+05" . "#222222")
          ("zenburn-bg+1"  . "#222222")
          ("zenburn-bg+2"  . "#3F3F3F")
          ("zenburn-bg+3"  . "#4F4F4F")))
  (load-theme 'zenburn t)

  (set-face-attribute 'fringe 'nil :background "#111111"))

(set-face-attribute 'mode-line nil
                    :height 110
                    :inverse-video nil
                    :box `(:line-width 6 :color ,(face-attribute 'mode-line :background) :style nil))

(set-face-attribute 'mode-line-inactive nil
                    :height 110
                    :inverse-video nil
                    :box `(:line-width 6 :color ,(face-attribute 'mode-line-inactive :background) :style nil))

(set-face-attribute 'vertical-border nil
                    :foreground (face-attribute 'mode-line-inactive :foreground))
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
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)")))
;; }}}

;; Ctags {{{
(use-package counsel-etags
  :bind (("C-]" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
        (lambda ()
          (add-hook 'after-save-hook
            'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories)
  (push "zig-cache" counsel-etags-ignore-directories)
  )
;; }}}

;; Keybindings {{{
(defun felipe/project-find-file ()
  (interactive)
  (let* ((pr (project-current t))
         (dirs (project-roots pr)))
    (project-find-file-in nil dirs pr)))

(define-key evil-normal-state-map (kbd "C-q") 'previous-error)
(define-key evil-normal-state-map (kbd "C-e") 'next-error)

(define-key evil-normal-state-map (kbd "C-a") 'ff-find-other-file)
(define-key evil-normal-state-map (kbd "C-p") 'felipe/project-find-file)
(define-key evil-normal-state-map (kbd "C-f") 'counsel-find-file)

(define-key evil-normal-state-map (kbd "C-b") 'counsel-switch-buffer)
(define-key evil-normal-state-map (kbd "M-q") 'kill-this-buffer)
(define-key evil-normal-state-map (kbd "M-j") 'next-buffer)
(define-key evil-normal-state-map (kbd "M-k") 'previous-buffer)

(define-key evil-normal-state-map (kbd "C-j") 'evil-window-next)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-prev)

(define-key evil-normal-state-map (kbd "<f7>") 'compile)

(use-package general
  :config
  (general-create-definer felipe/leader-def
    :prefix "SPC"))

(felipe/leader-def
  :keymaps 'normal
  "ir" 'ivy-resume

  "w/" 'split-window-right
  "w-" 'split-window-below
  "wd" 'delete-window
  "wb" 'balance-windows
  "wb" 'balance-windows

  "ff" 'counsel-find-file
  "fg" 'counsel-ag
  "fed" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))

  "bd" 'kill-this-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer

  "en" 'next-error
  "ep" 'previous-error

  "gs" 'magit-status)
;; }}}

;; Zooming {{{
(use-package default-text-scale
  :defer t
  :bind (("C-=" . default-text-scale-increase)
         ("C--" . default-text-scale-decrease)))
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
;; }}}

;; Magit {{{
(use-package magit
  :defer t
  :config
  (use-package evil-magit
    :after evil))

(use-package ssh-agency)
;; }}}

;; Highlight TODO {{{
(use-package hl-todo
  :config
  (global-hl-todo-mode))
;; }}}

(defun felipe/set-compile-command (proj-file command-str)
  (let ((proj-dir (locate-dominating-file default-directory proj-file)))
    (when proj-dir
        (set (make-local-variable 'compile-command) (format command-str proj-dir)))))

;; C/C++ {{{
(use-package meson-mode
  :defer t)
(use-package cmake-mode
  :defer t)
(use-package clang-format
  :defer t)

(add-to-list 'auto-mode-alist '("\\.inl\\'" . c-mode))

(defun felipe/c-c++-hook ()
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    ;; remove clang backend
    (setq company-backends (delete 'company-clang company-backends)))
  (felipe/set-compile-command "CMakeLists.txt" "cmake --build %s/build")
  (felipe/set-compile-command "meson.build" "ninja -C %s/build")
  (felipe/set-compile-command "Makefile" "make -f %s/Makefile")
  (felipe/set-compile-command "makefile" "make -f %s/makefile"))

(add-hook 'c++-mode-hook 'felipe/c-c++-hook)
(add-hook 'c-mode-hook 'felipe/c-c++-hook)

(felipe/leader-def 'normal c-mode-map
  "mf" 'clang-format-buffer)
(felipe/leader-def 'normal c++-mode-map
  "mf" 'clang-format-buffer)
;; }}}

;; D {{{
(use-package d-mode
  :defer t
  :config
  (add-hook 'd-mode-hook
            (lambda ()
              (felipe/set-compile-command "dub.sdl" "dub build"))))
;; }}}

;; Zig {{{
(use-package zig-mode
  :defer t
  :init
  (setq zig-format-on-save nil)
  :config
  (add-hook 'zig-mode-hook
            (lambda ()
              (felipe/set-compile-command "build.zig" "zig build"))))
;; }}}

;; GLSL {{{
(use-package glsl-mode
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
  :defer t
  :mode ("\\.hlsl\\'" . shader-mode))
;; }}}

;; Other major modes {{{
(use-package org
  :defer t)
(use-package yaml-mode
  :defer t)
(use-package markdown-mode
  :defer t)
;; }}}
