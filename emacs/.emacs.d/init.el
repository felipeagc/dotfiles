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

;; Pixelwise resizing
;; (setq frame-resize-pixelwise t)

;; Stop asking to follow symlinks
(setq vc-follow-symlinks t)

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
(setq show-paren-delay 0)
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
(add-hook 'prog-mode-hook 'hl-line-mode)

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
(use-package darktooth-theme
  :config
  (load-theme 'darktooth t)
  (darktooth-modeline-two)
  (set-face-background 'vertical-border (face-attribute 'darktooth-modeline-one-inactive :background))
  (set-face-foreground 'vertical-border (face-background 'vertical-border))
  (set-face-attribute 'mode-line nil :height 90)
  (set-face-attribute 'mode-line-inactive nil :height 90))

;; Modeline format
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
         "\\|\\(?:\\`.+?[#~]\\'\\)"))
  :config
  (use-package counsel-projectile
    :after projectile
    :config
    (counsel-projectile-mode)))

(use-package general
  :config
  (general-create-definer felipe/leader-def
    :prefix "SPC"))

;; Leader bindings
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
  "fg" 'counsel-rg
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

;; Highlight TODO
(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; C/C++
(use-package meson-mode)
(use-package cmake-mode)
(use-package clang-format)

(add-to-list 'auto-mode-alist '("\\.inl\\'" . c-mode))

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

;; Asciidoc
(define-derived-mode adoc-mode fundamental-mode "AsciiDoc"
  "major mode for editing asciidoc."
  (setq font-lock-defaults
        '((("^=.*$" . font-lock-keyword-face)
           ("^\\*+.+$" . font-lock-variable-name-face)
           ("[^ ]+://[^ ]+\\[.+\\]$" . font-lock-type-face)))))

(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

(felipe/leader-def 'normal adoc-mode-map
  "mbh" (lambda ()
          (interactive)
          (compile (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "asciidoctor -b html5 %s" file))))
  "mbp" (lambda ()
          (interactive)
          (compile (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "asciidoctor-pdf %s" file))))
  "mph" (lambda ()
          (interactive)
          (let ((filename (buffer-file-name)))
            (browse-url (concat "file://" (file-name-sans-extension filename) ".html"))))
  "mpp" (lambda ()
          (interactive)
          (start-process "zathura" nil "zathura" (concat (file-name-sans-extension (buffer-file-name)) ".pdf"))))

;; Other major modes
(use-package org)
(use-package yaml-mode)
