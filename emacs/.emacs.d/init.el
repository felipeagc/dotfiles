(defconst -is-a-mac (eq system-type 'darwin))

(setq vc-follow-symlinks t)

;; Backups
(setq
  backup-by-copying t      ; don't clobber symlinks
  backup-directory-alist '(("." . "~/.emacs.d/saves/"))    ; don't litter my fs tree
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t
  global-visual-line-mode t
  visible-bell 1)

;; Font
(add-to-list 'default-frame-alist '(font . "Menlo-13"))

;; Visuals
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)

;; Improve macOS stuff
(when -is-a-mac
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'none)
    ;; Make mouse wheel / trackpad scrolling less jerky
    (setq mouse-wheel-scroll-amount '(1
                                      ((shift) . 5)
                                      ((control))))
    (dolist (multiple '("" "double-" "triple-"))
      (dolist (direction '("right" "left"))
        (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
    (global-set-key (kbd "M-v") 'yank)
    (global-set-key (kbd "M-c") 'kill-ring-save)
    (global-set-key (kbd "M-`") 'ns-next-frame)
    (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
    (global-set-key (kbd "M-˙") 'ns-do-hide-others)
    (with-eval-after-load 'nxml-mode
      (define-key nxml-mode-map (kbd "M-h") nil))
    (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
    )

(setq initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setq display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes
(setq auto-revert-interval 1)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

;; Fix scroll margin
(pixel-scroll-precision-mode)
(setq scroll-margin 6
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;; Line numbers
(setq line-number-mode t)                        ; Show current line in modeline
(setq column-number-mode t)                      ; Show column as well
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Fix show-paren delay
(setq show-paren-delay 0.0)
(show-paren-mode 1)


;; Straight
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

(use-package straight
  :custom
  (straight-use-package-by-default t))

;; Fix macOS PATH variable
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; No littering
(use-package no-littering)

;; Zooming
(use-package default-text-scale
  :defer t
  :bind (("C-=" . default-text-scale-increase)
         ("C--" . default-text-scale-decrease)
         ("C-0" . default-text-scale-reset)))

;; Evil
(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :config
  (global-undo-tree-mode)
  (with-eval-after-load 'undo-tree (defun undo-tree-overridden-undo-bindings-p () nil)))

(use-package evil
  :after undo-tree
  :ensure t

  :init
  (setq evil-want-integration t ;; This is optional since it's already set to t by default.
	evil-want-keybinding nil)

  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)

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

  (eval-after-load "dired" '(progn
    (define-key evil-normal-state-map (kbd "-") 'dired-jump)
    (define-key dired-mode-map (kbd "-") 'dired-up-directory)))

  (define-key evil-normal-state-map (kbd "C-b") 'switch-to-buffer)
  (define-key evil-normal-state-map (kbd "C-p") 'project-find-file)
  (define-key evil-normal-state-map (kbd "C-a") 'ff-find-other-file)

  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-next)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-prev)

  (define-key evil-insert-state-map (kbd "C-n") 'completion-at-point)
  (define-key evil-insert-state-map (kbd "C-p") 'completion-at-point))

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package string-inflection
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "grc") 'string-inflection-lower-camelcase)
  (define-key evil-normal-state-map (kbd "grs") 'string-inflection-underscore)
  (define-key evil-normal-state-map (kbd "grp") 'string-inflection-camelcase))

(use-package general
  :ensure t
  :config
  ;; allow for shorter bindings -- e.g., just using things like nmap alone without general-* prefix
  (general-evil-setup t)
  (general-auto-unbind-keys)

  (general-create-definer felipe/leader-def
      :states '(normal)
      :keymaps 'override
      :prefix "SPC")

  (felipe/leader-def
     "w" '(:ignore t :which-key "window")
     "w/" '(split-window-right :which-key "split right")
     "w-" '(split-window-below :which-key "split below")
     "wd" '(delete-window :which-key "delete window")
     "wb" '(balance-windows :which-key "balance windows")
  
     "p" '(:ignore t :which-key "project")
     "pp" '(project-switch-project :which-key "switch project")
  
     "f" '(:ignore t :which-key "file")
     "fg" 'counsel-rg
     "fe" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :which-key "emacs config")
  
     "b" '(:ignore t :which-key "buffer")
     "bd" 'kill-this-buffer
     "bn" 'next-buffer
     "bp" 'previous-buffer
  
     "g" '(:ignore t :which-key "git")
     "gs" 'magit-status
  
     ;; "mf" 'eglot-format-buffer
     ;; "mr" 'eglot-rename
     ;; "mi" 'eglot-code-actions
   )
)

;; Which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Ivy/counsel
(use-package ivy
  :ensure t
  :config
  (ivy-mode))
(use-package counsel
  :ensure t)

;; Org-mode
(use-package org
  :ensure t)

;; Magit
(use-package magit
  :ensure t)

;; Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-homage-black t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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

;; Coq
(use-package proof-general
  :ensure t
  :init
  (setq proof-splash-enable nil)
  :config
  (add-hook 'coq-mode-hook (lambda () (undo-tree-mode 1))))


;; Ocaml
(use-package tuareg
  :ensure t)
