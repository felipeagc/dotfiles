;; TODO:
;; Projectile
;; Git gutter
;; Code folding
;; Show trailing spaces (and erase them on save)
;; Toggle flycheck for buffer


;;; Code:

;;
;; Security
;;

;;  (if (fboundp 'gnutls-available-p)
;;      (fmakunbound 'gnutls-available-p))

(require 'cl)
(setq tls-checktrust t)

(setq python (executable-find "python"))

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string (concat python " -m certifi"))))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))



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
;; Macros
;;

(defmacro def-popup! (&rest params)
`(push ',params shackle-rules))

(defmacro after! (feature &rest forms)
  "A smart wrapper around `with-eval-after-load', that supresses warnings
during compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (boundp 'byte-compile-current-file))
             (not byte-compile-current-file)
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         'progn
       (message "after: cannot find %s" feature)
       'with-no-warnings)
(with-eval-after-load ',feature ,@forms)))

(defmacro add-hook! (hook &rest func-or-forms)
  "A convenience macro for `add-hook'.
HOOK can be one hook or a list of hooks. If the hook(s) are not quoted, -hook is
appended to them automatically. If they are quoted, they are used verbatim.
FUNC-OR-FORMS can be a quoted symbol, a list of quoted symbols, or forms. Forms will be
wrapped in a lambda. A list of symbols will expand into a series of add-hook calls.
Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))"
  (declare (indent defun) (debug t))
  (unless func-or-forms
    (error "add-hook!: FUNC-OR-FORMS is empty"))
  (let* ((val (car func-or-forms))
         (quoted (eq (car-safe hook) 'quote))
         (hook (if quoted (cadr hook) hook))
         (funcs (if (eq (car-safe val) 'quote)
                    (if (cdr-safe (cadr val))
                        (cadr val)
                      (list (cadr val)))
                  (list func-or-forms)))
         (forms '()))
    (mapc
     (lambda (f)
       (let ((func (cond ((symbolp f) `(quote ,f))
                         (t `(lambda (&rest _) ,@func-or-forms)))))
         (mapc
          (lambda (h)
            (push `(add-hook ',(if quoted h (intern (format "%s-hook" h))) ,func) forms))
          (-list hook)))) funcs)
`(progn ,@forms)))

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

(setq-default fringes-outside-margins t)


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

  (require 'doom-nlinum)

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

  ; Overload shifts so that they don't lose the selection
  (define-key evil-visual-state-map (kbd ">") 'felipe/evil-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'felipe/evil-shift-left-visual)
  (define-key evil-visual-state-map [tab] 'felipe/evil-shift-right-visual)
  (define-key evil-visual-state-map [S-tab] 'felipe/evil-shift-left-visual)

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
  :init
  (setq flycheck-highlighting-mode nil)
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

  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  (evil-leader/set-key-for-mode 'typescript-mode
    "mf" 'tide-format
    "mg" 'tide-goto-reference))


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
;; Shaders
;;

(use-package glsl-mode)


;;
;; Git
;;

(use-package gitconfig-mode
  :mode ("/\\.?git/?config$" "/\\.gitmodules$")
  :init (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package gitignore-mode
  :mode ("/\\.gitignore$"
         "/\\.git/info/exclude$"
         "/git/ignore$"))

(use-package git-gutter
  :commands (git-gutter-mode)
  :init
  (add-hook! (text-mode prog-mode conf-mode) 'git-gutter-mode)
  :config

  (use-package git-gutter-fringe
    :config
    (def-popup! "^\\*git-gutter.+\\*$" :align below :size 15 :noselect t :regexp t)

    ;; NOTE If you want the git gutter to be on the outside of the margins (rather
    ;; than inside), `fringes-outside-margins' should be non-nil.

    ;; colored fringe "bars"
    (define-fringe-bitmap 'git-gutter-fr:added
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:modified
      [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
      nil nil 'center)
    (define-fringe-bitmap 'git-gutter-fr:deleted
      [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
      nil nil 'center)

    ;; Refreshing git-gutter
    (advice-add 'evil-force-normal-state :after 'git-gutter)
    (add-hook 'focus-in-hook 'git-gutter:update-all-windows)))


(use-package magit
  :after evil-leader
  :commands (magit-status)
  :config
  (use-package evil-magit)

  (evil-leader/set-key
    "gg" 'magit-status
    "gs" 'magit-stage
    "gu" 'magit-unstage
    "gr" 'magit-unstage-all
    "gp" 'magit-push
    "gc" 'magit-commit)

  (def-popup! "^\\*magit.+" :align below :regexp t)
  ;; Prevent magit + evil-snipe conflicts
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))


;;
;; Projectile
;;

(use-package projectile)


;;
;; Modeline
;;


(use-package f)

(use-package s)


(use-package powerline)

(use-package all-the-icons
  :after powerline
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
    (git-gutter-fringe glsl-mode scss-mode pug-mode web-mode tide helm-themes powerline projectile evil-magit magit elisp-format haskell-mode clang-format js2-mode cargo flycheck-pos-tip flycheck-rust flycheck company-lua company-anaconda racer company evil-smartparens smartparens helm which-key evil-leader evil doom-themes evil-nerd-commenter eyebrowse nlinum use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
