;;
;; General
;;
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq-default indent-tabs-mode nil)

(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(add-hook 'prog-mode-hook #'hs-minor-mode)

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

(use-package evil-vimish-fold
  :after evil
  :config
  (evil-vimish-fold-mode 1))

(use-package shackle
  :init
  (setq helm-display-function 'pop-to-buffer) ; make helm play nice
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.4)))
  (setq shackle-default-rule '(:same t)))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;
;; Electric pairs
;;
(electric-pair-mode)

