;;
;; Rust
;;
(use-package rust-mode
  :after evil-leader
  :config
  (use-package racer
    :after company
    :config
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode))
  (use-package flycheck-rust
    :after flycheck
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
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
(use-package elpy
  :config
  (elpy-enable)
  (setq elpy-modules (delete 'elpy-module-highlight-indentation elpy-modules)))

;;
;; Lua
;;
(use-package lua-mode
  :after evil-leader
  :init
  (setq lua-indent-level 2)
  :config
  (use-package company-lua
    :after company
    :config
    (add-to-list 'company-backends 'company-lua))
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
(use-package irony
  :after evil-leader
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (use-package company-irony
    :after company
    :config
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony)))
  (use-package irony-eldoc)
  (use-package clang-format))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(evil-leader/set-key-for-mode 'c++-mode
  "mf" 'clang-format-buffer
  "ms" 'ff-find-other-file)

(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))

;;
;; Haskell
;;
(use-package intero
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

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
;; Go
;;
(use-package go-mode
  :config
  (evil-leader/set-key-for-mode 'go-mode
    "mf" 'gofmt
    "mi" 'go-import-add)
  (use-package golint
    :after flycheck)
  (use-package company-go
    :config
    (add-hook 'go-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-go))
                              (company-mode))))
  (use-package go-eldoc
    :config
    (add-hook 'go-mode-hook 'go-eldoc-setup)))
  
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

(add-hook 'lua-mode
  (function (lambda ()
    (setq tab-width 2)
    (setq evil-shift-width 2))))
