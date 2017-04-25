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
  (global-set-key [escape] 'evil-exit-emacs-state)

  (use-package evil-leader
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
      "tt" 'helm-themes
      "zz" 'text-scale-adjust
      "zi" 'text-scale-increase
      "zo" 'text-scale-decrease
      "ff" 'helm-find-files
      "fb" 'hs-toggle-hiding
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
      "ep" 'flycheck-previous-error))

  (use-package evil-commentary))

