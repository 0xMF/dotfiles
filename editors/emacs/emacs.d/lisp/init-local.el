;;; package -- My overrides to purcell's settings

;;; Commentary:

;;  purcell/emacs.d provides basic functionality that I am interested in using
;;  however not all settings are required by me and so this simple configuration
;;  setup to include my local settings after purcell/emacs.d has been run.

;;; Code:

;;----------------------------------------------------------------------------
;; Evil mode settings
;;----------------------------------------------------------------------------
(require 'evil)
(evil-mode 0)

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(require 'general)
;; bind a key globally in normal state; keymaps must be quoted
(setq general-default-keymaps 'evil-normal-state-map)
;; bind j and k in normal state globally
(general-define-key "j" 'evil-next-visual-line
                    "k" 'evil-previous-visual-line)

;; bind wm and wc
(general-define-key :prefix "w"
                    "c" 'whitespace-cleanup
                    "t" 'whitespace-mode
                    "w" 'delete-other-windows
                    "d" 'delete-window)

(general-define-key :prefix "b"
                    "d" 'kill-buffer
                    "h" 'previous-buffer
                    "l" 'next-buffer
                    "n" 'next-buffer
                    "p" 'previous-buffer)

;; named prefix key allows ; to be used a mapper for my keybindings
(setq my-leader1 ";")
(general-define-key :prefix my-leader1
                    "b" 'list-buffers
                    "j" 'evil-next-line
                    "k" 'evil-previous-line
                    "f" 'find-file)

;; a default prefix sequence
(setq general-default-prefix ";")
(general-define-key "f" 'find-file)

;; bind a key in multiple states
(general-define-key :keymaps 'org-mode-map
                    :states '(insert emacs)
                    "<tab>" 'org-cycle)

;; smooth scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

;; jump j/k always even in visual mode
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; esc quits
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

(define-key evil-normal-state-map (kbd "C-k") (lambda ()
(interactive)
(evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
(interactive)
(evil-scroll-down nil)))

;; 2 spaces for tabs
(setq-default tab-width 2 indent-tabs-mode nil)
(setq-default c-basic-offset 2 c-default-style "bsd")

;; no backups
(setq make-backup-files nil)

;; important for markdown and GFM export
(eval-after-load "org"
  '(require 'ox-md nil t))
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; yes to powerline
                                        ;(require 'powerline)
                                        ;(display-time-mode t)

;;----------------------------------------------------------------------------
;; Language mode settings
;;----------------------------------------------------------------------------

;; Erlang
(setq load-path (cons "/usr/lib/erlang/lib/tools-2.9/emacs" load-path))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)

(add-to-list 'load-path "/home/mark/repos/distel/elisp")
  (require 'distel)
  (distel-setup)

(use-package erlang
  :init
  (add-to-list 'auto-mode-alist '("\\.P\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '("\\.E\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '("\\.S\\'" . erlang-mode))
  :config
  (add-hook 'erlang-mode-hook
            (lambda ()
              (setq mode-name "erl"
                    erlang-compile-extra-opts '((i . "../include"))
                    erlang-root-dir "/usr/lib/erlang"))))

(use-package edts
  :init
  (setq edts-inhibit-package-check t
        edts-man-root "~/.emacs.d/edts/doc/18.2.1"))

(use-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function nil
        flycheck-erlang-include-path '("../include")
        flycheck-erlang-library-path '()
        flycheck-check-syntax-automatically '(save)))

(require 'flymake)
(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name temp-file
                                         (file-name-directory buffer-file-name))))
    (list "../escript/flymake-local.el" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))

;;---

;;----------------------------------------------------------------------------
;; Other misc. yet imp stuff goes here. Credit: technomancy/better-defaults
;;----------------------------------------------------------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;;----------------------------------------------------------------------------
;; User mode settings for UI/keyboard/look and feel
;;----------------------------------------------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'blueknight t) ; other nice themes: 'grandshell 'dakrone 'doom-molokai

;; Set default font
;; (used and saved through menu Options->Set Default Font... into cutom.el)

;; C-h-b: to check keybinding and which functions are bound to which keys
;; C-h-k: to check which key is bound to which function
;; C-h-m: to list current major mode's keys
;; C-g:   to close that opened Bindings window
;; checkout: http://ergoemacs.org/emacs/keyboard_shortcuts.html

(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "C-;") ctl-x-map)
(global-set-key (kbd "C-z") ctl-x-map)
(global-set-key (kbd "C-j") nil)
(global-set-key (kbd "C-j") (kbd "C-c")) ; maps one key to another
(global-set-key (kbd "M-s") 'execute-extended-command)
(global-set-key (kbd "M-z") 'execute-extended-command)
(global-set-key (kbd "C-M-;") 'evil-mode)
(global-set-key (kbd "C-M-j") 'list-buffers)
(global-set-key (kbd "C-M-h") 'previous-buffer)
(global-set-key (kbd "C-M-k") 'kill-some-buffers)
(global-set-key (kbd "C-M-l") 'next-buffer)
(global-set-key (kbd "C-M-<left>") 'previous-buffer)
(global-set-key (kbd "C-M-<right>") 'next-buffer)
(global-set-key (kbd "C-M-SPC") 'delete-other-windows)
(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-; u") 'undo-tree-visualize)

;; Org mode settings
(setq org-startup-indented nil)
(setq org-hide-leading-stars t)
(setq org-indent-mode-turns-off-org-adapt-indentation nil)
(setq org-indent-mode-turns-on-hiding-stars nil)
(setq org-pretty-entities t)

;; Use bullets (default if uncommented)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))



;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

;; keep purcell's emacs.d settings happy
(provide 'init-local)

;;; init-local.el ends here
