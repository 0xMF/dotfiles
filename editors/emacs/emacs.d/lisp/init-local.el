;;; package -- My overrides to purcell's settings

;;; Commentary:

;;  purcell/emacs.d provides basic functionality that I am interested in using
;;  however not all settings are required by me and so this simple configuration
;;  setup to include my local settings after purcell/emacs.d has been run.

;;; Code:

(show-paren-mode t)
(setq show-paren-style 'expression)

;;----------------------------------------------------------------------------
;; Evil mode settings
;;----------------------------------------------------------------------------
(require 'evil)
(evil-mode 1)

(setq evil-default-state-cursor '("green" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-replace-state-cursor '("red" box))
(setq evil-operator-state-cursor '("red" hollow))

(defun my-default-cursor()  "Cursor color indicates mode: white = emacs, green = evil."
       (if (string= (symbol-value 'evil-state) "normal")
           (set-cursor-color "green")
         (set-cursor-color "white")))

(add-hook 'evil-mode-hook 'my-default-cursor)


;;----------------------------------------------------------------------------
;; General keymap settings
;;----------------------------------------------------------------------------
(require 'general)

;; bind a key globally in normal state
(setq general-default-keymaps 'evil-normal-state-map)

;; bind j and k in normal state globally
(general-define-key "j" 'evil-next-visual-line
                    "k" 'evil-previous-visual-line)

;; bind wm and wc
(general-define-key :prefix "w"
                    "c" 'whitespace-cleanup
                    "d" 'delete-window
                    "t" 'whitespace-mode
                    "o" 'other-window
                    "w" 'delete-other-windows)

(general-define-key :prefix "b"
                    "d" 'kill-buffer
                    "h" 'previous-buffer
                    "k" 'next-buffer
                    "l" 'list-buffers
                    "n" 'next-buffer
                    "p" 'previous-buffer)

(general-define-key :prefix "z"
                    "f" #'vimish-fold
                    "d" #'vimish-fold-delete)

;; named prefix key allows ; to be used a mapper for my keybindings
(setq my-leader1 ";")
(general-define-key :prefix my-leader1
                    "a" 'org-agenda
                    "b" 'list-buffers
                    "c" 'org-capture
                    "d" 'org-agenda-list
                    "e" 'org-babel-execute-src-block
                    "f" 'find-file
                    "j" 'evil-next-line
                    "k" 'evil-previous-line
                    "l" 'whitespace-mode
                    "n" 'linum-mode
                    "q" 'fill-paragraph
                    "r" 'org-babel-open-src-block-result
                    "t" 'org-todo-list
                    "T" 'org-set-tags
                    "w" '(lambda () (interactive) (org-agenda-list 7))
                    "/" 'org-tags-view
                    "." 'org-tags-view
                    "\\" 'org-match-sparse-tree)

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
(defun minibuffer-keyboard-quit () "Abort recursive edit.
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

(define-key evil-normal-state-map (kbd "C-k") (lambda () (interactive)
                                                (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-j") (lambda () (interactive)
                                                (evil-scroll-down nil)))

;; 2 spaces for tabs
(setq-default tab-width 2 indent-tabs-mode nil)
(setq-default c-basic-offset 2 c-default-style "bsd")
(setq tab-width 2
      tab-stop-list (number-sequence 2 20 2)
      indent-line-function 'tab-to-tab-stop)

;; no backups
(setq make-backup-files nil)

;; important for markdown and GFM export
(eval-after-load "org" '(require 'ox-md nil t))
(eval-after-load "org" '(require 'ox-gfm nil t))

;; yes to powerline
(require 'powerline)
(display-time-mode t)

;;----------------------------------------------------------------------------
;; Language mode settings
;;----------------------------------------------------------------------------

;; Erlang
(setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.9.1/emacs" load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
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
            (lambda () (setq mode-name "erl"
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

(defun flymake-erlang-init () "Compiles Erlang code on-the-fly as it's written."
       (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
              (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
         (list "~/.emacs.d/escript/flymake-erlang" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))

;;----------------------------------------------------------------------------
;; Pandoc-mode settings
;;----------------------------------------------------------------------------
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

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
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))


;;----------------------------------------------------------------------------
;; Abbreviations
;;----------------------------------------------------------------------------
(setq-default abbrev-mode t)
(read-abbrev-file "~/.abbrev_defs")
(setq save-abbrevs t)

;;----------------------------------------------------------------------------
;; Keybindings
;;----------------------------------------------------------------------------
;;
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

(global-set-key (kbd "C-<escape>") 'evil-mode)
(global-set-key (kbd "C-M-;") 'evil-mode)

(global-set-key (kbd "C-M-j") 'list-buffers)
(global-set-key (kbd "C-M-h") 'previous-buffer)
(global-set-key (kbd "C-M-k") 'kill-some-buffers)
(global-set-key (kbd "C-M-l") 'next-buffer)
(global-set-key (kbd "C-M-<left>") 'previous-buffer)
(global-set-key (kbd "C-M-<right>") 'next-buffer)
(global-set-key (kbd "C-M-SPC") 'delete-other-windows)
(global-set-key (kbd "C-M-RET") 'org-insert-heading)
(global-set-key (kbd "C-M-<return>") 'org-insert-heading)
(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-; u") 'undo-tree-visualize)
(global-set-key (kbd "<tab>") 'tab-to-tab-stop)

;;----------------------------------------------------------------------------
;; Org mode settings
;;----------------------------------------------------------------------------
(setq org-startup-indented nil)
(setq org-hide-leading-stars t)
(setq org-indent-mode-turns-off-org-adapt-indentation nil)
(setq org-indent-mode-turns-on-hiding-stars nil)
(setq org-pretty-entities t)

;; Removes org sparse tree views correctly
;; Credit: https://stackoverflow.com/a/44158824
(setq lexical-binding t)

(let ((*outline-data* nil))
  (defun org-save-outline-state (&optional arg type)
    (setq *outline-data* (org-outline-overlay-data t)))

  (defun org-restore-outline-state (&optional arg)
    (when *outline-data*
      (org-set-outline-overlay-data *outline-data*)
      (setq *outline-data* nil))))

(advice-add 'org-sparse-tree :before 'org-save-outline-state)
(advice-add 'org-match-sparse-tree :before 'org-save-outline-state)
(advice-add 'org-ctrl-c-ctrl-c :after 'org-restore-outline-state)
;; ---

;; Use bullets (default if uncommented)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(defun kill-misc-buffers() "Permanently remove some buffers."
       (if (get-buffer "*scratch*")
           (kill-buffer "*scratch*"))
       (if (get-buffer "*reg group-leader*")
           (kill-buffer "*reg group-leader*")))
(add-hook 'after-change-major-mode-hook 'kill-misc-buffers)

(evil-define-key 'insert org-mode-map (kbd "C-<tab>") #'tab-to-tab-stop)

;;----------------------------------------------------------------------------
;; Miscalleanous settings
;; User mode settings for UI/keyboard/look and feel
;;----------------------------------------------------------------------------
(setq browse-url-browser-function 'eww-browse-url)
(load-file "~/.emacs.d/lisp/secrets.el")

;; wrap lines (hard return) around column 100
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook '(lambda() (set-fill-column 100)))

(menu-bar-mode -1)
(require 'vimish-fold)

;; Set default font
;; (used and saved through menu Options->Set Default Font... into cutom.el)

;; optionally (set-frame-font "Source Code Pro Semibold 10")
(set-frame-font "DejaVu Sans Mono 11")

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

;; keep purcell's emacs.d settings happy
(provide 'init-local)

;;; init-local.el ends here
