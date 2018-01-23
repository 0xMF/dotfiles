;;; package -- My overrides to purcell's settings

;;; Commentary:

;;  purcell/emacs.d provides basic functionality that I am interested in using
;;  however not all settings are required by me and so this simple configuration
;;  setup to include my local settings after purcell/emacs.d has been run.

;;; Code:

;; bring in my preferred packages specified in custom.el/package-selected-packages
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq package-list '(
  add-node-modules-path aggressive-indent alert anaconda-mode anzu auto-compile avy beacon
  browse-at-remote browse-kill-ring bug-reference-github bundler cask-mode cider cl-lib-highlight
  cljsbuild-mode clojure-mode cmd-to-echo coffee-mode color-theme-sanityinc-solarized
  color-theme-sanityinc-tomorrow command-log-mode company company-anaconda company-quickhelp
  company-terraform counsel css-eldoc csv-mode csv-nav darcsum default-text-scale dhall-mode diff-hl
  diminish dired-sort diredfl disable-mouse docker docker-compose-mode dockerfile-mode dynamic-spaces
  elein elisp-slime-nav elm-mode erlang evil evil-vimish-fold exec-path-from-shell expand-region
  flycheck flycheck-clojure flycheck-color-mode-line flycheck-elm flycheck-package fullframe general
  git-blamed git-messenger git-timemachine gitconfig-mode github-clone github-issues gitignore-mode
  goto-gem guide-key haskell-mode highlight-escape-sequences highlight-quoted hindent
  hippie-expand-slime httprepl ibuffer-vc immortal-scratch inf-ruby intero ipretty ivy ivy-historian
  js-comint js2-mode json-mode less-css-mode list-unicode-display macrostep magit magit-gh-pulls
  markdown-mode mediawiki mmm-mode mode-line-bell move-dup multiple-cursors nlinum org-bullets
  org-cliplink org-fstree org-pomodoro page-break-lines paredit paredit-everywhere php-mode
  pip-requirements powerline prettier-js project-local-variables projectile projectile-rails psc-ide
  purescript-mode racer rainbow-delimiters rainbow-mode restclient robe rspec-mode ruby-compilation
  ruby-hash-syntax rust-mode sass-mode scratch scss-mode session skewer-less skewer-mode slime
  slime-company smarty-mode smex sql-indent switch-window symbol-overlay tagedit terraform-mode
  textile-mode tidy toml-mode typescript-mode undo-tree unfill uptimes use-package vc-darcs vlf
  wgrep whitespace-cleanup-mode whole-line-or-region writeroom-mode yagist yaml-mode yard-mode yari))

; install the missing packages when using emacs 24.5 and below
(unless (version<= emacs-version "24.5")
  (dolist (package package-list)
    (unless (package-installed-p package)
        (package-install package)))
  (package-install-selected-packages))

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
                    "d" '(lambda () (interactive) (kill-buffer)(delete-window))
                    "f" 'fci-mode
                    "h" 'previous-buffer
                    "j" 'next-buffer
                    "k" 'next-buffer
                    "l" 'previous-buffer
                    "t" 'whitespace-mode
                    "o" 'other-window
                    "w" 'delete-other-windows)

(general-define-key :prefix "b"
                    "c" 'yank
                    "d" 'kill-buffer
                    "f" 'markdown-follow-thing-at-point
                    "h" 'previous-buffer
                    "j" 'next-buffer
                    "l" 'list-buffers
                    "n" 'next-buffer
                    "o" 'counsel-find-file
                    "p" 'previous-buffer
                    "x" 'evil-delete)

(general-define-key :prefix "z"
                    "d" #'vimish-fold-delete
                    "f" #'vimish-fold
                    "g" 'save-this-word
                    "t" 'save-this-word)

;; named prefix key allows ; to be used a mapper for my keybindings
(setq my-leader1 ";")
(general-define-key :prefix my-leader1
                    "a" 'org-agenda
                    "b" 'list-buffers
                    "c" 'org-capture
                    "d" 'org-agenda-list
                    "e" 'org-babel-execute-src-block
                    "E" 'org-babel-open-src-block-result
                    "g" 'magit-status
                    "j" 'evil-next-line
                    "k" 'evil-previous-line
                    "l" 'whitespace-mode
                    "m" 'magit-mode
                    "L" 'org-open-at-point
                    "n" 'linum-mode
                    "o" 'find-file
                    "q" 'fill-paragraph
                    "r" 'undo-tree-redo
                    "s" 'dired-jump
                    "u" 'undo-tree-undo
                    "t" 'org-todo-list
                    "T" 'org-set-tags
                    "w" '(lambda () (interactive) (org-agenda-list 7))
                    "x" 'evil-delete
                    "+" 'text-scale-increase
                    "=" 'text-scale-increase
                    "-" 'text-scale-decrease
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

;; Credit: https://stackoverflow.com/questions/22107182/in-emacs-flyspell-mode-how-to-add-new-word-to-dictionary
(defun save-this-word ()
  "Save word to personal dict."
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))
;; ---

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


;;----------------------------------------------------------------------------
;; Pandoc-mode settings
;;----------------------------------------------------------------------------
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

(use-package markdown-mode)
:init
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda ()
                                (set-fill-column 72)))

(require 'mediawiki)
:init
;; setup files ending in “.mw” to open in mediwiki-mode
(add-to-list 'auto-mode-alist '("\\.mw\\'" . mediawiki-mode))


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
(global-set-key (kbd "C-M-<up>") 'previous-buffer)
(global-set-key (kbd "C-M-<prior>") 'previous-buffer)
(global-set-key (kbd "C-M-<right>") 'next-buffer)
(global-set-key (kbd "C-M-<down>") 'next-buffer)
(global-set-key (kbd "C-M-<next>") 'next-buffer)
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
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(setq org-startup-indented nil)
(setq org-hide-leading-stars t)
(setq org-indent-mode-turns-off-org-adapt-indentation nil)
(setq org-indent-mode-turns-on-hiding-stars nil)
(setq org-pretty-entities t)
(setq org-export-with-section-numbers nil)
(setq org-table-auto-blank-field nil)
(setq org-startup-indented t)

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

(setq org-time-stamp-custom-formats '("<%b-%d %a>" "<%m/%d/%y %a>" "<%m/%d/%y %a %H:%M>"))

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

;; Yup we want spell check to be turned on automatically in org mode
(add-hook 'org-mode-hook 'turn-on-flyspell)

;;----------------------------------------------------------------------------
;; Miscalleanous settings
;; User mode settings for UI/keyboard/look and feel
;;----------------------------------------------------------------------------
(setq browse-url-browser-function 'eww-browse-url)
(load-file "~/.emacs.d/lisp/secrets.el")

;; wrap lines (hard return) around column 100
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook '(lambda() (set-fill-column 100)))

;; replaces Emacs undo-redo system with something MUCH nicer!
(global-undo-tree-mode t)
(setq undo-tree-auto-save-history t)

;; M-x slime calls sbcl
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(setq slime-default-lisp 'sbcl)

(menu-bar-mode -1)
(require 'vimish-fold)

;; Using mouse to select and copy text to the clipboard
;; Source: https://stackoverflow.com/questions/13036155/how-to-combine-emacs-primary-clipboard-copy-and-paste-behavior-on-ms-windows
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(global-set-key [mouse-2] 'mouse-yank-at-click)
;;

;; Set default font
;; (used and saved through menu Options->Set Default Font... into cutom.el)

;; optionally (set-frame-font "Source Code Pro Semibold 10")
(set-frame-font "DejaVu Sans Mono 11")
(setenv "PATH" (concat (getenv "PATH") ":~/bin"))
(setq exec-path (append exec-path '("~/bin")))
(setq-default major-mode 'org-mode)

(require 'fill-column-indicator)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

;; keep purcell's emacs.d settings happy
(provide 'init-local)

;;;  -*- mode: Lisp;-*
;;; init-local.el ends here
