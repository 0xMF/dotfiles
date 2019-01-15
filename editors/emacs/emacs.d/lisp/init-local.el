;;; package -- My overrides to purcell's settings

;;; Commentary:

;;  purcell/emacs.d provides basic functionality that I am interested in using
;;  however not all settings are required by me and so this simple configuration
;;  setup to include my local settings after purcell/emacs.d has been run.

;;; Code:

;; bring in my preferred packages specified in custom.el/package-selected-packages
;;(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq my-required-packages '(evil evil-collection evil-magit
                                  fill-column-indicator general org-beautify-theme org-bullets
                                  org-pdfview powerline smart-mode-line
                                  smart-mode-line-powerline-theme use-package vimish-fold))
(dolist (package my-required-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; remember to add benchmark-init to my-required-packages if reusing
;;(require 'benchmark-init)
;;(benchmark-init/activate)

;; install the missing packages when using Emacs 24.5.1 and below
(setq vc-follow-symlinks t)
(show-paren-mode t)
(setq show-paren-style 'expression)

;; prefer newer source instead of older bytecode
(setq load-prefer-newer t)

;;----------------------------------------------------------------------------
;; Evil mode settings
;;----------------------------------------------------------------------------
(require 'evil)
(require 'evil-collection)
(require 'evil-magit)

(evil-mode 1)
(evil-collection-init)
(evil-magit-init)

(setq evil-default-state-cursor '("green" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-replace-state-cursor '("red" box))
(setq evil-operator-state-cursor '("red" hollow))

(defun my-default-cursor()  "Cursor color indicates mode: white = Emacs, green = evil (Vi/Vim)."
       (if (string= (symbol-value 'evil-state) "normal")
           (set-cursor-color "green")
         (set-cursor-color "white")))

;; set Tab key to work correctly with Org-mode tables and headings.
;;(setq evil-want-C-i-jump nil)
;;(when evil-want-C-i-jump
;;      (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward))

(add-hook 'evil-mode-hook 'my-default-cursor)

;;----------------------------------------------------------------------------
;; General keymap settings
;;----------------------------------------------------------------------------
(require 'general)

;; bind a key globally in normal state
(setq general-default-keymaps 'evil-normal-state-map)

;; bind j and k in normal state globally
(general-define-key "j" 'evil-next-visual-line
                    "k" 'evil-previous-visual-line
                    "SPC" 'evil-scroll-page-down
                    "DEL" 'evil-scroll-page-up)

;; bind wm and wc
(general-define-key :prefix "w"
                    "a" 'org-toggle-link-display
                    "c" 'whitespace-cleanup
                    "d" '(lambda () (interactive) (kill-buffer)(delete-window))
                    "f" 'fci-mode
                    "h" 'previous-buffer
                    "j" 'next-buffer
                    "k" 'next-buffer
                    "l" 'previous-buffer
                    "n" 'next-buffer
                    "N" 'other-window
                    "t" 'whitespace-mode
                    "o" 'other-window
                    "p" 'previous-buffer
                    ;; "P" 'other-window
                    "r" '0xMF/reset
                    "w" 'delete-other-windows)

(general-define-key :prefix "b"
                    "a" 'describe-bindings
                    "b" 'evil-scroll-page-up
                    "c" 'yank
                    "d" 'sanityinc/toggle-delete-other-windows
                    "f" 'markdown-follow-thing-at-point
                    "h" 'previous-buffer
                    "j" 'next-buffer
                    "l" 'list-buffers
                    "n" 'next-buffer
                    "o" 'counsel-find-file
                    "p" 'previous-buffer
                    "r" '0xMF/reset
                    "t" '(lambda () (interactive) (kill-buffer)(delete-window))
                    "x" 'evil-delete)

(general-define-key :prefix "z"
                    "d" #'vimish-fold-delete
                    "f" #'vimish-fold
                    "g" 'save-this-word
                    "t" 'save-this-word)

;; named prefix key allows ; to be used a mapper for my keybindings
(setq my-leader1 ";")
(general-define-key :prefix my-leader1
                    "a" 'org-toggle-link-display
                    "A" 'org-agenda
                    "b" 'switch-to-buffer
                    "c" 'org-capture
                    "d" 'org-agenda-list
                    "e" 'org-babel-execute-src-block
                    "E" 'org-babel-open-src-block-result
                    "f" 'file-reload
                    "g" 'magit-status
                    "j" 'evil-next-line
                    "k" 'evil-previous-line
                    "l" 'whitespace-mode
                    "m" 'magit-mode
                    "L" 'org-open-at-point
                    "n" 'linum-mode
                    "o" 'find-file
                    "q" 'fill-paragraph
                    "r" '0xMF/reset
                    "R" 'undo-tree-redo
                    "s" '0xMF/startup
                    "u" 'undo-tree-undo
                    "t" 'org-todo-list
                    "T" 'org-set-tags
                    "w" '(lambda () (interactive) (org-agenda-list 7))
                    "x" 'evil-delete
                    "+" '(lambda () (interactive) (text-scale-increase 2))
                    "=" '(lambda () (interactive) (text-scale-increase 3))
                    "-" 'text-scale-decrease
                    "0" '(lambda () (interactive) (text-scale-adjust 0))
                    "$" 'toggle-truncate-lines
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

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(define-key evil-normal-state-map (kbd "C-k") (lambda () (interactive) (evil-scroll-up nil)))
(global-set-key (kbd "S-SPC") 'evil-scroll-page-up)
(define-key evil-normal-state-map (kbd "C-j") (lambda () (interactive) (evil-scroll-down nil)))
(define-key evil-normal-state-map (kbd "C-d") 'save-buffer)
(define-key evil-normal-state-map (kbd "C-n") 'next-buffer)
(define-key evil-normal-state-map (kbd "C-p") 'previous-buffer)

;; Credit: [StackOverflow] in-emacs-flyspell-mode-how-to-add-new-word-to-dictionary
(defun save-this-word ()
  "Save word to personal dict."
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location
                           (cadr word) (caddr word) current-location))))
;; ---

;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*")
      (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; 2 spaces for tabs
(setq-default tab-width 2 indent-tabs-mode nil)
(setq-default c-basic-offset 2 c-default-style "bsd")
(setq tab-width 2
      tab-stop-list (number-sequence 2 20 2)
      indent-line-function 'tab-to-tab-stop)

;; no backups
(setq make-backup-files nil)

;; important for markdown, GFM export, and viewing pdfs
(eval-after-load "org" '(require 'ox-md nil t))
(eval-after-load "org" '(require 'ox-gfm nil t))
(eval-after-load "org" '(require 'org-pdfview))
(eval-after-load "org" '(require 'htmlize))

(pdf-tools-install)
(add-to-list 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)

(defun my-eww-settings ()
  "Enable vi-style keybindings."
  (define-key eww-mode-map "h" 'previous-char)
  (define-key eww-mode-map "j" 'next-line)
  (define-key eww-mode-map "k" 'previous-line)
  (define-key eww-mode-map "l" 'next-char)
  (define-key eww-mode-map "n" 'eww-forward-url)
  (define-key eww-mode-map "p" 'eww-back-url))
;; -- DANGER: avoid setting keys in evil-normal-map.
;;(define-key evil-normal-state-map "n" 'eww-forward-url)
;;(define-key evil-normal-state-map "p" 'eww-back-url))
(add-hook 'eww-mode-hook 'my-eww-settings)

(defun my-pdf-view-settings ()
  "Disable blinking in pdf-view-mode and enable vi-style keybindings."
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (setq pdf-view-continuous 't)
              (set (make-local-variable 'evil-emacs-state-cursor) (list nil))
              (local-set-key "j" 'pdf-view-next-line-or-next-page)
              (local-set-key "k" 'pdf-view-previous-line-or-previous-page)
              (local-set-key "n" 'pdf-view-scroll-up-or-next-page)
              (local-set-key "p" 'pdf-view-scroll-down-or-previous-page)
              (local-set-key "J" 'pdf-view-next-page)
              (local-set-key "K" 'pdf-view-previous-page)
              (local-set-key "g" 'pdf-view-first-page)
              (local-set-key "G" 'pdf-view-last-page)
              (local-set-key "l" 'pdf-view-goto-page)
              (local-set-key "/" 'isearch-forward)
              (local-set-key "?" 'isearch-backward)
              (local-set-key (kbd "<mouse-5>") 'pdf-view-next-line-or-next-page)
              (local-set-key (kbd "<mouse-4>") 'pdf-view-previous-line-or-previous-page))))
(add-hook 'pdf-view-mode-hook 'my-pdf-view-settings)

;; better clipboard copy-paste with evil
(fset 'evil-visual-update-x-selection 'ignore)

;; yes to powerline on a smart-mode-line
(require 'powerline)
(require 'smart-mode-line)
(require 'smart-mode-line-powerline-theme)
(setq powerline-arrow-shape 'arrow)
(powerline-vim-theme)
(setq sml/theme 'powerline)
(setq sml/no-confirm-load-theme t)
(setf rm-blacklist "")
(display-time-mode t)
(sml/setup)


;;----------------------------------------------------------------------------
;; Language mode settings
;;----------------------------------------------------------------------------

(use-package markdown-mode)
:init
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda () (set-fill-column 72)))

(unless (version< emacs-version "27")
  (setq url-http-referer 'nil))

;; remember to add mediawiki to my-required-packages if using mediawiki
;;(require 'mediawiki)
;;:init
;;setup files ending in “.mw” to open in mediwiki-mode
;;(add-to-list 'auto-mode-alist '("\\.mw\\'" . mediawiki-mode))


;; elm-mode
(setq elm-interactive-command '("elm" "repl")
      elm-reactor-command '("elm" "reactor")
      elm-reactor-arguments '("--port" "8000")
      elm-compile-command '("elm" "make")
      elm-compile-arguments '("--output=elm.js" "--debug")
      elm-package-command '("elm" "package"))

;;----------------------------------------------------------------------------
;; Other misc. yet imp stuff goes here. Credit: technomancy/better-defaults
;;----------------------------------------------------------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

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
(global-set-key (kbd "C-n") 'next-buffer)
(global-set-key (kbd "C-p") 'previous-buffer)

(global-set-key (kbd "C-<escape>") 'evil-mode)
(global-set-key (kbd "C-M-<escape>") 'evil-mode)
(global-set-key (kbd "C-M-;") 'evil-mode)

(global-set-key (kbd "C-M-j") 'list-buffers)
(global-set-key (kbd "C-M-h") 'previous-buffer)
(global-set-key (kbd "C-M-k") 'kill-some-buffers)
(global-set-key (kbd "C-M-l") 'next-buffer)
(global-set-key (kbd "C-M-<") 'previous-buffer)
(global-set-key (kbd "C-M->") 'next-buffer)
(global-set-key (kbd "C-M-<left>") 'previous-buffer)
(global-set-key (kbd "C-M-<up>") 'previous-buffer)
(global-set-key (kbd "C-M-s-<up>") 'previous-buffer)
(global-set-key (kbd "C-M-<prior>") 'previous-buffer)
(global-set-key (kbd "C-M-<right>") 'next-buffer)
(global-set-key (kbd "C-M-s-<right>") 'previous-buffer)
(global-set-key (kbd "C-M-<down>") 'next-buffer)
(global-set-key (kbd "C-M-s-<down>") 'next-buffer)
(global-set-key (kbd "C-M-<next>") 'next-buffer)
(global-set-key (kbd "C-M-s-<left>") 'next-buffer)
(global-set-key (kbd "C-M-SPC") 'delete-other-windows)
(global-set-key (kbd "C-M-RET") 'org-insert-heading)
(global-set-key (kbd "C-M-<return>") 'org-insert-heading)
(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)
(global-set-key (kbd "C-<prior>") 'previous-buffer)
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-; u") 'undo-tree-visualize)
(global-set-key (kbd "<tab>") 'tab-to-tab-stop)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-d") 'save-buffer)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

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
(setq org-list-allow-alphabetical t)


;; Removes org sparse tree views correctly
;; Credit: https://stackoverflow.com/a/44158824
;;(setq lexical-binding t)

;; (let ((*outline-data* nil))
;; (defun org-save-outline-state (&optional arg type)
;;(setq *outline-data* (org-outline-overlay-data t)))

;; (defun org-restore-outline-state (&optional arg)
;; (when *outline-data*
;;(org-set-outline-overlay-data *outline-data*)
;;(setq *outline-data* nil))))

;;(advice-add 'org-sparse-tree :before 'org-save-outline-state)
;;(advice-add 'org-match-sparse-tree :before 'org-save-outline-state)
;;(advice-add 'org-ctrl-c-ctrl-c :after 'org-restore-outline-state)
;; ---

(setq org-time-stamp-custom-formats '("<%b-%d %a>" "<%m/%d/%y %a>" "<%m/%d/%y %a %H:%M>"))

;; Use bullets (default if uncommented)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(defun kill-misc-buffers() "Permanently remove some buffers."
;; (if (get-buffer "*scratch*")
;;  (kill-buffer "*scratch*"))
(if (get-buffer "*reg group-leader*")
    (kill-buffer "*reg group-leader*")))
(add-hook 'after-change-major-mode-hook 'kill-misc-buffers)

(evil-define-key 'insert org-mode-map (kbd "C-<tab>") #'tab-to-tab-stop)

;; Yup we want spell check to be turned on automatically in org mode
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; Load agenda files if they exist.
;; Credit: [StackOverflow] how-to-make-all-org-files-under-a-folder-added-in-agenda-list-automatically
(when (file-exists-p "~/.emacs.d/agenda")
  (load-library "find-lisp")
  (setq org-agenda-files (find-lisp-find-files "~/.emacs.d/agenda" "\.org$")))

;; do not ask before prompting
(setq org-confirm-babel-evaluate nil)

;; output htmlize as css
;; refer: https://github.com/gongzhitaao/orgcss
(setq org-html-htmlize-output-type 'css)

;; make EWW default browser for html files
(setq browse-url-browser-function 'eww-browse-url)

;;----------------------------------------------------------------------------
;; Miscalleanous settings
;; User mode settings for UI/keyboard/look and feel
;;----------------------------------------------------------------------------
(set-default 'truncate-lines t)
(setq browse-url-browser-function 'eww-browse-url)

(defun load-if-file-exists (FILE)
  "Check if FILE exists before loading it."
  (if (file-readable-p FILE)
      (load-file FILE)))

(load-if-file-exists "~/.emacs.d/lisp/secrets.el")
(load-if-file-exists "~/quicklisp/clhs-use-local.el")

;; wrap lines (hard return) around column 100
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook '(lambda() (set-fill-column 100)))

;; replaces Emacs undo-redo system with something MUCH nicer!
(global-undo-tree-mode t)
(setq undo-tree-auto-save-history t)

;; M-x slime calls sbcl
;;(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(setq slime-default-lisp 'sbcl)

(menu-bar-mode -1)
(require 'vimish-fold)

;; Using mouse to select and copy text to the clipboard
;; Source: [StackOverflow] how-to-combine-emacs-primary-clipboard-copy-and-paste-behavior-on-ms-windows
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(global-set-key [mouse-2] 'mouse-yank-at-click)
;;

;; Set default font
;; (used and saved through menu Options->Set Default Font... into cutom.el)

;; optionally (set-frame-font "Source Code Pro Semibold-10")
(cond
 ((member "Source Code Variable" (font-family-list))
  (set-frame-font "Source Code Variable-13:style=Semibold" nil t))
 ((member "Source Code Pro" (font-family-list))
  (set-frame-font "Source Code Pro-13:style=Semibold" nil t))
 ((member "DejaVu Sans Mono" (font-family-list))
  (set-frame-font "DejaVu Sans Mono-10")))

(setenv "PATH" (concat (getenv "PATH") ":~/bin"))
(setq exec-path (append exec-path '("~/bin")))
(setq-default major-mode 'org-mode)

(unless (version<= emacs-version "25")
  (require 'fill-column-indicator))

;;(benchmark-init/show-durations-tabulated)
;; show battery indicator on mode
(display-battery-mode t)

;; hide trailing whitespace in command output from showing up in eshell
(add-hook 'eshell-mode-hook
          (defun hide-trailing-whitespace ()
            (interactive)
            (setq show-trailing-whitespace nil)))

(add-hook 'Info-mode-hook
          (defun 0xMF/Info-mode-settings ()
            "Enable vi-style keybindings."
            (interactive)
            (turn-off-evil-mode)
            (local-set-key "b" 'Info-last)
            (local-set-key "h" 'Info-backward-node)
            (local-set-key "j" 'next-line)
            (local-set-key "k" 'previous-line)
            (local-set-key "/" 'isearch-forward)
            ;(local-set-key "l" 'Info-forward-node)
            ;(local-set-key "n" 'Info-forward-node)
            ;(local-set-key "p" 'Info-backward-node)
            (define-key Info-mode-map "n" 'Info-forward-node)
            (define-key Info-mode-map "l" 'Info-forward-node)
            (define-key Info-mode-map "p" 'Info-backward-node)))


(setq counsel-find-file-ignore-regexp (concat "\\(.~undo-tree~\\|"
                                              ".desktop\\|"
                                              ".git\\|"
                                              ".historian\\|"
                                              ".lock\\|"
                                              ".*.fasl\\|"
                                              ".*~\\|"
                                              "#*#\\)"))

(defun 0xMF/my-orgmode-settings ()
  "My Orgmode settings."
  ;; make org-mode default for scratch (new) buffers
  (setq initial-major-mode 'org-mode)
  (setq initial-scratch-message
        (concat "# Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")))

(defun 0xMF/kill-buffers (regexp)
  "Kill buffers matching REGEXP without confirmation."
  (interactive "sKill buffers matching the regex given: ")
  (cl-letf (((symbol-function 'kill-buffer-ask)
             (lambda (buffer) (kill-buffer buffer))))
    (kill-matching-buffers regexp)))

(defun cleanup-Emacs-buffer-list ()
  "Remove all kinds of needless buffers."
  (when (get-buffer "*Compile-Log*")
    (kill-buffer "*Compile-Log*"))
  (0xMF/kill-buffers "*Packages*")
  (0xMF/kill-buffers "^\\magit:")
  (0xMF/kill-buffers "*compilation*")
  (0xMF/kill-buffers "magit-diff:")
  (0xMF/kill-buffers "magit-merge-preview:")
  (0xMF/kill-buffers "*magit-todos--scan-with-git-grep")
  (0xMF/kill-buffers "popup-win-dummy")
  (0xMF/kill-buffers "^\\*vc-diff*")
  (0xMF/kill-buffers "^\\*Backtrace*")
  (0xMF/kill-buffers "^\\*Buffer List*")
  (0xMF/kill-buffers "^\\*Calculator*")
  (0xMF/kill-buffers "^\\*Command Line*")
  (0xMF/kill-buffers "^\\*Help*")
  (0xMF/kill-buffers "^\\*Org-Babel Error Output*")
  (0xMF/kill-buffers "^\\*PP Eval Output*")
  (0xMF/kill-buffers "^\\*Flycheck error messages*")
  (0xMF/my-orgmode-settings)
  (get-buffer-create "*scratch*"))

(defun 0xMF/startup ()
  "Start/reset Emacs the way I like it ;-)."
  (interactive)
  (cleanup-Emacs-buffer-list)
  (global-display-line-numbers-mode -1)
  (display-line-numbers-mode -1)
  (line-number-mode -1)
  (message "0xMF/startup"))

(add-hook 'after-init-hook '0xMF/startup)


(defun file-reload ()
  "Reload file without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

;; keep purcell's emacs.d settings happy
(provide 'init-local)

;;;  -*- mode: Lisp;-*
;;; init-local.el ends here
