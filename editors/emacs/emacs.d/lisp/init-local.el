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
                                  fill-column-indicator general go-mode hide-mode-line
                                  org-beautify-theme org-bullets org-caldav org-gcal
                                  org-noter-pdftools org-pdftools org-present org-static-blog
                                  powerline racket-mode smart-mode-line
                                  smart-mode-line-powerline-theme ssh-agency use-package
                                  yafolding))

(dolist (package my-required-packages)
  (unless (package-installed-p package)
    (package-install package)))

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
(require 'general)

(defun my-default-cursor()
  "Cursor color indicates mode: white = Emacs, green = evil (Vi/Vim)."
  (if (string= (symbol-value 'evil-state) "normal")
      (set-cursor-color "green")
      (set-cursor-color "white")))

(evil-mode 1)
(evil-collection-init)
(evil-magit-init)

(setq evil-default-state-cursor '("green" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-replace-state-cursor '("red" box))
(setq evil-operator-state-cursor '("red" hollow))

;; better clipboard copy-paste with evil
(fset 'evil-visual-update-x-selection 'ignore)


(add-hook 'evil-mode-hook 'my-default-cursor)

;;----------------------------------------------------------------------------
;; General keymap settings
;;----------------------------------------------------------------------------

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
                    "d" '(lambda () (interactive) (kill-buffer) (unless (one-window-p)(delete-window)))
                    "f" '0xMF/toggle-font-large-normal
                    "|" 'fci-mode
                    "h" 'previous-buffer
                    "j" 'next-buffer
                    "k" 'kill-this-buffer
                    "l" 'previous-buffer
                    "n" 'next-buffer
                    "N" 'other-window
                    "t" 'whitespace-mode
                    "o" 'other-window
                    "O" 'org-open-at-point
                    "p" 'previous-buffer
                    ;; "P" 'other-window
                    "r" 'evil-window-rotate-upwards
                    "R" 'evil-window-rotate-downwards
                    "u" 'winner-undo
                    "U" 'winner-redo
                    "w" 'delete-other-windows
                    "0" 'delete-window
                    "1" 'delete-window)
(general-define-key :prefix "b"
                    "a" 'describe-bindings
                    "b" 'evil-scroll-page-up
                    "c" 'yank
                    "d" 'org-time-stamp-inactive
                    "D" 'org-time-stamp
                    "f" 'markdown-follow-thing-at-point
                    "h" 'previous-buffer
                    "j" 'next-buffer
                    "l" 'list-buffers
                    "n" 'next-buffer
                    "o" 'org-open-at-point
                    "p" 'previous-buffer
                    "r" '0xMF/reset
                    "t" '(lambda () (interactive) (kill-buffer)(delete-window))
                    "T" 'sanityinc/toggle-delete-other-windows
                    "x" 'evil-delete)
(general-define-key :prefix "z"
                    "b" 'paredit-forward-barf-sexp
                    "B" 'paredit-backward-barf-sexp
                    "d" #'yafolding-toggle-all
                    "f" #'yafolding-toggle-element
                    "g" 'save-this-word
                    "o" 'org-open-at-point
                    "s" 'paredit-forward-slurp-sexp
                    "S" 'paredit-backward-slurp-sexp
                    "x" '0xMF/orgmode-remove-tag
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
                    "f" 'set-fill-column
                    "F" 'file-reload
                    "g" 'magit-status
                    "i" '0xMF/Info-mode-settings
                    "k" 'kill-this-buffer
                    "l" 'whitespace-mode
                    "m" 'magit-mode
                    "L" 'org-open-at-point
                    "n" 'display-line-numbers-mode
                    "o" 'find-file
                    "O" 'org-open-at-point
                    "p" '0xMF/my-theme-settings
                    "P" '0xMF/start-slideshow ;;'org-present
                    "q" 'fill-paragraph
                    "r" '0xMF/reset
                    "R" 'file-reload ;;'undo-tree-redo
                    "s" '0xMF/startup
                    "u" 'undo-tree-undo
                    "v" '0xMF/vi
                    "t" '0xMF/my-theme-settings
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

(defun 0xMF/my-vi-settings ()
  "My Vi-settings."
  ;; jump j/k always even in visual mode
  (turn-on-evil-mode)
  (dolist (map  (list evil-normal-state-map))
    (define-key map (kbd "j") 'evil-next-visual-line)
    (define-key map (kbd "k") 'evil-previous-visual-line)
    (define-key map (kbd "p") 'evil-paste-after)
    (define-key map (kbd "q") 'keyboard-quit)
    (define-key map [escape] 'keyboard-quit)
    (define-key map [escape] 'keyboard-quit)
    (define-key map [prior] 'evil-scroll-page-up)
    (define-key map [next] 'evil-scroll-page-down)
    (define-key map (kbd "C-a") 'mark-whole-buffer)
    (define-key map (kbd "C-j") (lambda () (interactive) (evil-scroll-down nil)))
    (define-key map (kbd "C-d") 'save-buffer)
    (define-key map (kbd "C-n") 'next-buffer)
    (define-key map (kbd "C-p") 'previous-buffer))

  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  (dolist (map  (list minibuffer-local-isearch-map))
    (define-key map (kbd "n") 'isearch-printing-char))

  (global-set-key [escape] 'evil-exit-emacs-state)

  (define-key evil-normal-state-map (kbd "C-k") (lambda () (interactive) (evil-scroll-up nil)))
  ;;(global-set-key (kbd "S-SPC") 'evil-scroll-page-up)
  (global-set-key [?\S- ] 'evil-scroll-page-up))

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
In Delete Selection mode, if the mark is active,just deactivate
it; then it takes a second \\[keyboard-quit] to abort the
minibuffer."
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
(eval-after-load "org" '(require 'org-pdftools))
(eval-after-load "org" '(require 'htmlize))

(pdf-tools-install)
(use-package org-pdftools
  :hook (org-load . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
(add-to-list 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)

(defun my-eww-settings ()
  "Enable vi-style keybindings."
  (dolist (map  (list eww-mode-map))
    (define-key map (kbd "h") 'previous-char)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "l") 'next-char)
    (define-key map (kbd "n") 'eww-forward-url)
    (define-key map (kbd "p") 'eww-back-url)))
(add-hook 'eww-mode-hook 'my-eww-settings)

(defun my-pdf-view-settings ()
  "Disable blinking in pdf-view-mode and enable vi-style keybindings."
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (setq pdf-view-continuous 't)
              (set (make-local-variable 'evil-emacs-state-cursor) (list nil))
              (local-set-key (kbd  "j") 'pdf-view-next-line-or-next-page)
              (local-set-key (kbd "k") 'pdf-view-previous-line-or-previous-page)
              (local-set-key (kbd "n") 'pdf-view-scroll-up-or-next-page)
              (local-set-key (kbd "p") 'pdf-view-scroll-down-or-previous-page)
              (local-set-key (kbd "J") 'pdf-view-next-page)
              (local-set-key (kbd "K") 'pdf-view-previous-page)
              (local-set-key (kbd "g") 'pdf-view-first-page)
              (local-set-key (kbd "G") 'pdf-view-last-page)
              (local-set-key (kbd "l") 'pdf-view-goto-page)
              (local-set-key (kbd "/") 'isearch-forward)
              (local-set-key (kbd "?") 'isearch-backward)
              (local-set-key (kbd "<mouse-5>") 'pdf-view-next-line-or-next-page)
              (local-set-key (kbd "<mouse-4>") 'pdf-view-previous-line-or-previous-page))))
(add-hook 'pdf-view-mode-hook 'my-pdf-view-settings)

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
(add-hook 'markdown-mode-hook (lambda () (set-fill-column 99)))

(unless (version< emacs-version "27")
  (setq url-http-referer 'nil))

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

;;(global-set-key (kbd "C-s") 'save-buffer)
;;(global-set-key (kbd "C-d") 'save-buffer)
;;(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") '0xMF/my-insert-braces)
(global-set-key (kbd "M-\"") 'insert-pair)

;;----------------------------------------------------------------------------
;; Org mode settings
;;----------------------------------------------------------------------------
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(setq org-hide-leading-stars t)
(setq org-indent-mode-turns-off-org-adapt-indentation nil)
(setq org-indent-mode-turns-on-hiding-stars nil)
(setq org-pretty-entities t)
(setq org-export-with-section-numbers nil)
(setq org-table-auto-blank-field nil)
(setq org-startup-indented nil)
(setq org-startup-folded 'content)
(setq org-list-allow-alphabetical t)


(setq org-time-stamp-custom-formats '("<%b-%d %a>" "<%m/%d/%y %a>" "<%m/%d/%y %a %H:%M>"))

;; Use bullets (default if uncommented)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-tempo)

(defun kill-misc-buffers()
  "Permanently remove some buffers."
  ;; (if (get-buffer "*scratch*")
  ;;  (kill-buffer "*scratch*"))
  (if (get-buffer "*reg group-leader*")
      (kill-buffer "*reg group-leader*")))
(add-hook 'after-change-major-mode-hook 'kill-misc-buffers)

(evil-define-key 'insert org-mode-map (kbd "C-<tab>") #'tab-to-tab-stop)

;; Yup we want spell check to be turned on automatically in org mode and text wrap at 99.
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook '(lambda() (set-fill-column 99)))

;; do not ask before prompting
(setq org-confirm-babel-evaluate nil)

;; output htmlize as css
;; refer: https://github.com/gongzhitaao/orgcss
(setq org-html-htmlize-output-type 'css)


;;----------------------------------------------------------------------------
;; Miscalleanous settings
;; User mode settings for UI/keyboard/look and feel
;;----------------------------------------------------------------------------
(require 'org-gcal)
(require 'yafolding)

(add-hook 'prog-mode-hook
          (lambda () (yafolding-mode)))

(set-default 'truncate-lines t)

;; Do not ceate backups.
(setq  make-backup-files nil)

(defun load-if-file-exists (FILE)
  "Check if FILE exists before loading it."
  (if (file-readable-p FILE)
      (load-file FILE)))

(load-if-file-exists "~/.emacs.d/lisp/secrets.el")
(load-if-file-exists "~/quicklisp/clhs-use-local.el")

;; wrap lines (hard return) around column 99
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook '(lambda() (set-fill-column 99)))

;; replaces Emacs undo-redo system with something MUCH nicer!
(global-undo-tree-mode t)
(setq undo-tree-auto-save-history t)

;; M-x slime calls sbcl
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(require 'slime-autoloads)
(setq inferior-lisp-program "sbcl")
(setq slime-default-lisp 'sbcl)
(setq slime-contribs '(slime-scratch slime-editing-commands slime-fancy))
(add-hook 'lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)))
(put 'lambda 'lisp-indent-function 'defun)
(put 'while 'lisp-indent-function 1)
(put 'unless 'lisp-indent-function 1)
(put 'if 'lisp-indent-function nil)
(put 'do 'lisp-indent-function 2)
(put 'do* 'lisp-indent-function 2)

(menu-bar-mode -1)

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
 ((member "Source Code Pro" (font-family-list))
  (set-frame-font "Source Code Pro-13:style=Semibold" nil t))
 ((member "Source Code Variable" (font-family-list))
  (set-frame-font "Source Code Variable-13:style=Semibold" nil t))
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
            (local-set-key (kbd "j") 'next-line)
            (local-set-key (kbd "k") 'previous-line)
            (dolist (map  (list Info-mode-map))
              (define-key map (kbd "n") 'Info-forward-node)
              (define-key map (kbd "p") 'Info-backward-node)
              (define-key map (kbd "m") 'Info-menu))
            (message "0xMF/Info-mode-settings")))

(defun hide-mode-line-toggle ()
  "Toggle mode line toggle."
  (interactive)
  (hide-mode-line-mode (if hide-mode-line-mode -1 +1))
  (unless hide-mode-line-mode
    (redraw-display)))

;; Helpful links:
;;  https://emacs.stackexchange.com/questions/7748/why-cant-i-use-a-variable-when-defining-the-color-to-draw-a-box-with
;;  https://ftp.gnu.org/old-gnu/Manuals/elisp-manual-21-2.8/html_node/elisp_634.html
(defvar 0xMF-current-theme "dark" "Value of current theme: dark mode or light.")
(defun 0xMF/my-theme-settings ()
  "Bring sanity back to my current theme after changing themes."
  (interactive)
  (load-theme 'org-beautify)
  (org-toggle-pretty-entities)
  (let ((bg (face-background 'default)))
    (if (string= 0xMF-current-theme "dark")
        (progn
          (set-face-attribute 'org-checkbox nil :inherit 'default :background bg :foreground "NavyBlue" :box `(:line-width -3 :color ,bg :style "released-button"))
          (custom-set-faces '(org-macro ((t (:foreground "DarkOliveGreen" :bold t)))))
          (setq 0xMF-current-theme "light"))
        (progn
          (set-face-attribute 'org-checkbox nil :inherit 'default :background bg :foreground "Yellow" :box `(:line-width -3 :color ,bg :style "released-button"))
          (custom-set-faces '(org-macro ((t (:foreground "burlywood")))))
          (setq 0xMF-current-theme "dark"))
        (mapcar #'(lambda (f) (set-face-background f bg)
                    (set-border-color bg))
                '(org-checkbox org-macro org-hide))))
  (message "changed to %s mode" 0xMF-current-theme))

(defun 0xMF/ivy-minibuffer-settings ()
  "Bring sanity back to up/down keybindings."
  (interactive)
  (dolist (map  (list ivy-minibuffer-map))
    (define-key map [up] 'ivy-previous-line)))
(add-hook 'ivy-minibuffer-hook '0xMF/ivy-minibuffer-settings)
(add-hook 'ivy-mode-hook '0xMF/ivy-minibuffer-settings)

(require 'org-present)
(require 'hide-mode-line)
(autoload 'org-present "org-present" nil t)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (turn-off-evil-mode)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (local-set-key (kbd "n") 'org-present-next)
                 (local-set-key (kbd [space]) 'org-present-next)
                 (local-set-key (kbd "p") 'org-present-prev)
                 (local-set-key (kbd "q") 'org-present-quit)
                 (local-set-key (kbd "<") 'org-present-beginning)
                 (local-set-key (kbd "G") 'org-present-end)
                 (local-set-key (kbd ">") 'org-present-end)
                 (dolist (map  (list org-present-mode-map))
                   (define-key map (kbd "gg")  'org-present-beginning)
                   (define-key map [backspace] 'org-present-prev)
                   (define-key map [?\S- ]  'org-present-prev)
                   (define-key map [up]     'org-present-prev)
                   (define-key map [down]   'org-present-next))
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (local-unset-key (kbd "n"))
                 (local-unset-key (kbd "p"))
                 (local-unset-key (kbd "G"))
                 (turn-on-evil-mode)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (hide-mode-line-mode -1)
                 (org-present-read-write)))))

(add-hook 'org-present-mode-hook #'hide-mode-line-mode)

(setq counsel-find-file-ignore-regexp (concat "\\(.~undo-tree~\\|"
                                              ".desktop\\|"
                                              ".git\\|"
                                              ".historian\\|"
                                              ".lock\\|"
                                              ".*.fasl\\|"
                                              ".*~\\|"
                                              "#*#\\)"))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(defun 0xMF/toggle-browser-eww ()
  "Toggle between eww and chromium as default browser for html files."
  (interactive)
  (setq browse-url-browser-function
        (if (equal browse-url-browser-function 'eww-browse-url)
            'browse-url-chromium
            'eww-browse-url))
  (message "setting browser to '%s'" browse-url-browser-function))

(setq 0xMF/toggle-font-large-normal nil)
(defun 0xMF/toggle-font-large-normal ()
  "Toggle font sizes between large/normal."
  (interactive)
  (if (get '0xMF/toggle-font-large-normal 'state)
      (progn
        (set-frame-font "Source Code Pro-13:style=Semibold" nil t)
        (put '0xMF/toggle-font-large-normal 'state nil))
      (progn
        (set-frame-font "Source Code Pro-15:style=Semibold" nil t)
        (put '0xMF/toggle-font-large-normal 'state t)))
  (message 0xMF/toggle-font-large-normal))

(defun 0xMF/normal-font ()
  "Increase font size."
  (interactive)
  (when (member "Source Code Pro" (font-family-list))
    (set-frame-font "Source Code Pro-13:style=Semibold" nil t)))

(defun 0xMF/my-orgmode-settings ()
  "My Orgmode settings."
  ;; make org-mode default for scratch (new) buffers
  (setq initial-major-mode 'org-mode)
  (setq initial-scratch-message
        (concat "# Happy hacking, " user-login-name " - Emacs ♥ you!\n\n")))

(defun 0xMF/orgmode-remove-tag (tag)
  "Remove TAG from line."
  (interactive "sTag:")
  (org-toggle-tag tag 'off))

(defun 0xMF/my-insert-braces ()
  "Source: stackoverflow.com/questions/2951797/wrapping-selecting-text-in-enclosing-characters-in-emacs."
  (interactive)
  (if (region-active-p)
      (insert-pair 1 ?{ ?})
    (insert "{}")
    (backward-char)))

(defun 0xMF/kill-some-buffers (regexp)
  "Kill buffers matching REGEXP without confirmation."
  (interactive "Kill buffers matching the regex given: ")
  (cl-letf (((symbol-function 'kill-buffer-ask)
             (lambda (buffer) (kill-buffer buffer))))
    (kill-matching-buffers regexp)))

(defvar 0xMF/kill-all-magit t "Removes all magit-buffers (inucluding magit process).")
(defun cleanup-Emacs-buffer-list ()
  "Remove all kinds of needless buffers."
  (when (get-buffer "*Compile-Log*")
    (kill-buffer "*Compile-Log*"))
  (0xMF/kill-some-buffers "^\\*Calculator*")
  (0xMF/kill-some-buffers "^\\*Packages*")
  (0xMF/kill-some-buffers "^\\*compilation*")
  (when (bound-and-true-p 0xMF/kill-all-magit)
    (0xMF/kill-some-buffers "^magit:")
    (0xMF/kill-some-buffers "^magit-diff:")
    (0xMF/kill-some-buffers "^magit-merge-preview:")
    (0xMF/kill-some-buffers "^magit-process:")
    (0xMF/kill-some-buffers "^\\*magit-todos--scan-with-git-grep"))
  (0xMF/kill-some-buffers "^popup-win-dummy")
  (0xMF/kill-some-buffers "^\\*vc-diff*")
  (0xMF/kill-some-buffers "^\\*Backtrace*")
  (0xMF/kill-some-buffers "^\\*Buffer List*")
  (0xMF/kill-some-buffers "^\\*Calculator*")
  (0xMF/kill-some-buffers "^\\*Command Line*")
  (0xMF/kill-some-buffers "^\\*Help*")
  (0xMF/kill-some-buffers "^\\*List of Slides*")
  (0xMF/kill-some-buffers "^\\*Org-Babel Error Output*")
  (0xMF/kill-some-buffers "^\\*PP Eval Output*")
  (0xMF/kill-some-buffers "^\\*Flycheck error messages*")
  (0xMF/my-orgmode-settings)
  (when (fboundp '0xMF/local)
    (0xMF/local))
  (get-buffer-create "*scratch*"))

(defun 0xMF/startup ()
  "Start/reset Emacs the way like it ;-)."
  (interactive)
  (cleanup-Emacs-buffer-list)
  (global-display-line-numbers-mode -1)
  (display-line-numbers-mode -1)
  (line-number-mode t)
  (org-toggle-pretty-entities)
  (org-toggle-pretty-entities)
  (when (equal major-mode 'org-mode)
    (org-set-visibility-according-to-property)
    (setq electric-pair-mode nil))
  (when (equal major-mode 'Info-mode)
    (0xMF/Info-mode-settings))
  (when (fboundp 'ivy-minibuffer-map)
    (0xMF/ivy-minibuffer-settings))
  (when (fboundp '0xMF/local)
    (0xMF/local))
  (0xMF/my-vi-settings)
  (message "0xMF/startup"))

(defun 0xMF/wrap ()
  "Toggle line wrapping."
  (interactive)
  (toggle-truncate-lines))

(defun 0xMF/vi ()
  "Reset/set settings to vim."
  (interactive)
  (turn-on-evil-mode)
  (toggle-truncate-lines))

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
