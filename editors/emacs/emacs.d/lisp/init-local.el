;; My configurations to override purcell's settings

(require 'evil)
(evil-mode 1)

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(global-evil-tabs-mode t)

;; allows ; to be used a mapper for my keybindings
(global-evil-leader-mode)
(evil-leader/set-leader ";")
(setq evil-leader/in-all-states 1)

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

;; yes to powerline
(require 'powerline)
(display-time-mode t)

(require 'solarized)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dakrone t) ; other nice themes: 'grandshell

;; Set default font -- copied from custom.el
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight semi-bold :height 90 :width normal)))))

;; C-h-b: to check keybinding and which functions are bound to which keys
;; C-h-k: to check which key is bound to which function
;; C-h-m: to list current major mode's keys
;; C-g:   to close that opened Bindings window
;; checkout: http://ergoemacs.org/emacs/keyboard_shortcuts.html
(global-set-key (kbd "C-;") ctl-x-map)
(global-set-key (kbd "C-M-;") 'execute-extended-command)

;; keep purcell's emacs.d settings happy
(provide 'init-local)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;;
;;; init-local.el ends here
