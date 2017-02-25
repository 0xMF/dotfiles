;; My configurations to override purcell's settings

(require 'evil)
(evil-mode 1)

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
