;; My configurations to override purcell's settings

(require 'evil)
(evil-mode 1)

(require 'solarized)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dakrone t) ; other nice themes: 'grandshell

;; keep purcell's emacs.d settings happy
(provide 'init-local)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
