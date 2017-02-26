;;; package --- My preferences for .emacs

;;; Commentary:

;;  - relies on purcell's .emacs.d to do all the heavy lifting
;;  - all else is kept to bare minimum.

;;; Code:

;; grab all files in lisp and any sub-dirs inside it
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; silence ad-handle-redefinition warnings
(setq ad-redefinition-action 'accept)

;; my init.el is symlinked to purcell's emacs.d/init.el
(load "~/.emacs.d/init")

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'emacs)
;;; emacs.el ends here
