;;; package --- 0xMF preferences for .emacs

;;; Commentary:

;;  - relies on purcell's .emacs.d to do all the heavy lifting
;;  - all else is kept to bare minimum.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;; grab all files in lisp and any sub-dirs inside it
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path (expand-file-name ".dependencies" user-emacs-directory))
(let ((default-directory  "~/.emacs.d/lisp/.dependencies"))
  (normal-top-level-add-subdirs-to-load-path))

;; silence ad-handle-redefinition warnings
(setq ad-redefinition-action 'accept)

;;
(setq package-archives '(
("gnu" . "https://elpa.gnu.org/packages/")
("melpa" . "https://melpa.org/packages/")))

;; my init.el is symlinked to purcell's emacs.d/init.el
(load "~/.emacs.d/init")

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'emacs)
;;; emacs.el ends here
