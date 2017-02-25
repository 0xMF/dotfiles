;;----------------------------------------------------------------------------
;; bypass some purcell's emacs.d/init.el settings through provide stubs for
;;    settings of languages and platforms I do not presently want of use.
;;----------------------------------------------------------------------------

(provide 'init-osx-keys)
(provide 'init-textile)
(provide 'init-erlang)
(provide 'init-php)
(provide 'init-nxml)
(provide 'init-rails)
(unless (version<= emacs-version "24.2")
  (provide 'init-clojure)
  (provide 'init-clojure-cider))

;;; pre-load-local.el ends here
