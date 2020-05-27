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
;; evil, evil-collection, and evil-magit should play nice with each other
(setq evil-want-keybinding 'nil)

;;
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; my init.el is symlinked to purcell's emacs.d/init.el
(load "~/.emacs.d/init")

(quote (package-selected-packages '(add-node-modules-path
                                    aggressive-indent alert anaconda-mode anzu auto-compile auto-yasnippet avy
                                    beacon benchmark-init browse-at-remote browse-kill-ring bug-reference-github
                                    bundler cask-mode cider cl-lib cl-lib-highlight cl-libify cljsbuild-mode
                                    clojure-mode cmd-to-echo coffee-mode color-theme-sanityinc-solarized
                                    color-theme-sanityinc-tomorrow command-log-mode company company-anaconda
                                    company-nixos-options company-php company-quickhelp company-terraform counsel
                                    crontab-mode css-eldoc csv-mode daemons dante darcsum default-text-scale
                                    dhall-mode diff-hl diminish dimmer diredfl disable-mouse docker docker-compose-mode
                                    dockerfile-mode dotenv-mode dsvn elein elisp-slime-nav elm-mode elm-test-runner
                                    erlang evil evil-collection evil-magit exec-path-from-shell expand-region
                                    fill-column-indicator flycheck flycheck-clojure flycheck-color-mode-line
                                    flycheck-elm flycheck-ledger flycheck-package flycheck-relint flycheck-rust
                                    forge fullframe geiser general git-blamed git-commit git-messenger git-timemachine
                                    gitconfig-mode github-clone github-review gitignore-mode gnu-elpa-keyring-update
                                    gnuplot go-mode goto-gem goto-line-preview guide-key haml-mode haskell-mode
                                    hide-mode-line highlight-escape-sequences highlight-quoted hindent
                                    hippie-expand-slime htmlize httprepl ibuffer-projectile ibuffer-vc immortal-scratch
                                    inf-ruby info-colors intero ipretty ivy ivy-historian ivy-rich ivy-xref js-comint
                                    js2-mode json-mode ledger-mode list-unicode-display lua-mode macrostep magit magit-todos
                                    magithub markdown-mode mediawiki mmm-mode mode-line-bell move-dup multiple-cursors
                                    nginx-mode nix-buffer nix-mode nix-sandbox nixos-options nov org org-beautify-theme
                                    org-bullets org-cliplink org-gcal org-noter-pdftools org-pdftools-noter org-pdfview
                                    org-pomodoro org-present org-static-blog org-table-sticky-header origami page-break-lines
                                    paredit paredit-everywhere php-mode pip-requirements powerline prettier-js projectile
                                    projectile-rails psc-ide psci purescript-mode racer racket-mode rainbow-delimiters
                                    rainbow-mode reformatter regex-tool restclient robe rspec-mode ruby-compilation
                                    ruby-hash-syntax ruby-mode rust-mode sass-mode scratch seq session shfmt skewer-less
                                    skewer-mode slime slime-company smart-mode-line smart-mode-line-powerline-theme
                                    smarty-mode smex sqlformat sqlup-mode ssh-agency sudo-edit swiper switch-window
                                    symbol-overlay tagedit terraform-mode textile-mode toml-mode typescript-mode undo-tree
                                    unfill uptimes use-package vc-darcs vimish-fold vlf wgrep which-key whitespace-cleanup-mode
                                    whole-line-or-region writeroom-mode yafolding yagist yaml-mode yard-mode yari)))


;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'emacs)
;;; emacs.el ends here
