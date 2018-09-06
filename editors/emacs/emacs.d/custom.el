;;; package -- custom.el

;;; Commentary:

;;  override some of my own settings

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote nil))
 '(custom-safe-themes
   (quote ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279"
           "c41f0d2f6fcf80d298fab81e52f615851c56502ece0656fa2542436264d3170d"
           "b877536cd0ac12cf6ceed5bc29b186caefa4b411f48da9f6c5f0c41cf2dd371e"
           "d37a1eff1929a1e43cfc0649a0c2abd9b73c9141bf844998bdae553cf8f526f0"
           "bea5410f1c106a2a27e350da4c5c857b3c1a45220985830569f981705bd971f6" default)))

 (quote (package-selected-packages '(add-node-modules-path
                                     aggressive-indent alert anaconda-mode anzu auto-compile avy
                                     beacon benchmark-init browse-at-remote browse-kill-ring
                                     bug-reference-github bundler cask-mode cider cl-lib
                                     cl-lib-highlight cljsbuild-mode clojure-mode cmd-to-echo
                                     coffee-mode color-theme-sanityinc-solarized
                                     color-theme-sanityinc-tomorrow command-log-mode company
                                     company-anaconda company-php company-quickhelp company-terraform
                                     counsel css-eldoc csv-mode daemons darcsum default-text-scale
                                     dhall-mode diff-hl diminish dimmer diredfl disable-mouse docker
                                     docker-compose-mode dockerfile-mode dotenv-mode dsvn elein
                                     elisp-slime-nav elm-mode erlang evil evil-org
                                     exec-path-from-shell expand-region fill-column-indicator flycheck
                                     flycheck-clojure flycheck-color-mode-line flycheck-elm
                                     flycheck-ledger flycheck-package flycheck-rust fullframe general
                                     git-blamed git-commit git-messenger git-timemachine
                                     gitconfig-mode github-clone gitignore-mode gnuplot goto-gem
                                     guide-key haml-mode haskell-mode highlight-escape-sequences
                                     highlight-quoted hindent hippie-expand-slime htmlize httprepl
                                     ibuffer-vc immortal-scratch inf-ruby intero ipretty ivy
                                     ivy-historian ivy-xref js-comint js2-mode json-mode ledger-mode
                                     list-unicode-display lua-mode macrostep magit magit-todos
                                     magithub markdown-mode mmm-mode mode-line-bell move-dup
                                     multiple-cursors nginx-mode org-bullets org-cliplink org-pdfview
                                     org-pomodoro origami ox-pandoc page-break-lines paredit
                                     paredit-everywhere php-mode pip-requirements powerline
                                     prettier-js projectile projectile-rails psc-ide purescript-mode
                                     racer rainbow-delimiters rainbow-mode regex-tool restclient robe
                                     rspec-mode ruby-compilation ruby-hash-syntax ruby-mode rust-mode
                                     sass-mode scratch seq session skewer-less skewer-mode slime
                                     slime-company smart-mode-line smart-mode-line-powerline-theme
                                     smarty-mode smex swiper switch-window symbol-overlay tagedit
                                     terraform-mode textile-mode toml-mode typescript-mode undo-tree
                                     unfill uptimes use-package vc-darcs vimish-fold vlf wgrep
                                     whitespace-cleanup-mode whole-line-or-region writeroom-mode
                                     yagist yaml-mode yard-mode yari)))
 '(pdf-view-midnight-colors (quote ("#0080c0" . "#180248")))
 '(session-use-package t nil (session)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-archived ((t (:foreground "#270372" :height 0.9))))
 '(org-code ((t (:foreground "#ff8c00" :slant italic :weight bold))))
 '(org-document-title ((t (:foreground "#c8c8c8" :weight bold :height 1.2))))
 '(org-hide ((t (:foreground "#180248"))))
 '(org-level-1 ((t (:foreground "#0080c0" :weight bold :height 1.15))))
 '(org-level-2 ((t (:foreground "#ff6347" :weight bold :height 1.09))))
 '(org-level-3 ((t (:foreground "#9fff90" :slant italic :weight bold :height 1.0))))
 '(org-level-4 ((t (:foreground "#7fff00" :weight normal :height 1.0))))
 '(org-tag ((t (:foreground "#b8860b" :weight bold)))))

;;; custom.el ends here
