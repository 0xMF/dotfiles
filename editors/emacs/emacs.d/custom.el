;;; package -- custom.el

;;; Commentary:

;;  override some of my own settings

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(smart-mode-line-powerline))
 '(custom-safe-themes
   '("c253042dee36efffffe2e8a90d8c32c9f45facb7177c4e0ba6b8e5f696dca574" "97bce184accca1aec12e6f6ed62e4040e711396eec8849aed9f38d0c40eceb52" "440df10949e9ad8445e4d22be33ff4daec60607d6df169202c8141080ec2eacc" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "bea5410f1c106a2a27e350da4c5c857b3c1a45220985830569f981705bd971f6" "894258975fca040287301c8dd965e1337bed3ff8c5531b54921512837d1137b7" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default))
 '(package-selected-packages
   '(uptimes dotenv-mode daemons dsvn htmlize lua-mode gnuplot flycheck-ledger ledger-mode origami regex-tool flycheck-clojure cider elein cljsbuild-mode clojure-mode slime-company hippie-expand-slime slime cask-mode flycheck-package highlight-quoted macrostep cl-lib-highlight aggressive-indent immortal-scratch auto-compile ipretty elisp-slime-nav paredit-everywhere paredit nginx-mode company-terraform terraform-mode docker-compose-mode dockerfile-mode docker yaml-mode toml-mode flycheck-rust racer rust-mode projectile-rails yard-mode bundler goto-gem yari robe ruby-compilation inf-ruby rspec-mode ruby-hash-syntax ruby-mode psc-ide purescript-mode flycheck-elm elm-test-runner elm-mode dhall-mode hindent intero haskell-mode company-anaconda anaconda-mode pip-requirements restclient httprepl haml-mode css-eldoc skewer-less sass-mode rainbow-mode tagedit org-pomodoro writeroom-mode org-cliplink company-php smarty-mode php-mode add-node-modules-path skewer-mode js-comint prettier-js typescript-mode coffee-mode js2-mode json-mode erlang csv-mode markdown-mode textile-mode cmd-to-echo alert magithub github-clone bug-reference-github yagist git-messenger git-commit magit-todos magit git-timemachine gitconfig-mode gitignore-mode git-blamed vc-darcs darcsum browse-at-remote whitespace-cleanup-mode guide-key highlight-escape-sequences whole-line-or-region move-dup page-break-lines multiple-cursors avy expand-region browse-kill-ring symbol-overlay undo-tree rainbow-delimiters beacon mode-line-bell vlf list-unicode-display unfill mmm-mode default-text-scale session switch-window company-quickhelp company ivy-xref swiper projectile counsel ivy-historian ivy smex flycheck-color-mode-line flycheck ibuffer-vc anzu diff-hl diredfl disable-mouse dimmer color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized command-log-mode scratch diminish wgrep exec-path-from-shell cl-lib fullframe seq auto-yasnippet demo-it org-beautify-theme vimish-fold use-package smart-mode-line-powerline-theme smart-mode-line powerline org-pdfview org-bullets mediawiki general fill-column-indicator evil benchmark-init))
 '(pdf-view-midnight-colors '("#0080c0" . "#180248"))
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
 '(org-level-2 ((t (:foreground "#ff8822" :weight bold :height 1.09))))
 '(org-level-3 ((t (:foreground "#9fff90" :slant italic :weight bold :height 1.0))))
 '(org-level-4 ((t (:foreground "#7fff00" :weight normal :height 1.0))))
 '(org-tag ((t (:foreground "#b8860b" :weight bold)))))

;;; custom.el ends here
