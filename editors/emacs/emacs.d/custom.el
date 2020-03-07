;;; package -- custom.el

;;; Commentary:

;;  override some of my own settings

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (smart-mode-line-powerline org-beautify)))
 '(custom-safe-themes
   (quote
    ("61fe01878beefa051fe15c4cae1b03c5fadcd74ab74d5ccf58f5207c7144fa83" "eca86cec33b9da5f71eb23e8bba803f125420d76afa280c466f007ea7a4c2979" "10efa2432f01053505d36a08819b7b41aa34be5a521a4263f2d41b2209d00a28" "a71588763f11b430191769cb4460ef1e50a9ed895a6c1f2b53651410878c0340" "7c605bb846a2fd1252c86e50549294ccf71a1e81f58bc595731c321131a7e219" "fb4c2600dd2c174d4419f4c64149fae1459d9edee167e848bde02e0d6837a6b5" "6a30a49339813df4b7a4fb51b219d3bcc36bdf54a5d823578c1e191e2be9b7e6" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "4639288d273cbd3dc880992e6032f9c817f17c4a91f00f3872009a099f5b3f84" default)))
 '(debug-on-error t)
 '(evil-shift-width 2)
 '(org-bullets-bullet-list (quote ("◉" "○" "✿" "✸")))
 '(package-selected-packages
   (quote
    (uptimes dotenv-mode daemons dsvn htmlize lua-mode gnuplot flycheck-ledger ledger-mode origami regex-tool info-colors flycheck-clojure cider elein cljsbuild-mode clojure-mode slime-company hippie-expand-slime slime cask-mode flycheck-relint cl-libify flycheck-package highlight-quoted macrostep cl-lib-highlight aggressive-indent immortal-scratch auto-compile ipretty elisp-slime-nav paredit nginx-mode company-nixos-options nixos-options nix-buffer nix-sandbox nix-mode company-terraform terraform-mode docker-compose-mode dockerfile-mode docker yaml-mode toml-mode flycheck-rust racer rust-mode sqlformat projectile-rails yard-mode bundler goto-gem yari robe ruby-compilation inf-ruby rspec-mode ruby-hash-syntax psci psc-ide purescript-mode flycheck-elm elm-test-runner elm-mode dhall-mode reformatter dante haskell-mode company-anaconda anaconda-mode pip-requirements restclient httprepl haml-mode css-eldoc skewer-less sass-mode rainbow-mode tagedit org-pomodoro writeroom-mode org-cliplink company-php smarty-mode php-mode add-node-modules-path skewer-mode js-comint prettier-js typescript-mode coffee-mode js2-mode json-mode erlang csv-mode markdown-mode textile-mode crontab-mode cmd-to-echo alert ibuffer-projectile github-review forge github-clone bug-reference-github yagist git-commit magit-todos magit git-timemachine gitconfig-mode gitignore-mode git-blamed vc-darcs darcsum browse-at-remote whitespace-cleanup-mode which-key highlight-escape-sequences whole-line-or-region move-dup page-break-lines multiple-cursors avy expand-region browse-kill-ring symbol-overlay rainbow-delimiters goto-line-preview beacon mode-line-bell vlf list-unicode-display unfill mmm-mode session switch-window company-quickhelp company ivy-xref swiper projectile counsel ivy smex flycheck-color-mode-line flycheck ibuffer-vc wgrep anzu diff-hl diredfl disable-mouse default-text-scale dimmer color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized command-log-mode scratch diminish exec-path-from-shell gnu-elpa-keyring-update fullframe seq go-mode geiser racket-mode paredit-everywhere hide-mode-line org-present yafolding guide-key ssh-agency org-gcal hindent intero sqlup-mode magithub evil-magit nov ivy-historian git-messenger ruby-mode evil-collection org-table-sticky-header cl-lib undo-tree auto-yasnippet org-beautify-theme vimish-fold use-package smart-mode-line-powerline-theme smart-mode-line powerline org-pdfview org-bullets mediawiki general fill-column-indicator evil benchmark-init)))
 '(pdf-view-midnight-colors (quote ("#0080c0" . "#180248")))
 '(safe-local-variable-values (quote ((flyspell-mode . 0) (flyspell-mode))))
 '(session-use-package t nil (session)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:foreground "Orange1" :background nil :box nil :weight bold))))
 '(org-block-begin-line ((t (:foreground "DarkOliveGreen" :background nil :weight semi-bold))))
 '(org-block-end-line ((t (:foreground "DarkOliveGreen" :background nil :weight semi-bold))))
 '(org-checkbox ((t (:background "#180248" :foreground "yellow" :box (:line-width -3 :color "#180248" :style "released-button")))))
 '(org-code ((t (:foreground "Orange1" :box nil :weight bold))))
 '(org-document-title ((t (:foreground "#c8c8c8" :weight bold :height 1.2))))
 '(org-hide ((t (:foreground "#180248"))))
 '(org-link ((t (:underline nil))))
 '(org-tag ((t (:foreground "#b8860b" :weight bold)))))

;;; custom.el ends here
