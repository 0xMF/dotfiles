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
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "119a9bc8d5a0051d7bdcfd72e6f4037c8207012086019806f1da491a926f9a4a" "eca86cec33b9da5f71eb23e8bba803f125420d76afa280c466f007ea7a4c2979" "c15efc857c82e8d3d1eb3c2570d78f0eaae347049c92b6dcb7b08b2f311644b0" "7c605bb846a2fd1252c86e50549294ccf71a1e81f58bc595731c321131a7e219" "c2d1ef5efc0a59769e33af6741083a77292536e39a3f485d5fdab61e57eb073b" "6a30a49339813df4b7a4fb51b219d3bcc36bdf54a5d823578c1e191e2be9b7e6" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "4639288d273cbd3dc880992e6032f9c817f17c4a91f00f3872009a099f5b3f84" default)))
 '(debug-on-error t)
 '(evil-shift-width 2)
 '(org-bullets-bullet-list (quote ("◉" "○" "✿" "✸")))
 '(package-selected-packages
   (quote
    (uptimes dotenv-mode daemons dsvn htmlize lua-mode gnuplot flycheck-ledger ledger-mode origami regex-tool info-colors flycheck-clojure cider elein cljsbuild-mode clojure-mode slime-company hippie-expand-slime slime cask-mode cl-libify flycheck-package highlight-quoted macrostep cl-lib-highlight aggressive-indent immortal-scratch auto-compile ipretty elisp-slime-nav paredit nginx-mode company-nixos-options nixos-options nix-buffer nix-sandbox nix-mode company-terraform terraform-mode docker-compose-mode dockerfile-mode docker yaml-mode toml-mode flycheck-rust racer rust-mode sqlformat projectile-rails yard-mode bundler goto-gem yari robe ruby-compilation inf-ruby rspec-mode ruby-hash-syntax psci psc-ide purescript-mode flycheck-elm elm-test-runner elm-mode dhall-mode reformatter dante haskell-mode company-anaconda anaconda-mode pip-requirements restclient httprepl haml-mode css-eldoc skewer-less sass-mode rainbow-mode tagedit org-pomodoro writeroom-mode org-cliplink company-php smarty-mode php-mode add-node-modules-path skewer-mode js-comint prettier-js typescript-mode coffee-mode js2-mode json-mode erlang csv-mode markdown-mode textile-mode cmd-to-echo alert ibuffer-projectile github-review forge github-clone bug-reference-github yagist git-commit magit-todos magit git-timemachine gitconfig-mode gitignore-mode git-blamed vc-darcs darcsum browse-at-remote whitespace-cleanup-mode which-key highlight-escape-sequences whole-line-or-region move-dup page-break-lines multiple-cursors avy expand-region browse-kill-ring symbol-overlay rainbow-delimiters goto-line-preview beacon mode-line-bell vlf list-unicode-display unfill mmm-mode session switch-window company-quickhelp company ivy-xref swiper projectile counsel ivy smex flycheck-color-mode-line flycheck ibuffer-vc wgrep anzu diff-hl diredfl disable-mouse default-text-scale dimmer color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized command-log-mode scratch diminish exec-path-from-shell gnu-elpa-keyring-update fullframe seq racket-mode paredit-everywhere hide-mode-line org-present yafolding guide-key ssh-agency org-gcal hindent intero sqlup-mode magithub evil-magit nov ivy-historian git-messenger ruby-mode evil-collection org-table-sticky-header cl-lib undo-tree auto-yasnippet org-beautify-theme vimish-fold use-package smart-mode-line-powerline-theme smart-mode-line powerline org-pdfview org-bullets mediawiki general fill-column-indicator evil benchmark-init)))
 '(pdf-view-midnight-colors (quote ("#0080c0" . "#180248")))
 '(safe-local-variable-values (quote ((flyspell-mode))))
 '(session-use-package t nil (session)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:foreground "Orange1" :background nil :box nil :weight bold))))
 '(org-block-begin-line ((t (:foreground "DarkOliveGreen" :background nil :weight semi-bold))))
 '(org-block-end-line ((t (:foreground "DarkOliveGreen" :background nil :weight semi-bold))))
 '(org-checkbox ((t (:background "#062451" :foreground "yellow" :box (:line-width -3 :color "#062451" :style "released-button")))))
 '(org-code ((t (:foreground "Orange1" :box nil :weight bold))))
 '(org-document-title ((t (:foreground "#c8c8c8" :weight bold :height 1.2))))
 '(org-hide ((t (:foreground "#180248"))))
 '(org-link ((t (:underline nil))))
 '(org-tag ((t (:foreground "#b8860b" :weight bold)))))

;;; custom.el ends here
