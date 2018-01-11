;;; package -- custom.el

;;; Commentary:

;;  override some of my own settings

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (blueknight)))
 '(custom-safe-themes
   (quote
    ("aae169618087752b3d1933b845b7540984ed84a33a92d6e9fc6324a5e0d34575")))
 '(package-selected-packages
   (quote
    (uptimes evil-vimish-fold org-bullets use-package mediawiki powerline general evil flycheck-clojure cider elein cljsbuild-mode clojure-mode slime-company hippie-expand-slime slime cask-mode flycheck-package highlight-quoted macrostep cl-lib-highlight aggressive-indent immortal-scratch auto-compile ipretty elisp-slime-nav paredit-everywhere paredit company-terraform terraform-mode docker-compose-mode dockerfile-mode docker yaml-mode toml-mode racer rust-mode sql-indent projectile-rails yard-mode bundler goto-gem yari robe ruby-compilation inf-ruby rspec-mode ruby-hash-syntax psc-ide purescript-mode flycheck-elm elm-mode dhall-mode hindent intero haskell-mode company-anaconda anaconda-mode pip-requirements restclient httprepl css-eldoc skewer-less less-css-mode scss-mode sass-mode rainbow-mode tagedit tidy org-pomodoro writeroom-mode org-cliplink org-fstree smarty-mode php-mode add-node-modules-path skewer-mode js-comint prettier-js typescript-mode coffee-mode js2-mode json-mode erlang csv-nav csv-mode markdown-mode textile-mode cmd-to-echo alert magit-gh-pulls github-issues github-clone bug-reference-github yagist git-messenger magit git-timemachine gitconfig-mode gitignore-mode git-blamed vc-darcs darcsum browse-at-remote whitespace-cleanup-mode guide-key highlight-escape-sequences whole-line-or-region move-dup page-break-lines multiple-cursors avy expand-region browse-kill-ring symbol-overlay undo-tree rainbow-delimiters nlinum beacon mode-line-bell vlf dynamic-spaces list-unicode-display unfill mmm-mode default-text-scale session switch-window company-quickhelp company ivy-xref projectile counsel ivy-historian ivy smex flycheck-color-mode-line flycheck ibuffer-vc anzu diff-hl diredfl dired-sort disable-mouse dimmer color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized command-log-mode scratch diminish project-local-variables wgrep exec-path-from-shell fullframe)))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-code ((t (:background "#180248" :foreground "#ff8c00" :slant italic :weight bold))))
 '(org-document-title ((t (:background "#180248" :foreground "#c8c8c8" :weight bold :height 1.2))))
 '(org-hide ((t (:background "#180248" :foreground "#180248"))))
 '(org-level-1 ((t (:background "#180248" :foreground "#0080c0" :weight bold :height 1.15))))
 '(org-level-2 ((t (:background "#180248" :foreground "#ff6347" :weight bold :height 1.09))))
 '(org-level-3 ((t (:background "#180248" :foreground "#9fff90" :slant italic :weight bold :height 1.0))))
 '(org-level-4 ((t (:background "#180248" :foreground "#7fff00" :weight normal :height 1.0)))))
