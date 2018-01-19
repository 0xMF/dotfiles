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
    ("1b5d82b1d1f46122368b55e0561eb42de38e4197c4bc491ee92ff2d98855cb65"
     "f0b0b52fc6f3296f4b53849b36acfd9fa9e90f6588464f7b449c2359fa5508de"
     "cd77c58935420d6700b5f7e93c89b715ae66e3584c75d547fca767b0f3024819")))

 '(package-selected-packages
<<<<<<< HEAD
   (quote (add-node-modules-path aggressive-indent alert
                                 anaconda-mode anzu auto-compile avy beacon browse-at-remote
                                 browse-kill-ring bug-reference-github bundler cask-mode cider
                                 cl-lib-highlight cljsbuild-mode clojure-mode cmd-to-echo
                                 coffee-mode color-theme-sanityinc-solarized
                                 color-theme-sanityinc-tomorrow command-log-mode company
                                 company-anaconda company-quickhelp company-terraform counsel
                                 css-eldoc csv-mode csv-nav darcsum default-text-scale
                                 dhall-mode diff-hl diminish dimmer dired-sort diredfl
                                 disable-mouse docker docker-compose-mode dockerfile-mode
                                 dynamic-spaces elein elisp-slime-nav elm-mode erlang evil
                                 evil-vimish-fold exec-path-from-shell expand-region flycheck
                                 flycheck-clojure flycheck-color-mode-line flycheck-elm
                                 flycheck-package fullframe general git-blamed git-messenger
                                 git-timemachine gitconfig-mode github-clone github-issues
                                 gitignore-mode goto-gem guide-key haskell-mode
                                 highlight-escape-sequences highlight-quoted hindent
                                 hippie-expand-slim e httprepl ibuffer-vc immortal-scratch
                                 inf-ruby intero ipretty ivy ivy-historian ivy-xref js-comint
                                 js2-mode json-mode less-css-mode list-unicode-display
                                 macrostep magit magit-gh-pulls markdown-mode mediawiki
                                 mmm-mode mode-line-bell move-dup multiple-cursors nlinum
                                 org-bullets org-cliplink org-fstree org-pomodoro
                                 page-break-lines paredit paredit-everywhere php-mode
                                 pip-requirements powerline prettier-js project-local-variables
                                 projectile projectile-rails psc-ide purescript-mode racer
                                 rainbow-delimiters rainbow-mode restclient robe rspec-mode
                                 ruby-compilation ruby-hash-syntax rust-mode sass-mode scratch
                                 scss-mode session skewer-less skewer-mode slime slime-company
                                 smarty-mode smex sql-indent switch-window symbol-overlay
                                 tagedit terraform-mode textile-mode tidy toml-mode
                                 typescript-mode undo-tree unfill uptimes use-package vc-darcs
                                 vlf wgrep whitespace-cleanup-mode whole-line-or-region
                                 writeroom-mode yagist yaml-mode yard-mode yari)))
=======
   (quote (add-node-modules-path aggressive-indent alert anaconda-mode anzu auto-compile
           avy beacon browse-at-remote browse-kill-ring bug-reference-github bundler cask-mode
           cider cl-lib-highlight cljsbuild-mode clojure-mode cmd-to-echo coffee-mode
           color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow command-log-mode company
           company-anaconda company-quickhelp company-terraform counsel css-eldoc csv-mode csv-nav
           darcsum default-text-scale dhall-mode diff-hl diminish dimmer dired-sort diredfl
           disable-mouse docker docker-compose-mode dockerfile-mode dynamic-spaces elein
           elisp-slime-nav elm-mode erlang evil evil-vimish-fold exec-path-from-shell
           expand-region fill-column-indicator flycheck flycheck-clojure flycheck-color-mode-line
           flycheck-elm flycheck-package fullframe general git-blamed git-messenger
           git-timemachine gitconfig-mode github-clone github-issues gitignore-mode goto-gem
           guide-key haskell-mode highlight-escape-sequences highlight-quoted hindent
           hippie-expand-slim e httprepl ibuffer-vc immortal-scratch inf-ruby intero ipretty ivy
           ivy-historian ivy-xref js-comint js2-mode json-mode less-css-mode list-unicode-display
           macrostep magit magit-gh-pulls markdown-mode mediawiki mmm-mode mode-line-bell move-dup
           multiple-cursors nlinum org-bullets org-cliplink org-fstree org-pomodoro
           page-break-lines paredit paredit-everywhere php-mode pip-requirements powerline
           prettier-js project-local-variables projectile projectile-rails psc-ide purescript-mode
           racer rainbow-delimiters rainbow-mode restclient robe rspec-mode ruby-compilation
           ruby-hash-syntax rust-mode sass-mode scratch scss-mode session skewer-less skewer-mode
           slime slime-company smarty-mode smex sql-indent switch-window symbol-overlay tagedit
           terraform-mode textile-mode tidy toml-mode typescript-mode undo-tree unfill uptimes
           use-package vc-darcs vlf wgrep whitespace-cleanup-mode whole-line-or-region
           writeroom-mode yagist yaml-mode yard-mode yari)))
>>>>>>> 7787302... reb

 '(session-use-package t nil (session)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-code ((t (:foreground "#ff8c00" :slant italic :weight bold))))
 '(org-document-title ((t (:foreground "#c8c8c8" :weight bold :height 1.2))))
 '(org-hide ((t (:foreground "#180248"))))
 '(org-level-1 ((t (:foreground "#0080c0" :weight bold :height 1.15))))
 '(org-level-2 ((t (:foreground "#ff6347" :weight bold :height 1.09))))
 '(org-level-3 ((t (:foreground "#9fff90" :slant italic :weight bold :height 1.0))))
 '(org-level-4 ((t (:foreground "#7fff00" :weight normal :height 1.0))))
 '(org-tag ((t (:foreground "#b8860b" :weight bold)))))

;;; custom.el ends here
