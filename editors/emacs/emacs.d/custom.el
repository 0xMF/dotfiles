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
    (add-node-modules-path aggressive-indent alert anaconda-mode anzu auto-compile avy beacon
                           browse-at-remote browse-kill-ring bug-reference-github bundler cask-mode cider
                           cl-lib-highlight cljsbuild-mode clojure-mode cmd-to-echo coffee-mode
                           color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow command-log-mode
                           company company-anaconda company-quickhelp company-terraform counsel css-eldoc
                           csv-mode csv-nav darcsum default-text-scale dhall-mode diff-hl
                           diminish dimmer dired-sort diredfl disable-mouse docker docker-compose-mode
                           dockerfile-mode dynamic-spaces elein elisp-slime-nav elm-mode erlang evil
                           evil-vimish-fold exec-path-from-shell expand-region flycheck flycheck-clojure
                           flycheck-color-mode-line flycheck-elm flycheck-package fullframe general git-blamed
                           git-messenger git-timemachine gitconfig-mode github-clone github-issues
                           gitignore-mode goto-gem guide-key haskell-mode highlight-escape-sequences
                           highlight-quoted hindent hippie-expand-slime httprepl ibuffer-vc immortal-scratch
                           inf-ruby intero ipretty ivy ivy-historian ivy-xref js-comint js2-mode json-mode
                           less-css-mode list-unicode-display macrostep magit magit-gh-pulls markdown-mode
                           mediawiki mmm-mode mode-line-bell move-dup multiple-cursors nlinum org-bullets
                           org-cliplink org-fstree org-pomodoro page-break-lines paredit paredit-everywhere
                           php-mode pip-requirements powerline prettier-js project-local-variables projectile
                           projectile-rails psc-ide purescript-mode racer rainbow-delimiters rainbow-mode
                           restclient robe rspec-mode ruby-compilation ruby-hash-syntax rust-mode sass-mode
                           scratch scss-mode session skewer-less skewer-mode slime slime-company smarty-mode
                           smex sql-indent switch-window symbol-overlay tagedit terraform-mode textile-mode
                           tidy toml-mode typescript-mode undo-tree unfill uptimes use-package vc-darcs vlf
                           wgrep whitespace-cleanup-mode whole-line-or-region writeroom-mode yagist yaml-mode
                           yard-mode yari)))

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

;;; custom.el ends here
