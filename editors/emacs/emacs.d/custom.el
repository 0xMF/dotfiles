;;; package -- custom.el

;;; Commentary:

;;  override some of my own settings

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote ()))
 '(custom-safe-themes
   (quote ("b877536cd0ac12cf6ceed5bc29b186caefa4b411f48da9f6c5f0c41cf2dd371e"
           "bea5410f1c106a2a27e350da4c5c857b3c1a45220985830569f981705bd971f6"
           "19e548f0fb3baf8d19c2d8ad6ec79998782dfc8fb266cad34ff10bf94ef79a42" default)))

 '(package-selected-packages
   (quote (hippie-expand-slime nginx-mode magithub yari yard-mode
                               yagist writeroom-mode whole-line-or-region
                               whitespace-cleanup-mode wgrep vlf vc-darcs use-package uptimes
                               unfill typescript-mode toml-mode textile-mode tagedit
                               symbol-overlay switch-window sql-indent smex smarty-mode
                               slime-company skewer-less session scss-mode scratch sass-mode
                               ruby-hash-syntax ruby-compilation rspec-mode robe restclient
                               rainbow-mode rainbow-delimiters racer purescript-mode psc-ide
                               projectile-rails prettier-js powerline pip-requirements
                               php-mode paredit-everywhere page-break-lines org-pomodoro
                               org-fstree org-cliplink org-bullets nlinum multiple-cursors
                               move-dup mode-line-bell mmm-mode mediawiki markdown-mode
                               magit-gh-pulls list-unicode-display less-css-mode js-comint
                               ivy-xref ivy-historian ipretty intero immortal-scratch
                               ibuffer-vc httprepl hindent highlight-quoted
                               highlight-escape-sequences guide-key goto-gem gitignore-mode
                               github-issues github-clone gitconfig-mode git-timemachine
                               git-messenger git-blamed general fullframe flycheck-package
                               flycheck-elm flycheck-color-mode-line flycheck-clojure
                               fill-column-indicator expand-region exec-path-from-shell
                               evil-vimish-fold erlang elm-mode elisp-slime-nav elein
                               dynamic-spaces dockerfile-mode docker-compose-mode docker
                               disable-mouse diredfl dimmer diminish diff-hl dhall-mode
                               default-text-scale darcsum csv-mode css-eldoc counsel
                               company-quickhelp company-anaconda command-log-mode
                               coffee-mode cmd-to-echo cljsbuild-mode cl-lib-highlight
                               cask-mode bundler bug-reference-github browse-kill-ring
                               browse-at-remote beacon avy auto-compile anzu
                               aggressive-indent add-node-modules-path)))

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
