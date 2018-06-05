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
   (quote ("c41f0d2f6fcf80d298fab81e52f615851c56502ece0656fa2542436264d3170d"
           "b877536cd0ac12cf6ceed5bc29b186caefa4b411f48da9f6c5f0c41cf2dd371e"
           "d37a1eff1929a1e43cfc0649a0c2abd9b73c9141bf844998bdae553cf8f526f0"
           "bea5410f1c106a2a27e350da4c5c857b3c1a45220985830569f981705bd971f6" default)))
 '(package-selected-packages
   (quote (benchmark-init evil fill-column-indicator general
                          mediawiki org-bullets powerline use-package vimish-fold)))

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
