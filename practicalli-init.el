;; custom theme modification
;; - overriding default height of modeline
(setq-default
 theming-modifications
 '((spacemacs-light
    (mode-line :height 0.92)
    (mode-line-inactive :height 0.92))
   (doom-solarized-light
    (mode-line :height 0.92)
    (mode-line-inactive :height 0.92))
   (doom-gruvbox-light
    (mode-line :height 0.80)
    (mode-line-inactive :height 0.92))))

(provide 'practicalli-init)
