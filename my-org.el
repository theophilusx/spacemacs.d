;; - create a new journal entry
(spacemacs/set-leader-keys "oj" 'org-journal-new-entry)

;; Markdown mode hook for orgtbl-mode minor mode
(add-hook 'markdown-mode-hook 'turn-on-orgtbl)

(with-eval-after-load 'org
  (org-element-update-syntax)
  (org-clock-persistence-insinuate))

(provide 'my-org)
