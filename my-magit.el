;; Set the files that are searched for writing tokens
;; by default ~/.authinfo will be used
;; and write a token in unencrypted format
(setq auth-sources '("~/.authinfo.gpg"))

;; Configure number of topics show, open and closed
;; use negative number to toggle the view of closed topics
;; using `SPC SPC forge-toggle-closed-visibility'
(setq  forge-topic-list-limit '(100 . -10))

;; GitHub user and organization accounts owned
;; used by @ c f  to create a fork
(setq forge-owned-accounts
      '(("theophilusx")))

;; To blacklist specific accounts,
;; over-riding forge-owned-accounts
;; (setq forge-owned-blacklist
;;       '(("bad-hacks" "really-bad-hacks")))

;; diff-hl - diff hightlights in right gutter as you type
(diff-hl-flydiff-mode)

;; Use Spacemacs as the $EDITOR (or $GIT_EDITOR) for git commits messages
;; when using git commit on the command line
(global-git-commit-mode t)

;; Set locations of all your Git repositories
;; with a number to define how many sub-directories to search
;; `SPC g L' - list all Git repositories in the defined paths,
(setq magit-repository-directories
      '(("~/.emacs.d"  . 0)
        ("~/Projects/" . 2)
        ("~/git" . 2)))

(provide 'my-magit)
