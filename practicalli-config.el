  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Emacs text rendering optimizations
  ;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html

  ;; Only render text left to right
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; Disable Bidirectional Parentheses Algorithm
  (if (version<= "27.1" emacs-version)
      (setq bidi-inhibit-bpa t))

  ;; Files with known long lines
  ;; SPC f l to open files literally to disable most text processing

  ;; So long mode when Emacs thinks a file would affect performance
  (if (version<= "27.1" emacs-version)
      (global-so-long-mode 1))

  ;; End of: Emacs text rendering optimizations
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Doom theme settings
  ;; (setq doom-gruvbox-light-variant "hard")
  ;;
  ;; (defun practicalli/setup-custom-doom-modeline ()
  ;;   (doom-modeline-set-modeline 'practicalli-modeline 'default))
  ;;
  ;; (with-eval-after-load 'doom-modeline
  ;;   (doom-modeline-def-modeline 'practicalli-modeline
  ;;     '(workspace-name window-number modals persp-name buffer-info remote-host vcs)
  ;;     '(repl debug lsp process matches checker buffer-position word-count parrot selection-info misc-info))
  ;;   (practicalli/setup-custom-doom-modeline))
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; User key bindings
  ;;
  ;; org-journal user keybinding
  ;; - create a new journal entry
  (spacemacs/set-leader-keys "oj" 'org-journal-new-entry)
  ;;
  ;; Toggle workspaces forward/backwards
  (spacemacs/set-leader-keys "ow" 'eyebrowse-next-window-config)
  (spacemacs/set-leader-keys "oW" 'eyebrowse-last-window-config)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Over-ride Spacemacs defaults
  ;;
  ;;
  ;; Set new location for file bookmarks, SPC f b
  ;; Default: ~/.emacs.d/.cache/bookmarks
  (setq bookmark-default-file "~/.spacemacs.d/bookmarks")
  ;;
  ;;
  ;; Set new location for recent save files
  ;; Default: ~/.emacs.d/.cache/recentf
;; (setq bookmark-default-file "~/.spacemacs.d/recentf")
(setq recentf-save-file "~/.spacemacs.d/recentf")
  ;;
  ;;
  ;; native line numbers taking up lots of space?
  (setq-default display-line-numbers-width nil)
  ;;
  ;;
  ;; replace / search with helm-swoop in Evil normal state
  (evil-global-set-key 'normal "/" 'helm-swoop)
  ;;
  ;;
  ;; Do not highlight trailing whitespace
  ;; - whitespace deleted on save using: dotspacemacs-whitespace-cleanup 'all
  (setq spacemacs-show-trailing-whitespace nil)
  ;;
  ;;
  ;; Open ranger with the minus keybinding - not working
  ;; Currently opens with deer
  ;; (setq ranger-enter-with-minus t)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Magit - forge configuration
  ;;
  ;; Set the files that are searched for writing tokens
  ;; by default ~/.authinfo will be used
  ;; and write a token in unencrypted format
  (setq auth-sources '("~/.authinfo.gpg"))
  ;;
  ;; Configure number of topics show, open and closed
  ;; use negative number to toggle the view of closed topics
  ;; using `SPC SPC forge-toggle-closed-visibility'
  (setq  forge-topic-list-limit '(100 . -10))
  ;; set closed to 0 to never show closed issues
  ;; (setq  forge-topic-list-limit '(100 . 0))
  ;;
  ;; GitHub user and organization accounts owned
  ;; used by @ c f  to create a fork
  (setq forge-owned-accounts
        '(("practicalli" "jr0cket"
           "ClojureBridgeLondon" "ldnclj"
           "clojure-hacks"
           "reclojure")))
  ;; To blacklist specific accounts,
  ;; over-riding forge-owned-accounts
  ;; (setq forge-owned-blacklist
  ;;       '(("bad-hacks" "really-bad-hacks")))
  ;;
  ;; End of Magit - forge configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Safe structural editing
  ;; for all major modes
  (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)
  ;; for clojure layer only (comment out line above)
  ;; (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hook-clojure-mode)
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Version Control configuration - Git, etc
  ;;
  ;; diff-hl - diff hightlights in right gutter as you type
  (diff-hl-flydiff-mode)
  ;;
  ;; Load in magithub features after magit package has loaded
  ;; (use-package magithub
  ;;   :after magit
  ;;   :config (magithub-feature-autoinject t))
  ;;
  ;; Use Spacemacs as the $EDITOR (or $GIT_EDITOR) for git commits messages
  ;; when using git commit on the command line
  (global-git-commit-mode t)
  ;;
  ;; Set locations of all your Git repositories
  ;; with a number to define how many sub-directories to search
  ;; `SPC g L' - list all Git repositories in the defined paths,
  (setq magit-repository-directories
        '(("~/.emacs.d"  . 0)
          ("~/Projects/" . 2)))
  ;;
  ;; end of version control configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org-mode configuration
  ;;
  ;; I should write a toggle function to show descriptive or literate links in Org-mode
  ;;(setq org-descriptive-links nil)
  ;;
  ;; Org-reveal - define were reveal.js files can be found
  ;; (I place reveal.js files in same directory as I write the org files)
  (setq org-reveal-root "")
  ;;
  ;; Define the location of the file to hold tasks
  (with-eval-after-load 'org
    (setq org-default-notes-file "~/Dropbox/todo-list.org"))
  ;;
  ;; Define a kanban style set of stages for todo tasks
  (with-eval-after-load 'org
    (setq org-todo-keywords
         '((sequence "TODO" "DOING" "BLOCKED" "REVIEW" "|" "DONE" "ARCHIVED"))))
  ;;
  ;; The default keywords all use the same colour.
  ;; Make the states easier to distinguish by using different colours
  ;; Using X11 colour names from: https://en.wikipedia.org/wiki/Web_colors
  ;; Setting colours (faces) using the `org-todo-keyword-faces' defcustom function
  ;; https://github.com/tkf/org-mode/blob/master/lisp/org-faces.el#L376
  ;; Using `with-eval-after-load' as a hook to call this setting when org-mode is run
  ;;
  (with-eval-after-load 'org
    (setq org-todo-keyword-faces
          '(("TODO" . "SlateGray")
            ("DOING" . "DarkOrchid")
            ("BLOCKED" . "Firebrick")
            ("REVIEW" . "Teal")
            ("DONE" . "ForestGreen")
            ("ARCHIVED" .  "SlateBlue"))))
  ;;
  ;;
  ;; Set TODO keyword faces if over-ridden by theme.
  (defun practicalli/set-todo-keyword-faces ()
    (interactive)
    (setq hl-todo-keyword-faces
          '(("TODO" . "SlateGray")
            ("DOING" . "DarkOrchid")
            ("BLOCKED" . "Firebrick")
            ("REVIEW" . "Teal")
            ("DONE" . "ForestGreen")
            ("ARCHIVED" .  "SlateBlue"))))
  ;;
  ;;
  ;; Progress Logging
  ;; When a TODO item enters DONE, add a CLOSED: property with current date-time stamp
  (with-eval-after-load 'org
    (setq org-log-done 'time))
  ;;
  ;;
  ;; customize org-mode's checkboxes with unicode symbols
  (add-hook
   'org-mode-hook
   (lambda ()
     "Beautify Org Checkbox Symbol"
     (push '("[ ]" . "☐") prettify-symbols-alist)
     (push '("[X]" . "☑" ) prettify-symbols-alist)
     (push '("[-]" . "❍" ) prettify-symbols-alist)
     (prettify-symbols-mode)))
  ;;
  ;; Markdown mode hook for orgtbl-mode minor mode
  (add-hook 'markdown-mode-hook 'turn-on-orgtbl)
  ;;
  ;; Turn on visual-line-mode for Org-mode only
  ;; (add-hook 'org-mode-hook 'turn-on-visual-line-mode)
  ;;
  ;; use org-re-reveal instead of org-reveal (which hasnt been updated in ages and breaks org-mode 9.2)
  ;; (use-package org-re-reveal :after org)
  ;;
  ;; End of Org-mode Configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Clojure configurations
  ;;
  ;;
  ;; CIDER 0.23 Lima release options
  ;; Configure the position of evaluation result
  ;; By default the result displays at the end of the current line
  ;; Set cider-result-overlay-position to `at-point' to display results right after the expression evaluated
  ;; Useful for evaluating nexsted expressions with `, e e'
  (setq cider-result-overlay-position 'at-point)
  ;;
  ;;
  ;; Pretty print in Clojure to use the Fast Idiomatic Pretty-Printer. This is approximately 5-10x faster than clojure.core/pprint
  (setq cider-pprint-fn 'fipp)
  ;;
  ;;
  ;; Indentation of function forms
  ;; https://github.com/clojure-emacs/clojure-mode#indentation-of-function-forms
  (setq clojure-indent-style 'align-arguments)
  ;;
  ;; Vertically align s-expressions
  ;; https://github.com/clojure-emacs/clojure-mode#vertical-alignment
  (setq clojure-align-forms-automatically t)
  ;;
  ;; Auto-indent code automatically
  ;; https://emacsredux.com/blog/2016/02/07/auto-indent-your-code-with-aggressive-indent-mode/
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  ;;
  ;;
  ;; Local Clojure and Java sources
  ;; Extract the clojure-x.x.x-sources.jar and Java src.zip files
  ;; Extracted files enable use of search tools (ripgrep, ag).
  ;; https://docs.cider.mx/cider/config/basic_config.html#_use_a_local_copy_of_the_java_source_code
  ;; (setq cider-jdk-src-paths '("~/projects/java/clojure-1.10.1-sources"
  ;;                             "~/projects/java/openjdk-11/src"))
  ;;
  ;;
  ;; anakondo - static analysis using clj-kondo
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; https://github.com/didibus/anakondo
  ;; Provides auto-completion without the need for a REPL
  ;; Add anakondo to `dotspacemacs-additional-packages` list
  ;;
  ;; `SPC SPC anakondo-minor-mode' to run manually for the current project.
  ;;
  ;; Commented until static analysis is an optional or background process
  ;; https://github.com/didibus/anakondo/issues/1
  ;;
  ;; Lazy load of anakondo until Clojure buffer is used
  ;; (autoload 'anakondo-minor-mode "anakondo")
  ;;
  ;; Enable anakondo-minor-mode in all Clojure buffers
  ;; (add-hook 'clojure-mode-hook #'anakondo-minor-mode)
  ;; Enable anakondo-minor-mode in all ClojureScript buffers
  ;; (add-hook 'clojurescript-mode-hook #'anakondo-minor-mode)
  ;; Enable anakondo-minor-mode in all cljc buffers
  ;; (add-hook 'clojurec-mode-hook #'anakondo-minor-mode)
  ;;
  ;;
  ;;
  ;; LSP server for Clojure with clj-kondo
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; An alternative approach to the Clojure layer variable clojure-enable-linters 'clj-kondo
  ;; for those environments where the clj-kondo binary does not run (eg. graal).
  ;; Uses a custom script to run the clj-kondo-lsp-server.jar which should be added
  ;; to the operating system path and include:
  ;; java -jar ~/path/to/clj-kondo-lsp-server-standalone.jar
  ;; (use-package lsp-mode
  ;;   :ensure t
  ;;   :hook ((clojure-mode . lsp))
  ;;   :commands lsp
  ;;   :custom ((lsp-clojure-server-command '("clojure-lsp-server-clj-kondo")))
  ;;   :config (dolist  (m '(clojure-mode clojurescript-mode))
  ;;             (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))
  ;;
  ;;
  ;; TODO: review this binding - gives poor user experience
  ;; Multi-line editing in the REPL buffer
  ;; `RTN` creates a new line, `C-RTN` evaluates the code
  ;; Multi-line editing in the REPL buffer
  ;; (add-hook 'cider-repl-mode-hook
  ;;           '(lambda ()
  ;;              (define-key cider-repl-mode-map (kbd "RET") #'cider-repl-newline-and-indent)
  ;;              (define-key cider-repl-mode-map (kbd "C-<return>") #'cider-repl-return)))
  ;;
  ;;
  ;; TODO: review this binding
  ;; repl history keybindings - not used - use s-<up> and s-<down> which are the defaults
  ;; (add-hook 'cider-repl-mode-hook
  ;;           '(lambda ()
  ;;              (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
  ;;              (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)))
  ;;
  ;;
  ;; hook for command-line-mode - shows keybindings & commands in separate buffer
  ;; load command-line-mode when opening a clojure file
  ;; (add-hook 'clojure-mode-hook 'command-log-mode)
  ;;
  ;; turn on command-log-mode when opening a source code or text file
  ;; (add-hook 'prog-mode-hook 'command-log-mode)
  ;; (add-hook 'text-mode-hook 'command-log-mode)
  ;;
  ;; toggle reader macro sexp comment
  ;; toggles the #_ characters at the start of an expression
  (defun clojure-toggle-reader-comment-sexp ()
    (interactive)
    (let* ((point-pos1 (point)))
      (evil-insert-line 0)
      (let* ((point-pos2 (point))
             (cmtstr "#_")
             (cmtstr-len (length cmtstr))
             (line-start (buffer-substring-no-properties point-pos2 (+ point-pos2 cmtstr-len)))
             (point-movement (if (string= cmtstr line-start) -2 2))
             (ending-point-pos (+ point-pos1 point-movement 1)))
        (if (string= cmtstr line-start)
            (delete-char cmtstr-len)
          (insert cmtstr))
        (goto-char ending-point-pos)))
    (evil-normal-state))
  ;;
  ;; Assign keybinding to the toggle-reader-comment-sexp function
  (define-key global-map (kbd "C-#") 'clojure-toggle-reader-comment-sexp)
  ;;
  ;; Evaluate code when it is contained in a (comment (,,,))
  ;; 24th sept - didnt work, even after updating spacemacs and packages
  ;; (setq cider-eval-toplevel-inside-comment-form t)
  ;;
  ;; (add-hook 'clojure-mode-hook
  ;;           '(setq cider-eval-toplevel-inside-comment-form t))
  ;;
  ;;
  ;; Toggle view of a clojure `(comment ,,,) block'
  ;;
  (defun clojure-hack/toggle-comment-block (arg)
    "Close all top level (comment) forms. With universal arg, open all."
    (interactive "P")
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^(comment\\>" nil 'noerror)
        (call-interactively
         (if arg 'evil-open-fold
           'evil-close-fold)))))
  ;;
  (evil-define-key 'normal clojure-mode-map
    "zC" 'clojure-hack/toggle-comment-block
    "zO" (lambda () (interactive) (clojure-hack/toggle-comment-block 'open)))
  ;;
  ;;
  ;; Experiment: Start Clojure REPL with a specific profile
  ;; https://stackoverflow.com/questions/18304271/how-do-i-choose-switch-leiningen-profiles-with-emacs-nrepl
  ;;
  ;; (defun start-cider-repl-with-profile ()
  ;;   (interactive)
  ;;   (letrec ((profile (read-string "Enter profile name: "))
  ;;            (lein-params (concat "with-profile +" profile " repl :headless")))
  ;;     (message "lein-params set to: %s" lein-params)
  ;;     (set-variable 'cider-lein-parameters lein-params)
  ;;     (cider-jack-in)))
  ;;
  ;; My altered more idiomatic version, hopefully
  ;; - seems to be a bug...
  ;; (defun start-cider-repl-with-profile (profile)
  ;;   (interactive "sEnter profile name: ")
  ;;   (letrec ((lein-params (concat "with-profile +" profile " repl :headless")))
  ;;     (message "lein-params set to: %s" lein-params)
  ;;     (set-variable 'cider-lein-parameters lein-params)
  ;;     (cider-jack-in)))
  ;;
  ;;
  ;; Hook for command-log-mode
  ;; shows keybindings & commands in separate buffer
  ;; Load command-log-mode when opening a clojure file
  ;; (add-hook 'clojure-mode-hook 'command-log-mode)
  ;;
  ;; Turn on command-log-mode when opening a source code or text file
  ;; (add-hook 'prog-mode-hook 'command-log-mode)
  ;; (add-hook 'text-mode-hook 'command-log-mode)
  ;;
  ;;
  ;; end of clojure configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Web-mode configuration
  ;;
  ;; Changing auto indent size for languages in html layer (web mode) to 2 (defaults to 4)
  (defun web-mode-indent-2-hook ()
    "Indent settings for languages in Web mode, markup=html, css=css, code=javascript/php/etc."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset  2)
    (setq web-mode-code-indent-offset 2))
  ;;
  (add-hook 'web-mode-hook  'web-mode-indent-2-hook)
  ;;
  ;; End of Web-mode configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Eshell visual enhancements
  ;;
  ;; Add git status visual labels
  ;;
  (require 'dash)
  (require 's)
  ;;
  (defmacro with-face (STR &rest PROPS)
    "Return STR propertized with PROPS."
    `(propertize ,STR 'face (list ,@PROPS)))
  ;;
  (defmacro esh-section (NAME ICON FORM &rest PROPS)
    "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
    `(setq ,NAME
           (lambda () (when ,FORM
                        (-> ,ICON
                            (concat esh-section-delim ,FORM)
                            (with-face ,@PROPS))))))
  ;;
  (defun esh-acc (acc x)
    "Accumulator for evaluating and concatenating esh-sections."
    (--if-let (funcall x)
        (if (s-blank? acc)
            it
          (concat acc esh-sep it))
      acc))
  ;;
  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat esh-header
            (-reduce-from 'esh-acc "" eshell-funcs)
            "\n"
            eshell-prompt-string))
  ;;
  ;;
  ;; Looking for unicode icons on Emacs
  ;; `list-character-sets' and select unicode-bmp
  ;; scroll through bitmaps list to find the one you want
  ;; some bitmaps seem to change
  ;;
  ;; "\x26A5 "  (female-male symbol)
  ;; "\xf394"   (non-binary)
  ;; "\xf105"     (docker - changes)
  ;; "\xf105"   (leiningen - changes)
  ;; "\xe919"   (clojure logo - ??)
  ;; "\xf104"   (clojurescript logo - changes)
  ;; "\xf09b"   (github octocat)
  ;; "\xf397"  (git branch)
  ;; "\xf126"    (was git fork, changes..)
  ;; "\xf1d3"  ;  (git icon - changes)
  ;; "\xf5b0"   (git merge)
  ;; "\xf07b" 
  ;; "\xf114"   (closed folder - changes)
  ;; "\xf115"   (open folder - changes)
  ;; "\xf074" 
  ;; "\xe97c" 
  ;; "\xe943"  
  ;; "\xe566"  
  ;; "\xe422"  
  ;; "\xe907"  ; 
  ;; "\xe91b"  ;  
  ;; "\xf126"    (was git fork, changes..)
  ;; "\xf1d3"  ;  (git icon - changes)
  ;;
  ;;
  (esh-section esh-dir
               "\xf07c"  ;  (faicon folder)
               (abbreviate-file-name (eshell/pwd))
               '(:foreground "olive" :bold bold :underline t))
  ;;
  (esh-section esh-git
               "\xf397"  ;  (git branch icon)
               (magit-get-current-branch)
               '(:foreground "maroon"))
  ;;
  ;; (esh-section esh-python
  ;;              "\xe928"  ;  (python icon)
  ;;              pyvenv-virtual-env-name)
  ;;
  (esh-section esh-clock
               ""  ;  (clock icon)
               (format-time-string "%H:%M" (current-time))
               '(:foreground "forest green"))
  ;;
  ;; Below I implement a "prompt number" section
  (setq esh-prompt-num 0)
  (add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
  (advice-add 'eshell-send-input :before
              (lambda (&rest args) (setq esh-prompt-num (incf esh-prompt-num))))
  ;;
  ;;
  ;; "\xf0c9"  ;  (list icon)
  (esh-section esh-num
               "\x2130"  ;  ℰ (eshell icon)
               (number-to-string esh-prompt-num)
               '(:foreground "brown"))
  ;;
  ;; Separator between esh-sections
  (setq esh-sep " ")  ; or " | "
  ;;
  ;; Separator between an esh-section icon and form
  (setq esh-section-delim "")
  ;;
  ;; Eshell prompt header
  (setq esh-header "\n ")  ; or "\n┌─"
  ;;
  ;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
  ;; your login, these can be the same.
  (setq eshell-prompt-regexp " \x2130 ")   ; or "└─> "
  (setq eshell-prompt-string " \x2130 ")   ; or "└─> "
  ;;
  ;; Choose which eshell-funcs to enable
  ;; (setq eshell-funcs (list esh-dir esh-git esh-python esh-clock esh-num))
  ;; (setq eshell-funcs (list esh-dir esh-git esh-clock esh-num))
  (setq eshell-funcs (list esh-dir esh-git))

  ;; Enable the new eshell prompt
  (setq eshell-prompt-function 'esh-prompt-func)

  ;; End of Eshell
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Shell configuration
  ;;
  ;; Use zsh for default multi-term shell
  ;; (setq multi-term-program "/usr/bin/zsh")
  ;;
  ;; End of Shell configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; MacOSX
  ;; Disable touchpad zoom gestures
  ;;
  ;; (define-key global-map (kbd "<magnify-up>") nil)
  ;; (define-key global-map (kbd "<magnify-down>") nil)
  ;;
  ;; (defun practicalli-nothing ()
  ;;   (interactive)
  ;;   (message "Buttons are not toys") )
  ;;
  ;; (define-key global-map (kbd "<magnify-up>") 'practicalli-nothing)
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  ;; Hacking spacebind
  ;; TODO: How to over-ride name of spacebind defined key?
  ;; The following does not work
  ;; (spacemacs/set-leader-keys "b C-d" 'spacemacs/kill-matching-buffers-rudely "Hackme")
  ;; (spacemacs/set-leader-keys "b C-d" 'spacemacs/kill-matching-buffers-rudely "Hackme")

  ;; (spacemacs|spacebind
  ;;  "Encrypt / decrypt files with Easy PG"
  ;;  :global
  ;;  (("b" "Buffers"
  ;;    ("C-d" spacemacs/kill-matching-buffers-rudely "Rudely"))))

;; (spacemacs|spacebind
;;  "Compare buffers, files and directories."
;;  :global
;;  (("TAB" spacemacs/alternate-buffer "Last buffer")
;;   ("b" "Buffers"
;;    ("C-e" spacemacs/kill-matching-buffers-rudely "Kill rudely..."))))


  ;; (spacemacs|spacebind
  ;;  "Encrypt / decrypt files with Easy PG"
  ;;  :global
  ;;  (("a" "applications"
  ;;    ("g"  "easy pg"
  ;;     ("d" epa-decrypt-file "Decrypt file to...")
  ;;     ("D" epa-delete-keys  "Delete keys...")
  ;;     ("e" epa-encrypt-file "Encrypt file...")
  ;;     ("i" epa-insert-keys  "Insert keys...")
  ;;     ("k" epa-list-keys "List keys...")
  ;;     ("K" epa-list-secret-keys "List secret keys...")
  ;;     ("x" epa-export-keys "Export keys...")
  ;;     ("s"  "sign"
  ;;      ("f" epa-sign-file "Sign file...")
  ;;      ("m" epa-sign-mail "Sign mail...")
  ;;      ("r" epa-sign-region "Sign region..."))
  ;;     ("v"  "verify"
  ;;      ("f" epa-verify-file "Verify file...")
  ;;      ("r" epa-verify-region "Verify region...")
  ;;      ("c" epa-verify-cleartext-in-region "Verify cleartext region..."))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; (add-hook 'persp-mode-hook
  ;;           (lambda ()
  ;;             (persp-load-state-from-file (expand-file-name "~/.emacs.d/.cache/layouts/persp-my-layout"))))
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Configuration no longer used
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Workarounds and bug fixes - temporary hopefully
  ;;
  ;; Undo history size limit, triggering garbage collection
  ;; Updating all defaults by a power of 10 (adding another zero at the end)
  ;; default in spacemacs is 80000
  ;; (setq undo-limit 400000)
  ;;
  ;; default in spacemacs is 120000
  ;; (setq undo-strong-limit 6000000)
  ;;
  ;; default in spacemacs is 12000000
  ;; (setq undo-strong-limit 60000000)
  ;;
  ;;
  ;; disable undo-tree as it seems to be loosing history
  ;; (global-undo-tree-mode -1)
  ;;
  ;; TODO: try explicitly saving history
  ;; (setq undo-tree-auto-save-history t)
  ;;
  ;; TODO: try setting undo-tree tmp files location
  ;; (setq undo-tree-history-directory-alist '(("." . "~/var/emacs/undo")))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Which-key now using a different sorting order for keybindings
  ;; which-key-sort-order 'which-key-prefix-then-key-order
  ;; https://github.com/syl20bnr/spacemacs/commit/ab3511cfb55aadaa7a13be03356917cca3071c02
  ;; (setq which-key-sort-order 'which-key-key-order-alpha)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Spell checking
  ;; merged into Spacemacs `develop'
  ;;
  ;; Add keybinding to correct current word under the cursor
  ;; to the existing spelling menu, `S'
  ;; (spacemacs/set-leader-keys "Ss" 'flyspell-correct-at-point)
  ;;
  ;; Or in the user-binding menu
  ;; (spacemacs/set-leader-keys "os" 'flyspell-correct-at-point)
  ;;
  ;; Documentation:
  ;; http://develop.spacemacs.org/doc/DOCUMENTATION.html#binding-keys
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Redundant configurations - Clojure
  ;;
  ;; disable the new enhanced ClojureScript code completion
  ;; Use the ClojureScript completion from earlier versions of CIDER if enhanced cljs completion is causing issues
  ;;
  ;; (setq cider-enhanced-cljs-completion-p nil)
  ;;
  ;; End of CIDER 0.23 Lima release options
  ;;
  ;;
  ;; In clojure-mode, treat hyphenated words as a single word.
  ;; Using `w' in Evil normal words gets stuck on names containing `->'
  ;; (add-hook 'clojure-mode-hook #'(lambda () (modify-syntax-entry ?- "w")))
  ;; (add-hook 'clojure-mode-hook #'(lambda ()
  ;;                                  (dolist (c (string-to-list "-_>?:"))
  ;;                                    (modify-syntax-entry c "w"))))
  ;;
  ;; Alternative approach - using subword-mode
  ;;
  ;; Enabling CamelCase support for editing commands
  ;; https://cider.readthedocs.io/en/latest/additional_packages/#subword-mode
  ;; (add-hook 'cider-repl-mode-hook #'subword-mode)
  ;;
  ;;
 ;;
  ;;
  ;;
  ;; Linting with clj-kondo
  ;; https://github.com/borkdude/clj-kondo/blob/master/doc/editor-integration.md#spacemacs
  ;;
  ;; Using clj-kondo by itself
  ;; (use-package clojure-mode
  ;;   :ensure t
  ;;   :config
  ;;   (require 'flycheck-clj-kondo))

  ;; Using clj-kondo with joker
  ;; (use-package clojure-mode
  ;;   :ensure t
  ;;   :config
  ;;   (require 'flycheck-joker)
  ;;   (require 'flycheck-clj-kondo)
  ;;   (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  ;;     (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))
  ;;   (dolist (checkers '((clj-kondo-clj . clojure-joker)
  ;;                       (clj-kondo-cljs . clojurescript-joker)
  ;;                       (clj-kondo-cljc . clojure-joker)
  ;;                       (clj-kondo-edn . edn-joker)))
  ;;     (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers)))))


  ;; TODO: Spacemacs pull request with these keybindings, updating REPL intro text with details
  ;; You can remove this message with the <M-x cider-repl-clear-help-banner> command.
  ;; You can disable it from appearing on start by setting
  ;; ‘cider-repl-display-help-banner’ to nil.
  ;; Cannot set the banner to another text, its hard coded in CIDER code.
  ;; (setq cider-repl-display-help-banner "Evaluate in the source code buffer for fun and profit!")


  ;; Merged into develop
  ;; (spacemacs/set-leader-keys-for-major-mode 'clojure "ei" 'cider-interrupt)

  ;; Experiment: Turn on all font locking options for Clojure
  ;; (setq cider-font-lock-dynamically t)
  ;;
  ;; configure clojurescript-jack-in to use the helper functions provided by lein-figwheel template
  ;; https://github.com/bhauman/lein-figwheel
  ;; fig-start will start figwheel and compile the clojurescript application
  ;; cljs-repl will connect emacs buffer to clojurescript repl created by figwheel
  ;;
  ;;
  ;; TODO: review this binding - CIDER takes care of this now
  ;; without this configuration, emacs command clojurescript-jack-in defaults to jvm rhino repl
  ;; if using a different clojurescript template you may require different function calls in the do expression
  ;; alternatively: set via m-x customize-variable cider-cljs-lein-repl
  ;; TODO: Is cider-cljs-lein-repl still required to be set?
  ;; Or is this just specific to those projects that have a user/fig-start and user/cljs-repl functions
  ;; (setq cider-cljs-lein-repl
  ;;      "(do
  ;;         (user/fig-start)
  ;;         (user/cljs-repl))")
  ;;
  ;; if you are not using figwheel template to configure funcitons in dev/core.clj
  ;; then use the full function calls
  ;; (setq cider-cljs-lein-repl
  ;;       "(do (require 'figwheel-sidecar.repl-api)
  ;;          (figwheel-sidecar.repl-api/start-figwheel!)
  ;;          (figwheel-sidecar.repl-api/cljs-repl))")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; unwanted features / bug workarounds
  ;;
  ;; opening recent files on spacemacs home page with mouse click
  ;; pastes contents of kill ring once file is open
  ;;
  ;; (define-key spacemacs-buffer-mode-map [down-mouse-1] nil)
  ;;
  ;;
  ;; (define-key global-map (kbd "C-#") 'clojure-toggle-reader-comment-sexp)
  ;;
  ;; (define-key global-map (kbd "SPC S s") 'flyspell-correct-at-point)
  ;;
  ;; (define-key markdown-mode-map (kbd "SPC S s") #'flyspell-correct-at-point)
  ;;
  ;; helm opens a new frame when cursor in a buffer positioned underneath another
  ;; see my gist for details to add...
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; elpa stable repository
  ;; if you want to disable the elpa stable repository put this in your dotfile in the user-init function:
  ;; (setq configuration-layer-elpa-archives '(("melpa" . "melpa.org/packages/")
  ;;   ("org" . "orgmode.org/elpa/") ("gnu" . "elpa.gnu.org/packages/")))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Literal Searching Configuration
  ;;
  ;; Literal search, rather than regex, in spacemacs search - helm-ag
  ;; (setq-default helm-grep-ag-command-option "-Q")
  ;;
  ;; End of Searching Configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; evil-cleverparens - now part of the clojure layer (develop branch)
  ;;
  ;; use the evil-cleverparens layer
  ;; https://github.com/luxbock/evil-cleverparens
  ;;
  ;; add evil-cleverparens to clojure-mode
  ;; (spacemacs/toggle-evil-cleverparens-on)
  ;; (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  ;; end of evil-smartparens
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; exclude sayid as it currently does not support nrepl 0.4
  ;;
  ;; Temporary fix
  ;; (setq sayid-inject-dependencies-at-jack-in nil)
  ;; issue raised: https://github.com/syl20bnr/spacemacs/issues/11146
  ;;
  ;; pull request merged into develop to switch sayid off by default
  ;; https://github.com/bpiel/sayid/pull/40
  ;; enable sayid by adding this code to the .spacemacs dotspacemacs/layers configuration
  ;;   dotspacemacs-configuration-layers
  ;;    '(
  ;;       (clojure :variables clojure-enable-sayid t)
  ;;     )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Systemd user service
  ;;
  ;; Use the exec-path-from-shell package to get PATH, MANPATH
  ;; and the environment variables from your zsh or bash rc-files.
  ;;
  ;; (setq exec-path-from-shell-variables
  ;;       (append exec-path-from-shell-variables
  ;;               (list "TERM"
  ;;                     "RUST_SRC_PATH"
  ;;                     "…"
  ;;                     )))
  ;; (exec-path-from-shell-initialize)
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Neotree configuration
  ;;
  ;; Display neotree on the right rather than left (default)
  ;; (setq neo-window-position 'right)
  ;;
  ;; End of Neotree configuration
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; old-school emacs style keybindings that i am replacing with nicer spacemacs alternatives
  ;;
  ;; jr0cket: text scaling keybindings - use spc z x =/-
  ;; (define-key global-map (kbd "c-+") 'text-scale-increase)
  ;; (define-key global-map (kbd "c--") 'text-scale-decrease)
  ;;
  ;; smartparens keybindings - use lisp-state, spc k menu instead
  ;; (define-key global-map (kbd "c-)") 'sp-forward-slurp-sexp)
  ;; (define-key global-map (kbd "c-(") 'sp-backward-slurp-sexp)
  ;; (define-key global-map (kbd "m-)") 'sp-forward-barf-sexp)
  ;; (define-key global-map (kbd "m-(") 'sp-backward-barf-sexp)
  ;;
  ;; jr0cket: keybindings for cycling buffers
  ;; use spc b n and spc b n instead
  ;; (global-set-key [c-prior] 'previous-buffer)
  ;; (global-set-key [c-next] 'next-buffer)
  ;;
  ;; jr0cket: remap multiple cursors to a pattern that is easier to remember
  ;; learn iedit mode instead (its fantastic)
  ;; (define-key global-map (kbd "c-c m c") 'mc/edit-lines)
  ;;
  ;; end of old-school bindings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'practicalli-config)
