(setq org-agenda-files '("~/Documents/org-data")
      org-agenda-show-outline-path nil
      org-catch-invisible-edits 'smart
      org-clock-in-resume t
      org-clock-out-remove-zero-time-clocks t
      org-clock-persist 'clock
      org-ctrl-k-protect-subtree t
      org-default-notes-file "~/Documents/org-data/notes.org"
      org-directory "~/Documents/org-data"
      org-ditaa-jar-path (expand-file-name "~/git/my-spacemacs/jars/ditaa.jar")
      org-ditaa-eps-jar-path (expand-file-name "~/git/my-spacemacs/jars//DitaaEps.jar")
      org-export-coding-system 'utf-8
      org-hide-block-startup t
      org-journal-dir "~/Documents/journal/"
      org-journal-file-format "%Y-%m-%d"
      org-journal-date-prefix "#+TITLE: "
      org-journal-date-format "%A, %B %d %Y"
      org-journal-time-prefix "* "
      org-journal-time-format ""
      org-log-done 'time
      org-log-into-drawer t
      org-log-refile 'time
      org-plantuml-jar-path (expand-file-name "~/git/my-spacemacs/jars/plantuml.jar")
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets (quote ((nil :maxlevel . 5)
                                 (org-agenda-files :maxlevel . 5)))
      org-refile-use-outline-path (quote file)
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window
      org-startup-with-inline-images (display-graphic-p))

(setq org-capture-templates
      (quote
       (("t" "todo" entry
         (file "~/Documents/org-data/refile.org")
         "* TODO %?\n\n  %a"
         :empty-lines-after 1 :clock-in t :clock-resume t)
        ("r" "respond" entry
         (file "~/Documents/org-data/refile.org")
         "* NEXT Respond to %:from on %:subject\n  SCHEDULED: %t\n  %a"
         :empty-lines-after 1 :clock-in t :clock-resume t)
        ("n" "note" entry
         (file "~/Documents/org-data/notes.org")
         "* %? :NOTE:\n\n  %a"
         :empty-lines-after 1 :clock-in t :clock-resume t)
        ("j" "journal" entry
         (file+olp+datetree "~/Documents/org-data/journal.org")
         "* %?\n  "
         :empty-lines-after 1 :clock-in t :clock-resume t)
        ("p" "phone" entry
         (file "~/Documents/org-data/refile.org")
         "* PHONE %? :PHONE:\n  "
         :empty-lines-after 1 :clock-in t :clock-resume t)
        ("m" "mail" entry
         (file "~/Documents/org-data/refile.org")
         "* MAIL from %:from on %:subject\n\n  %a"
         :empty-lines-after 1 :clock-in t :clock-resume t))))

(setq org-todo-keywords (quote
                         ((sequence "TODO(t)"
                                    "NEXT(n)"
                                    "STARTED(s!)"
                                    "DELEGATED(w@/!)"
                                    "HOLD(h@/!)"
                                    "|"
                                    "CANCELLED(c@)"
                                    "DONE(d!)"))))

(setq org-latex-classes
      '(("beamer"
         "\\documentclass[presentation]{beamer}"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("hitec-article"
         "\\documentclass[12pt]{hitec}
  [DEFAULT-PACKAGES]
  [PACKAGES]
  [NO-EXTRA]
  \\settextfraction{0.95}\n"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        ("article" "\\documentclass[11pt]{article}"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        ("korma-article" "\\documentclass[11pt]{scrartcl}"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        ("report"
         "\\documentclass[11pt]{report}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("korma-report"
         "\\documentclass[11pt]{scrreport}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("korma-book"
         "\\documentclass[11pt]{scrbook}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
        ("book"
         "\\documentclass[11pt]{book}"
         ("\\part{%s}" . "\\part*{%s}")
         ("\\chapter{%s}" . "\\chapter*{%s}")
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(setq org-latex-hyperref-template
      "\\hypersetup{pdfauthor={%a},
                      pdftitle={%t},
                      pdfkeywords={%k},
                      pdfsubject={%d},
                      pdfcreator={%c},
                      pdflang={%L},
                      colorlinks=true,
                      linkcolor=blue}")

(setq org-latex-listings t
      org-latex-listings-options '(("basicstyle" "\\tiny")
                                   ("frame" "single")
                                   ("stringstyle" "\\color{orange}")
                                   ("commentstyle" "\\color{cyan}")
                                   ("keywordstyle" "\\color{blue}")
                                   ("showstringspaces" "false")
                                   ("breakatwhitespace" "false")
                                   ("breaklines" "true")))

(setq org-latex-pdf-process
      '("lualatex -interaction nonstopmode -output-directory %o %f"
        "lualatex -interaction nonstopmode -output-directory %o %f"
        "lualatex -interaction nonstopmode -output-directory %o %f"))

(setq org-html-checkbox-type 'unicode
      org-html-html5-fancy t
      org-html-doctype "html5")

(setq org-ascii-charset 'utf-8
      org-ascii-text-width 79)

;; - create a new journal entry
(spacemacs/set-leader-keys "oj" 'org-journal-new-entry)

;; Markdown mode hook for orgtbl-mode minor mode
(add-hook 'markdown-mode-hook 'turn-on-orgtbl)

(with-eval-after-load 'org

  (setq org-agenda-custom-commands
        (quote
         (("n" "Agenda and all TODO's"
           ((agenda "" nil)
            (alltodo "" nil))
           nil)
          ("wr" "Weekly Report"
           ((todo "DONE|CANCELLED"
                  ((org-agenda-overriding-header "Completed and Cancelled : Last Week")))
            (todo "STARTED|NEXT"
                  ((org-agenda-overriding-header "WIP")))
            (todo "HOLD|DELEGATED"
                  ((org-agenda-overriding-header "On Hold and Delegated Tasks")))
            (todo "TODO"
                  ((org-agenda-overriding-header "Task Backlog"))))
           nil nil))))
  (org-element-update-syntax)
  (org-clock-persistence-insinuate))

(provide 'my-org)
