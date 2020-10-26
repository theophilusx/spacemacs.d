;; Indentation of function forms
;; https://github.com/clojure-emacs/clojure-mode#indentation-of-function-forms
(setq clojure-indent-style 'align-arguments)

;; Vertically align s-expressions
;; https://github.com/clojure-emacs/clojure-mode#vertical-alignment
(setq clojure-align-forms-automatically t)

(setq clojure-toplevel-inside-comment-form t
      cider-overlays-use-font-lock t)

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

;; Assign keybinding to the toggle-reader-comment-sexp function
(define-key global-map (kbd "C-#") 'clojure-toggle-reader-comment-sexp)

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

(provide 'my-clojure)
