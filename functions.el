(defun sgml-delete-tagged-text ()
  "delete text between the tags that contain the current point"
  (interactive)
  (let ((b (point)))
    (sgml-skip-tag-backward 1)
    (when (not (eq b (point)))
      ;; moved somewhere, should be at front of a tag now
      (save-excursion
        (forward-sexp 1)
        (setq b (point)))
      (sgml-skip-tag-forward 1)
      (backward-sexp 1)
      (delete-region b (point)))))

;; functions
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun blank-line-p ()
  "Check if this is a blank line"
  (save-excursion
    (beginning-of-line) (looking-at "[ \t]*$")))

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "C-o") 'open-next-line)

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "M-o") 'open-previous-line)

; sql mode enhancement
(defun my-sql-send-paragraph ()
  "send paragraph without the extra line to sql buffer"
  (interactive)
  (let ((start (save-excursion
                 (backward-paragraph)
                 (if (not (blank-line-p))
                     (point)
                   (forward-line 1)
                   (point))))
        (end (save-excursion
               (forward-paragraph)
               (point-min))))
    (sql-send-region start end)))

(defun my-addition-to-sql-mode ()
  "override send-paragraph key to my function"
  (local-set-key (kbd "C-c C-r") 'my-addition-to-sql-mode)
  (toggle-truncate-lines))

(add-hook 'sql-mode-hook 'my-addition-to-sql-mode)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; php automode
(defun my-php-mode-common-hook ()
  ;; my customizations for php-mode
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (c-set-offset 'topmost-intro-cont 4)
  (c-set-offset 'class-open 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'arglist-intro '+)
  (eldoc-mode))

;; Turn off autosave for all tramp files
(require 'tramp)
(defun my-tramp-no-auto-save ()
    (when (tramp-tramp-file-p (buffer-file-name))
      (make-local-variable 'auto-save-default)
      (setq auto-save-default nil)
      (auto-save-mode -1)))

(provide 'my-tramp-no-auto-save)
;;; my-tramp-no-auto-save.el ends here
