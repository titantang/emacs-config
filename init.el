(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(package-initialize)
(load-file "~/.emacs.d/elpa/color-theme-molokai.el")

;; global settings
(global-linum-mode 1)
(add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand) ;put yasnippet in hippie-expansion list
(setq smart-tab-using-hippie-expand t)
(global-smart-tab-mode t)
(setq-default tab-width 4)
(autoload 'whitespace-mode           "whitespace" "Toggle whitespace visualization."        t)
(auto-fill-mode 1)
(setq comment-auto-fill-only-comments t)
(setq yas/prompt-functions '(yas/dropdown-prompt))

(setq mouse-wheel-progressive-speed nil)

(setq mouse-wheel-scroll-amount '(2
                                  ((shift)
                                   . 1)
                                  ((control))))

(setq-default whitespace-style '(face trailing tabs))
(global-whitespace-mode 1)
(turn-off-auto-fill)

(setq bookmark-save-flag 1)
(setq ffip-patterns '("*.html" "*.org" "*.txt" "*.md" "*.el" "*.clj" "*.py" "*.rb" "*.js" "*.pl" "*.sh" "*.erl" "*.hs" "*.ml" "*.php"))
(textmate-mode)

;; display settings
(color-theme-molokai)

;; markdown automode
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; php automode
(defun my-php-mode-common-hook ()
  ;; my customizations for php-mode
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (c-set-offset 'topmost-intro-cont 4)
  (c-set-offset 'class-open 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'arglist-intro '+))

(add-hook 'php-mode-hook 'my-php-mode-common-hook)
(setq auto-mode-alist (cons '("\\.php" . php-mode) auto-mode-alist))

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

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

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

;; key bindings
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-%") 'goto-match-paren)
(global-set-key (kbd "C-d") 'duplicate-line)
