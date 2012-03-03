(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(package-initialize)

;; global settings
(global-linum-mode 1)
(add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand) ;put yasnippet in hippie-expansion list
(setq smart-tab-using-hippie-expand t)
(global-smart-tab-mode t)
(setq-default tab-width 4)
(autoload 'whitespace-mode           "whitespace" "Toggle whitespace visualization."        t)
(setq yas/prompt-functions '(yas/dropdown-prompt))

;; display settings
;; (load-theme 'zenburn)
;; (enable-theme 'zenburn)

;; markdown automode
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

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
