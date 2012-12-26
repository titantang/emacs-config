(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(package-initialize)
(load-file "~/.emacs.d/elpa/color-theme-molokai.el")

;; global settings
(global-linum-mode 1)
(global-visual-line-mode 1)
(add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand) ;put yasnippet in hippie-expansion list
(setq smart-tab-using-hippie-expand t)
(global-smart-tab-mode t)
(setq-default tab-width 4)
(autoload 'whitespace-mode           "whitespace" "Toggle whitespace visualization."        t)
(auto-fill-mode 1)
(setq comment-auto-fill-only-comments t)
(setq yas/prompt-functions '(yas/dropdown-prompt))

(setq auto-mode-alist (cons '("\\.pp" . puppet-mode) auto-mode-alist))

(setq mouse-wheel-progressive-speed nil)

(setq mouse-wheel-scroll-amount '(2
                                  ((shift)
                                   . 1)
                                  ((control))))

(setq-default whitespace-style '(face trailing tabs))
(global-whitespace-mode 1)

;; chinese display
(when (eq window-system 'ns)
  (let ((my-font-height 120)
        (my-font (cond
                  (t   "Monaco")  ;; XCode 3.1
                  (nil "Menlo")   ;; XCode 3.2
                  ))
        (my-font-ja "STHeiti"))

    (setq mac-allow-anti-aliasing t)
    (setq face-font-rescale-alist
          '(("^-apple-hiragino.*" . 1.2)
            (".*osaka-bold.*" . 1.2)
            (".*osaka-medium.*" . 1.2)
            (".*courier-bold-.*-mac-roman" . 1.0)
            (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
            (".*monaco-bold-.*-mac-roman" . 0.9)
            ("-cdac$" . 1.3)))

    (when my-font
      (set-face-attribute 'default nil :family my-font :height my-font-height)
      ;;(set-frame-font (format "%s-%d" my-font (/ my-font-height 10)))
      )

    (when my-font-ja
      (let ((fn (frame-parameter nil 'font))
            (rg "iso10646-1"))
        (set-fontset-font fn 'chinese-gb2312 `(,my-font-ja . ,rg))
        (set-fontset-font fn 'chinese-gbk `(,my-font-ja . ,rg))))))
;;        (set-fontset-font fn 'unicode `(,my-font-ja . ,rg))))))

(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

(turn-off-auto-fill)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'lisp-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))

(setq bookmark-save-flag 1)
(setq ffip-patterns '("*.html" "*.org" "*.txt" "*.md" "*.el" "*.clj" "*.py" "*.rb" "*.js" "*.pl" "*.sh" "*.erl" "*.hs" "*.ml" "*.php"))
(textmate-mode)

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

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(menu-bar-mode 1)
;; display settings
(color-theme-solarized 'dark)

;; markdown automode
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; feature mode
(setq auto-mode-alist (cons '("\\.feature" . feature-mode) auto-mode-alist))

;; psptl use html mode
(setq auto-mode-alist (cons '("\\.pstpl" . html-mode) auto-mode-alist))

;;Auto-start zencoding on any markup modes
(add-hook 'sgml-mode-hook 'zencoding-mode)

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

;; Turn off autosave for all tramp files
(require 'tramp)
(defun my-tramp-no-auto-save ()
    (when (tramp-tramp-file-p (buffer-file-name))
      (make-local-variable 'auto-save-default)
      (setq auto-save-default nil)
      (auto-save-mode -1)))

(add-hook 'find-file-hooks
          'my-tramp-no-auto-save)

(provide 'my-tramp-no-auto-save)
;;; my-tramp-no-auto-save.el ends here

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

;; yasnippets
(require 'yasnippet)
(yas/initialize)
(yas/load-directory
 (concat (file-name-directory (or load-file-name buffer-file-name))
         "snippets/"))
