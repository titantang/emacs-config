(require 'package)
;(add-to-list 'package-archives
;             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/display.el")

(set-exec-path-from-shell-PATH)

;; global settings
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand) ;put yasnippet in hippie-expansion list
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
(column-number-mode)
(line-number-mode)
(global-whitespace-mode 1)
(turn-off-auto-fill)
(setq bookmark-save-flag 1)
(setq ffip-patterns '("*.html" "*.org" "*.txt" "*.md" "*.el" "*.clj" "*.py" "*.rb" "*.js" "*.pl" "*.sh" "*.erl" "*.hs" "*.ml" "*.php" "*.css" "*.htm"))
(setq scheme-program-name  "/usr/local/bin/mit-scheme")
;; end global settings
(require 'smart-tab)
(setq smart-tab-using-hippie-expand t)
(global-smart-tab-mode t)
(setq inhibit-splash-screen t)
;; begin minor modes loading secition

(textmate-mode)
(setq ido-everywhere t)

;; use rbenv
(global-rbenv-mode)

;; enable autopair in all buffers
(autopair-global-mode)

;; end minor modes loading section

;; begin filetype section
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pp" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))
(add-to-list 'auto-mode-alist '("\\.pstpl$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . jsx-mode))
;; end filetype section

;; hooks definition
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'untabify)
;(add-hook 'lisp-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'php-mode-hook 'my-php-mode-common-hook)
(add-hook 'find-file-hooks 'my-tramp-no-auto-save)
;; end hooks definition

;; key bindings
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-%") 'goto-match-paren)
(global-set-key (kbd "C-d") 'duplicate-line)
(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-x") 'smex)
;; end key bindings

;; yasnippets
(yas-global-mode 1)
(yas/load-directory
 (concat (file-name-directory (or load-file-name buffer-file-name))
         "snippets/"))

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;; org-mode
(setq org-default-notes-file "~/Dropbox/gtd.org")
(define-key global-map "\C-cc" 'org-capture)
(setq org-completion-use-ido t)

;; dash integration
(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)

;; load my gtd file after emacs started
(find-file "~/Dropbox/gtd.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-todo-keywords (quote ((sequence "TODO(t)" "DONE(d)"))))
 '(ruby-deep-indent-paren-style nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
