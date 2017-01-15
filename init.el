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
(setq-default indent-tabs-mode nil)
;; end global settings
(require 'smart-tab)
(setq smart-tab-using-hippie-expand t)
(global-smart-tab-mode t)
(setq inhibit-splash-screen t)
(require 'python-mode)
;; begin minor modes loading secition

(textmate-mode)
(setq ido-everywhere t)

;; use rbenv
(global-rbenv-mode)

;; enable autopair in all buffers
(autopair-global-mode)

;; enable rinari
(global-rinari-mode)

;; enable elpy
(elpy-enable)

;; end minor modes loading section

;; begin filetype section
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pp" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))
(add-to-list 'auto-mode-alist '("\\.pstpl$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . jsx-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb$" . web-mode))
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
(setq org-default-notes-file "~/Dropbox/notes/gtd.org")
(setq org-directory "~/Dropbox/notes")
(setq org-completion-use-ido t)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
;; web-mode
(setq web-mode-markup-indent-offset 2)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "refile.org")
               "** TODO %?\n%U\n")
              ("r" "respond" entry (file "refile.org")
               "** NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "refile.org")
               "** %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "refile.org")
               "** TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "refile.org")
               "** MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "refile.org")
               "** PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t))))
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("laptop" . ?l)))
(setq org-agenda-files (quote ("~/Dropbox/notes")))
;; dash integration
(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)

;; load my gtd file after emacs started
(find-file "~/Dropbox/notes/gtd.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (color-theme-solarized web-mode web-beautify twilight-theme tidy textmate solarized-theme smex smart-tab scss-mode sbt-mode sass-mode rvm ruby-test-mode rinari rbenv python-mode puppet-mode php-extras paredit org mustache-mode monokai-theme material-theme markdown-mode magit-svn magit-find-file lorem-ipsum log4j-mode jsx-mode js2-mode ido-ubiquitous idle-highlight-mode go-mode gitignore-mode gist geben full-ack flymake-php flymake feature-mode expand-region exec-path-from-shell emmet-mode elpy ein dockerfile-mode date-at-point dash-at-point coffee-mode autopair angular-snippets))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
