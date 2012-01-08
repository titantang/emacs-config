
;; display settings
(color-theme-solarized-light)

;; enable rvm path loading
(rvm-use-default)

;; markdown automode
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
