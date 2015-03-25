;; display settings
(setq-default tab-width 4)
(menu-bar-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)

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
      )

    (when my-font-ja
      (let ((fn (frame-parameter nil 'font))
            (rg "iso10646-1"))
        (set-fontset-font fn 'chinese-gb2312 `(,my-font-ja . ,rg))
        (set-fontset-font fn 'chinese-gbk `(,my-font-ja . ,rg))))))

(load-theme 'zenburn t)
