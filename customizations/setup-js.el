;; javascript / html
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

;; Activate subword-mode
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'html-mode-hook 'subword-mode)

;; Setup js2-refactor-mode
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")

;; Set indentation level
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-style-padding 2
              web-mode-script-padding 2)

;; Use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

;; Use json-mode for .json files
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;; Use smartparens
(require 'smartparens-config)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'web-mode-hook #'smartparens-mode)
(add-hook 'json-mode-hook #'smartparens-mode)

(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))
