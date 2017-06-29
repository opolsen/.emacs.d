;; javascript / html
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

(setq js2-highlight-level 3)

(defun my-js2-mode-hook ()
            (flycheck-mode t)
            (when (executable-find "eslint")
              (flycheck-select-checker 'javascript-eslint))
            (tern-mode t)
            (define-key js-mode-map (kbd "M-.") nil)
            (define-key js2-mode-map (kbd "M-j") nil)
            (define-key js2-mode-map (kbd "C-k") 'js2r-kill))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;; Activate subword-mode
(add-hook 'js-mode-hook 'subword-mode)
(add-hook 'web-mode-hook 'subword-mode)
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

;; Use jsx as default content-type
(defun my-web-mode-hook ()
  (web-mode-set-content-type "jsx")
  (flycheck-mode t)
  (when (executable-find "eslint")
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (tern-mode t)))

(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Use json-mode for .json files
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '(".eslintrc" . json-mode))

;; Use smartparens for web modes
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'web-mode-hook #'smartparens-mode)
(add-hook 'json-mode-hook #'smartparens-mode)

;; Additional pairs for smartparens
(sp-with-modes 'web-mode
  (sp-local-pair "<" ">")
  (sp-local-pair "`" "`"))

(require 'company-tern)
(add-to-list 'company-backends 'company-tern)

(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))
