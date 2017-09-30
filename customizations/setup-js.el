(require 'company-tern)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(defun common-js-setup ()
  (smartparens-mode)
  (tern-mode)
  (company-mode)
  (add-to-list 'company-backends 'company-tern)
  (when (executable-find "eslint")
    (flycheck-mode t)
    (flycheck-select-checker 'javascript-eslint)))

(defun my-js2-mode-hook ()
  (common-js-setup)
  (js2-imenu-extras-mode)
  (js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m")
  (define-key js-mode-map (kbd "M-.") nil)
  (define-key js2-mode-map (kbd "M-j") nil)
  (define-key js2-mode-map (kbd "C-k") 'js2r-kill))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(defun my-web-mode-hook ()
  (common-js-setup)
  (web-mode-set-content-type "jsx"))

(add-hook 'web-mode-hook 'my-web-mode-hook)

(setq js2-highlight-level 3)

;; Set indentation level
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(setq-default web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-style-padding 2
              web-mode-script-padding 2)

;; Additional pairs for smartparens
(sp-with-modes 'web-mode
  (sp-local-pair "<" ">")
  (sp-local-pair "`" "`"))

;; From https://emacs.stackexchange.com/questions/33536/how-to-edit-jsx-react-files-in-emacs
(defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
  "Workaround sgml-mode and follow airbnb component style."
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))
