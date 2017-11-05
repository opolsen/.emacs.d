(require 'company)
(require 'company-go)
(require 'flycheck)
(require 'go-mode)
(require 'go-eldoc)

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(defun my-go-mode-hook ()
  (flycheck-mode)
  (company-mode)
  (go-eldoc-setup)
  (add-to-list 'company-backends 'company-go))

(add-hook 'go-mode-hook #'my-go-mode-hook)
