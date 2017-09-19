(require 'company)
(require 'company-racer)
(require 'flycheck)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(defun my-rust-mode-hook ()
  (racer-mode) ;; adds additional code completion and navigation
  (eldoc-mode)
  (flycheck-mode)
  (flycheck-rust-setup)
  (company-mode)
  (add-to-list 'company-backends 'company-racer))

(add-hook 'rust-mode-hook #'my-rust-mode-hook)
