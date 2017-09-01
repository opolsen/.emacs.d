(require 'company)
(require 'company-anaconda)
(require 'anaconda-mode)
(require 'smartparens)
(require 'flycheck)

(defun my-python-mode-hook ()
  (anaconda-mode)
  (anaconda-eldoc-mode)
  (subword-mode)
  (company-mode)
  (smartparens-mode)
  (flycheck-mode)
  (when (executable-find "pylint")
    (flycheck-select-checker 'python-pylint)))

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'inferior-python-mode-hook 'smartparens-mode)

(define-key python-mode-map (kbd "C-c v") 'pythonic-activate)
