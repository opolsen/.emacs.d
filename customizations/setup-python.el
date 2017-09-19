(require 'anaconda-mode)
(require 'company)
(require 'company-anaconda)
(require 'flycheck)
(require 'smartparens)

(defun my-python-mode-hook ()
  (anaconda-mode)
  (anaconda-eldoc-mode)
  (subword-mode)
  (company-mode)
  (add-to-list 'company-backends 'company-anaconda)
  (smartparens-mode)
  (flycheck-mode)
  (when (executable-find "pylint")
    (flycheck-select-checker 'python-pylint)))


(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'inferior-python-mode-hook 'smartparens-mode)


(define-key python-mode-map (kbd "C-c v") 'pythonic-activate)
