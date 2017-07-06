(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '(".eslintrc" . json-mode))

(add-hook 'json-mode-hook #'smartparens-mode)
