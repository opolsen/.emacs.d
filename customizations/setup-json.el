(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '(".eslintrc" . json-mode))

(add-hook 'json-mode-hook #'smartparens-mode)

(defun create-json-buffer ()
  (interactive)
  (switch-to-buffer "*JSON*")
  (json-mode))

(global-set-key (kbd "C-c j") 'create-json-buffer)
