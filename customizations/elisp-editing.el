(add-hook 'emacs-lisp-mode-hook       #'smartparens-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
(add-hook 'ielm-mode-hook             #'smartparens-mode)
(add-hook 'lisp-mode-hook             #'smartparens-mode)
(add-hook 'lisp-interaction-mode-hook #'smartparens-mode)
(add-hook 'scheme-mode-hook           #'smartparens-mode)

(global-set-key (kbd "C-c C-k") 'eval-buffer)
