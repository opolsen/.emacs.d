(use-package json-mode
  :mode ("\\.json\\'" ".eslintrc")
  :hook (json-mode . smartparens-mode)
  :bind (:map json-mode-map
         ("C-c C-j" . jq-interactively))
  :config
  (defun create-json-buffer ()
    "Create a new buffer in json-mode."
    (interactive)
    (switch-to-buffer "*JSON*")
    (json-mode))
  (global-set-key (kbd "C-c j") 'create-json-buffer))

(use-package jq-mode
  :defer t)
