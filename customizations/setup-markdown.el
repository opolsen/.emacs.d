(use-package markdown-mode
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc")))
