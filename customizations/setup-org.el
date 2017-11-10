(setq org-src-fontify-natively t)

(defun my-org-mode-hook ()
  (org-indent-mode))

(add-hook 'org-mode-hook #'my-org-mode-hook)
