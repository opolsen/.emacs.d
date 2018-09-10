(setq org-src-fontify-natively t)
(setq org-tree-slide-skip-outline-level 4)

(defun my-org-mode-hook ()
  (org-indent-mode))

(add-hook 'org-mode-hook #'my-org-mode-hook)

(global-set-key (kbd "<f8>") 'org-tree-slide-mode)
(global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
