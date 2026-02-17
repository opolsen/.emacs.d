(use-package org
  :ensure nil
  :defer t
  :bind (("<f8>"   . org-tree-slide-mode)
         ("S-<f8>" . org-tree-slide-skip-done-toggle)
         ("C-c a"  . org-agenda))
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-src-fontify-natively t
        org-agenda-files (list "~/org/work.org")
        org-log-done t))

(use-package org-tree-slide
  :defer t
  :config
  (setq org-tree-slide-skip-outline-level 4))
