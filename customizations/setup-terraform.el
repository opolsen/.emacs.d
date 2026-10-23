(use-package terraform-mode
  :defer t
  :hook (terraform-mode . terraform-format-on-save-mode))
