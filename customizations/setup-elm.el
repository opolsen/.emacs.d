(require 'elm-mode)

(defun my-elm-mode-hook ()
  (smartparens-mode)
  (add-to-list 'company-backends 'company-elm)
  (company-mode)
  (company-quickhelp-mode 1)
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))
  (flycheck-mode))

(add-hook 'elm-mode-hook 'my-elm-mode-hook)

(setq elm-tags-on-save t)
(setq elm-tags-exclude-elm-stuff nil)
(setq elm-format-on-save t)

(add-to-list 'auto-mode-alist '("\\.elm$" . elm-mode))
