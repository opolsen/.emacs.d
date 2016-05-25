(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Racer adds additional code completion and navigation
;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)

;; (add-hook 'racer-mode-hook #'company-mode)
;; (global-set-key (kbd "TAB") #'company-indent-or-complete-common) ;
;; (setq company-tooltip-align-annotations t)
