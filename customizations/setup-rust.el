(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Racer cannot read RUST_SRC_PATH in emacs for some reason,
;; so setting this explicitly for now
(setq racer-rust-src-path "~/rustc-1.8.0/src")

;; RACER adds additional code completion and navigation
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(add-hook 'racer-mode-hook #'flycheck-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; Use company for auto-completion
(require 'company-racer)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-racer))

(add-hook 'racer-mode-hook #'company-mode)
(setq company-tooltip-align-annotations t)
