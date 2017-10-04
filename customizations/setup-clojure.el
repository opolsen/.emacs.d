(require 'clojure-mode-extra-font-locking)
(require 'clj-refactor)
(require 'flycheck-joker)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (smartparens-strict-mode)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (flycheck-mode 1)
  (flycheck-clojure-setup)
  (when (executable-find "joker")
    (flycheck-select-checker 'clojure-joker)))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(add-hook 'cider-mode-hook 'eldoc-mode)

(add-hook 'cider-repl-mode-hook 'smartparens-mode)

;; hide REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect nil)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; Start figwheel when creating cljs repl
(setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
