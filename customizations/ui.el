;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Turn off the menu bar at the top of each frame because it's distracting
(if (not (eq system-type 'darwin))
    (menu-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(load-theme 'kaolin-dark t)

(set-face-attribute 'default nil :height 100 :family "Source Code Pro Medium")

(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :height 120))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

;; Always start emacs in fullscreen
(set-frame-parameter nil 'fullscreen 'maximized)

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")

;; Prevent smartparens overlay, since it makes content unreadable with current color scheme
(setq sp-highlight-pair-overlay nil)

;; Show line and column
(setq line-number-mode t)
(setq column-number-mode t)

;; Prevent line breaking
(setq-default truncate-lines t)

;; Cleanup modeline
(require 'diminish)
(eval-after-load "subword" '(diminish 'subword-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "ethan-wspace" '(diminish 'ethan-wspace-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
(eval-after-load "tern" '(diminish 'tern-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "autorevert" '(diminish 'auto-revert-mode))
(eval-after-load "git-gutter" '(diminish 'git-gutter-mode))
(eval-after-load "anzu" '(diminish 'anzu-mode))




(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq flycheck-indication-mode nil)
