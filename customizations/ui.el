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

(load-theme 'modus-vivendi t)

(set-face-attribute 'default nil :height 120 :family "Fira Code")

;; These settings relate to how emacs interacts with your operating system
(setq ;; Save clipboard strings into kill ring before replacing them.
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

;; Show line and column
(setq line-number-mode t)
(setq column-number-mode t)

;; Show empty lines in fringe
(set-default 'indicate-empty-lines t)

;; Prevent line breaking
(setq-default truncate-lines t)

;; Diminish — hide minor modes from modeline
(use-package diminish
  :ensure t
  :config
  (diminish 'subword-mode)
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package diff-hl
  :config
  (global-diff-hl-mode))
