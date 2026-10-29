;; Customizations relating to editing a buffer.

;; Set super key on windows
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-all-abbrevs
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Anzu — show match count in search
(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode 1))

(define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))
(save-place-mode 1)

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Rainbow delimiters in all programming modes
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(electric-indent-mode -1)

;; Delete region when typing
(delete-selection-mode 1)

;; Expansions
(global-set-key (kbd "C-,") 'completion-at-point)
(global-set-key (kbd "C-.") 'hippie-expand)

;; Move-text — move lines/regions up/down
(use-package move-text
  :bind (("C-S-<up>"   . move-text-up)
         ("C-S-<down>" . move-text-down)))

;; Keybinding for occur
(global-set-key (kbd "C-c o") 'occur)

;; Indent after newline
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Keep the whitespace clean
(setq mode-require-final-newline nil)

;; Whitespace cleanup (only on buffers that were clean when opened)
(use-package whitespace-cleanup-mode
  :ensure t
  :diminish
  :bind ("C-c w" . whitespace-cleanup)
  :init
  (setq whitespace-style '(face trailing empty tabs))
  :config
  (global-whitespace-cleanup-mode 1))

(setq-default show-trailing-whitespace t)

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-c <tab>") 'indent-buffer)

;; Automatically update unmodified buffers whose files have changed.
(global-auto-revert-mode 1)

;; Multiple-cursors
(use-package multiple-cursors
  :bind (("C-æ"       . mc/mark-next-like-this)
         ("C-å"       . mc/mark-previous-like-this)
         ("M-æ"       . mc/mark-all-like-this)
         ("C-M-æ"     . mc/mark-all-in-region)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-e" . mc/edit-ends-of-lines)
         ("C-S-c C-a" . mc/edit-beginnings-of-lines)))

;; Avy — jump to visible text
(use-package avy
  :bind (("C-ø"     . avy-goto-word-or-subword-1)
         ("C-Ø"     . avy-goto-char-timer)
         ("C-M-ø"   . avy-goto-line)
         :map isearch-mode-map
         ("C-ø"     . avy-isearch))
  :config
  (setq avy-background t))

;; Expand-region — expand selection semantically
(use-package expand-region
  :bind (("C-'" . er/expand-region)
         ("C-*" . er/contract-region)))

;; Quickly join next line with current line
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))


;; Quickly open line above or below current line regardless of cursor position
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; Smartparens — structural editing for all languages
(use-package smartparens
  :diminish
  :defer t
  :bind (:map smartparens-mode-map
              ("C-<right>"   . sp-forward-slurp-sexp)
              ("C-<left>"    . sp-forward-barf-sexp)
              ("C-M-<left>"  . sp-backward-slurp-sexp)
              ("C-M-<right>" . sp-backward-barf-sexp)
              ("M-r"         . sp-rewrap-sexp)
              ("C-k"         . sp-kill-hybrid-sexp)
              ("C-M-w"       . sp-copy-sexp))
  :config
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  (setq sp-highlight-pair-overlay nil))

;; Whole-line-or-region
(use-package whole-line-or-region
  :bind ("M-w" . whole-line-or-region-kill-ring-save))

;; Undo-tree — visualize undo history
(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree-history" user-emacs-directory)))))

;; Browse-kill-ring
(use-package browse-kill-ring
  :defer t)
