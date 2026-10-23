;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(use-package ido
  :ensure nil
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t
        ido-use-filename-at-point nil
        ido-auto-merge-work-directories-length -1
        ido-use-virtual-buffers t
        ido-enable-flex-matching t
        ido-use-filename-at-point nil
        ido-auto-merge-work-directories-length -1
        ido-use-virtual-buffers t))

(use-package flx-ido
  :after ido
  :config
  (flx-ido-mode +1))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :after ido
  :config
  (ido-vertical-mode 1))

(use-package ido-at-point
  :after ido
  :config
  (ido-at-point-mode))

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Smex — better M-x
(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (smex-initialize))

;; Projectile — project management
(use-package projectile
  :diminish
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (setq projectile-globally-ignored-directories
        (append '("node_modules" "target" ".tmp" "dist" ".cljs_rhino_repl")
                projectile-globally-ignored-directories)
        projectile-use-git-grep t
        projectile-enable-caching nil
        projectile-indexing-method 'alien
        projectile-git-submodule-command nil))

;; Require this package in order to be able to visit all marked files in dired mode
(require 'dired-x)

;; Quickly jump to next window
(global-set-key (kbd "M-o") 'other-window)

;; Move between windows with Shift-arrow
(windmove-default-keybindings)

;; Quicker navigation between paragraphs
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "C-<home>") 'beginning-of-buffer)
(global-set-key (kbd "C-<end>") 'end-of-buffer)

;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))


;; Which-key — display available keybindings
(use-package which-key
  :diminish
  :defer 1
  :config
  (which-key-mode))

;; Try to get a bit smoother scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-up-aggressively 0.0
      scroll-down-aggressively 0.0
      scroll-preserve-screen-position t)

;; Quickly revert buffer with F5
(global-set-key (kbd "<f5>") 'revert-buffer)

(require 'recentf)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode.
(recentf-mode t)

; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

;; Smart home key.
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "<home>") 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;; Crux — useful interactive commands
(use-package crux
  :bind (("C-x 4 t" . crux-transpose-windows)
         ("C-c D"   . crux-delete-file-and-buffer)
         ("C-c r"   . crux-rename-file-and-buffer)
         ("C-c d"   . crux-duplicate-current-line-or-region)
         ("C-S-k"   . crux-kill-whole-line)
         ("C-c e"   . crux-eval-and-replace)))

;; Kill current buffer without asking
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(use-package helm
  :defer nil)

(use-package helm-projectile
  :after (helm projectile)
  :bind (:map projectile-mode-map
              ;; Nice interactive way to search for pattern in project
              ("C-c p n" . helm-projectile-grep)))

(global-subword-mode 1)

;; Create a new scratch buffer
(defun create-scratch-buffer ()
  (interactive)
  (let ((n 0)
        buffer-name)
    (while (progn
             (setq buffer-name (concat "*scratch-" (int-to-string n) "*"))
             (setq n (1+ n))
             (get-buffer buffer-name)))
    (get-buffer-create buffer-name)
    (switch-to-buffer buffer-name)
    (emacs-lisp-mode)
    (insert ";; Scratch buffer\n\n")))

(global-set-key (kbd "C-c b") 'create-scratch-buffer)

(global-set-key (kbd "C-c s") 'eshell)

(defun goto-line-with-feedback ()
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode 1)
        (call-interactively 'goto-line))
    (display-line-numbers-mode -1)))

(global-set-key (kbd "M-g M-g") 'goto-line-with-feedback)

;; Enable winner-mode. Enables undoing window changes.
(winner-mode 1)
