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
(ido-mode t)

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-enable-flex-matching t)

;;; smarter fuzzy matching for ido
(flx-ido-mode +1)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; Show results vertically
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

;; Use ido for completion-at-point
(require 'ido-at-point)
(ido-at-point-mode)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; projectile everywhere!
(projectile-global-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-globally-ignored-directories
      (append '("node_modules" "target" ".tmp" "dist" ".cljs_rhino_repl")
              projectile-globally-ignored-directories))
(setq projectile-use-git-grep t)
(setq projectile-enable-caching nil)
(setq projectile-indexing-method 'alien)

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


;; Displays the available key bindings after opening sequence
(require 'which-key)
(which-key-mode)

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

(require 'crux)
(global-set-key (kbd "C-x 4 t") #'crux-transpose-windows)
(global-set-key (kbd "C-c D") #'crux-delete-file-and-buffer)
(global-set-key (kbd "C-c r") #'crux-rename-file-and-buffer)
(global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-S-k") #'crux-kill-whole-line)

;; Kill current buffer without asking
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Nice interactive way to search for pattern in project
(define-key projectile-mode-map (kbd "C-c p n") 'helm-projectile-grep)

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

;; Temporary fix for projectile bug on Windows 10
(setq projectile-git-submodule-command 'nil)
