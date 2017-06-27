;; Prefer utf-8 encoding
(prefer-coding-system 'utf-8)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(setq mac-option-modifier 'super
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;; Norwegian mac-keyboard alt-keys
(define-key key-translation-map (kbd "s-8") (kbd "["))
(define-key key-translation-map (kbd "s-(") (kbd "{"))
(define-key key-translation-map (kbd "s-9") (kbd "]"))
(define-key key-translation-map (kbd "s-)") (kbd "}"))
(define-key key-translation-map (kbd "s-7") (kbd "|"))
(define-key key-translation-map (kbd "s-/") (kbd "\\"))

;; Prevent props to be commented out by quotes
(add-hook 'conf-javaprop-mode-hook
          '(lambda () (conf-quote-normal nil)))

;; Move files to trash can when deleted
(setq delete-by-moving-to-trash t)

(setq tramp-default-method "ssh")

(add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))

(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))
