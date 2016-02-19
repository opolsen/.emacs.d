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

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;; Prevent props to be commented out by quotes
(add-hook 'conf-javaprop-mode-hook
          '(lambda () (conf-quote-normal nil)))
