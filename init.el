;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(paredit
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    ido-ubiquitous
    smex
    projectile
    rainbow-delimiters
    tagedit
    magit
    git-timemachine
    avy
    dired-details
    expand-region
    flycheck
    flycheck-pos-tip
    flycheck-clojure
    flycheck-rust
    flycheck-elm
    which-key
    ido-at-point
    ido-vertical-mode
    flx-ido
    js2-mode
    js2-refactor
    clj-refactor
    move-text
    multiple-cursors
    nodejs-repl
    smartparens
    whole-line-or-region
    yasnippet
    json-mode
    web-mode
    ethan-wspace
    color-theme-sanityinc-tomorrow
    zerodark-theme
    smooth-scroll
    dash
    diminish
    company
    company-quickhelp
    tern
    company-tern
    company-racer
    git-gutter-fringe
    hydra
    anzu
    crux
    rust-mode
    racer
    elm-mode
    anaconda-mode
    company-anaconda))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(add-to-list 'load-path site-lisp-dir)

;;;;
;; Customization
;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-python.el")
(load "setup-rust.el")
(load "setup-xml.el")
(load "setup-elm.el")
(load "setup-org.el")
