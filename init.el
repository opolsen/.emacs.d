;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa". "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable". "https://stable.melpa.org/packages/") t)

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
  '(anaconda-mode
    anzu
    avy
    browse-kill-ring
    cider
    clj-refactor
    clojure-mode
    clojure-mode-extra-font-locking
    company
    company-anaconda
    company-quickhelp
    crux
    dash
    diff-hl
    diminish
    dockerfile-mode
    ethan-wspace
    expand-region
    flx-ido
    flycheck
    flycheck-clojure
    flycheck-joker
    flycheck-pos-tip
    git-timemachine
    helm
    helm-projectile
    hydra
    ido-at-point
    ido-completing-read+
    ido-vertical-mode
    json-mode
    jq-mode
    magit
    markdown-mode
    monokai-theme
    move-text
    multiple-cursors
    org-tree-slide
    projectile
    rainbow-delimiters
    restclient
    smartparens
    smex
    smooth-scroll
    ssh-agency
    tagedit
    terraform-mode
    tide
    undo-tree
    vlf
    which-key
    whole-line-or-region
    yasnippet
    yaml-mode))

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

;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))

;;;;
;; Customization
;;;;

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path (expand-file-name "customizations" user-emacs-directory))

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

(load "setup-magit.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-ts.el")
(load "setup-json.el")
(load "setup-html.el")
(load "setup-python.el")
(load "setup-xml.el")
(load "setup-org.el")
(load "setup-yaml.el")
(load "setup-docker.el")

(load "decode-jwt.el")
