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

;; Always download missing packages
(setq use-package-always-ensure t)

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
(load "setup-json.el")
(load "setup-xml.el")
(load "setup-org.el")
(load "setup-yaml.el")
(load "setup-docker.el")

(load "setup-terraform.el")
(load "decode-jwt.el")
(load "setup-markdown.el")
