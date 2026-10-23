(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  ;; Full screen magit-status
  (defun magit-status-fullscreen-a (fn &rest args)
    (window-configuration-to-register :magit-fullscreen)
    (apply fn args)
    (delete-other-windows))
  (advice-add 'magit-status :around #'magit-status-fullscreen-a)

  (defun magit-quit-session ()
    "Restore previous window configuration and kill the magit buffer."
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq magit-section-visibility-indicator nil))

(setenv "SSH_ASKPASS" "git-gui--askpass")
