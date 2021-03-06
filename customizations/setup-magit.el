(require 'magit)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(global-set-key (kbd "C-x g") #'magit-status)

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(setq magit-section-visibility-indicator nil)

(setenv "SSH_ASKPASS" "git-gui--askpass")
