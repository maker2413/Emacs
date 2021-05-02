(provide 'init-config)

;; Make Emacs look clean
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Change where temp files are stored
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Bring in shell environment variables
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

;; Set theme
(load-theme 'afternoon t)

(add-hook 'sh-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq sh-basic-offset 2)
	    (setq sh-indentation 2)
	    (setq sh-indent-after-continuation 'always)))

(add-hook 'lua-mode-hook
	  (lambda ()
	    (setq tab-width 2
		  indent-tabs-mode nil)))

(add-hook 'go-mode-hook
	  (lambda ()
	    (setq tab-width 4)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (setq tab-width 4)))

(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)
