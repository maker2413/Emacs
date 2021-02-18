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

;; Set theme
(load-theme 'afternoon t)
