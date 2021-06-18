(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

(setq visible-bell t)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)

;; Initialize package sources
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package afternoon-theme)

;;(load-theme 'afternoon t)

(use-package diminish)

(use-package counsel
  :bind(("M-x" . counsel-M-x)))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :config
  (ivy-mode 1))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode t)
  :custom ((doom-modeline-height 10)))

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(eshell-mode-hook
		shell-mode-hook
		term-mode-hook))
  (add-hook mode(lambda() (display-line-numbers-mode 0))))

;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode t)
  :diminish
  :config
  (setq which-key-idls-delay 0.5))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra)

(defhydra hydra-window (global-map "C-o"
                        :timeout 3
                        :hint nil)
  "
  ^Window Management^

  ^Windows^     ^Buffers^     ^Frame^
  ^^^^^^^^---------------------------------
  _f_: next     _n_: next     _TAB_: switch
  _b_: prev     _p_: prev     ^ ^
  "
  ("f" other-window)
  ("b" (other-window (- 1)))
  ("n" next-buffer)
  ("p" previous-buffer)
  ("TAB" other-frame)
  ("g" nil "cancel" :color blue))

(setq-default indent-tabs-mode nil)

;; Change where temp files are stored
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(hydra helpful ivy-rich which-key doom-themes counsel yaml-mode use-package terraform-mode markdown-mode lua-mode json-mode ivy groovy-mode go-mode fish-mode exec-path-from-shell doom-modeline dockerfile-mode diminish afternoon-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
