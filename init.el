(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

(setq-default indent-tabs-mode nil)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha '(96 . 92))
(add-to-list 'default-frame-alist '(alpha . (96 . 92)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)

;; Change where temp files are stored
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;(use-package afternoon-theme)

(use-package doom-modeline
  :init (doom-modeline-mode t)
  :custom ((doom-modeline-height 10)))

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package all-the-icons)

(use-package diminish)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode t)
  :diminish
  :config
  (setq which-key-idls-delay 0.5))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind(("M-x" . counsel-M-x)
        ("C-c r" . counsel-rg)
        ("C-x b" . counsel-switch-buffer)))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package hydra)

(defhydra hydra-window (global-map "C-o"
                        :timeout 5
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

(dolist (mode '(eshell-mode-hook
                shell-mode-hook
                term-mode-hook))
  (add-hook mode(lambda() (display-line-numbers-mode 0))))

(add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(setq org-confirm-babel-evaluate nil)

;; Automatically tangle our Emacs.org config file when we save it
(defun heph/org-babel-tangle-config ()
  (when (or (string-equal (buffer-file-name)
                          (expand-file-name "~/.config/emacs/README.org"))
            (string-equal (buffer-file-name)
                          (expand-file-name "~/.config/emacs/exwm/README.org")))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'heph/org-babel-tangle-config)))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("ba" . "src bash"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("ja" . "src java"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
