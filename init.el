;; -----------------------------------------------------------------------------
;; Note this file is generated from the README.org file. Any changes made to
;; this file will be wiped when new changes are added to the README.org. Please
;; edit the README.org for any changes you desire.
;; -----------------------------------------------------------------------------

(setq inhibit-startup-message t)

;; Clean up the UI
(if (display-graphic-p)
    (scroll-bar-mode -1))
(tool-bar-mode -1)
(tooltip-mode -1)

(require 'fringe)
(set-fringe-mode 10)

(menu-bar-mode -1)

;; Flash instead of beep
(setq visible-bell t)

;; Show line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Spaces > tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq c-basic-offset 2)
(setq sh-basic-offset 2)

;; Set frame transparency
;; (set-frame-parameter (selected-frame) 'alpha '(96 . 92))
;; (add-to-list 'default-frame-alist '(alpha . (96 . 92)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set org-mode for scratch buffer
(setq initial-major-mode 'org-mode)

;; Set default scratch buffer text
(setq initial-scratch-message "\
#+TITLE: Scratch
This buffer is for text that is not saved, and for Lisp evaluation.
To create a file, visit it with C-x C-f and enter text in its buffer.

")

(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)

;; Change where temp files are stored
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(global-set-key (kbd "C-x !") 'toggle-maximize-buffer)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package ef-themes
  :init (load-theme 'ef-light t))

;(load-theme 'whiteboard t)

;(use-package afternoon
  ;:init (load-theme 'afternoon t))

;(use-package doom-themes
  ;:init (load-theme 'doom-palenight t))

(use-package doom-modeline
  :init (doom-modeline-mode t)
  :custom ((doom-modeline-height 10)))

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
        ("C-x b" . counsel-switch-buffer))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(dolist (mode '(eshell-mode-hook
                shell-mode-hook
                term-mode-hook
                vterm-mode-hook
                treemacs-mode-hook))
  (add-hook mode(lambda() (display-line-numbers-mode 0))))

(use-package vterm)

;; Make org mode auto new line after the 80th character
(add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Updated last_modified heading if present after file save
(add-hook 'org-mode-hook (lambda ()
                           (setq-local time-stamp-active t
                                       time-stamp-line-limit 8
                                       time-stamp-start "^#\\+last_modified: [ \t]*"
                                       time-stamp-end "$"
                                       time-stamp-format "\[%Y-%m-%d %a %H:%M\]")
                           (add-hook 'before-save-hook 'time-stamp nil 'local)))

(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)

;; Automatically tangle our Emacs.org config file when we save it
(defun org-babel-tangle-config ()
  (when (or (string-equal (buffer-file-name)
                          (expand-file-name "~/.config/emacs/README.org"))
            (string-equal (buffer-file-name)
                          (expand-file-name "~/.config/emacs/exwm/README.org")))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

;; Run org-babel-tangle-config function after save of org file
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'org-babel-tangle-config)))

(use-package jupyter)

;; This environment variable needs to be set for jupyter to load properly
(setenv "PYDEVD_DISABLE_FILE_VALIDATION" "1" 1)

;;(setq python-shell-interpreter "ipython"
;;      python-shell-interpreter-args "-i --simple-prompt")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)
   (jupyter . t)))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(setq org-auto-tangle-default t)

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "✸" "○")))

(add-hook 'org-mode-hook 'flyspell-mode)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("ba" . "src bash"))
;; Remove "C" structure template to map "C" to "src C"
(delete '("C" . "comment") org-structure-template-alist)
(add-to-list 'org-structure-template-alist '("C" . "src c"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("ex" . "example"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("ja" . "src java"))
(add-to-list 'org-structure-template-alist '("js" . "src javascript"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))
(add-to-list 'org-structure-template-alist '("ju" . "src jupyter-python"))
(add-to-list 'org-structure-template-alist '("li" . "src lisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("ya" . "src yaml"))

(use-package org-roam
  :ensure t
  :init
  ;; Disable v2 warning message
  (setq org-roam-v2-ack t)
  :custom
  ;; My Roam Notes directory
  (org-roam-directory "~/Notes")
  (org-roam-capture-templates
   ;; My default org-roam template
   '(("a" "AI" plain
      (file "~/Notes/RoamTemplates/DefaultTemplate.org")
      :if-new (file+head
               "Content/${slug}.org"
               "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: AI ArtificialIntelligence")
      :unnarrowed t)
     ("c" "Concepts" plain
      (file "~/Notes/RoamTemplates/DefaultTemplate.org")
      :if-new (file+head
               "Content/${slug}.org"
               "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: Concept")
      :unnarrowed t)
     ("d" "Default" plain
      (file "~/Notes/RoamTemplates/DefaultTemplate.org")
      :if-new (file+head
               "Content/${slug}.org"
               "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: ")
      :unnarrowed t)
     ("m" "Mathematics" plain
      (file "~/Notes/RoamTemplates/DefaultTemplate.org")
      :if-new (file+head
               "Content/${slug}.org"
               "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: Math Mathematics")
      :unnarrowed t)
     ("o" "One Off" plain
      (file "~/Notes/RoamTemplates/DefaultTemplate.org")
      :if-new (file+head
               "Content/${slug}.org"
               "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: OneOff")
      :unnarrowed t)
     ("P" "Person" plain
      (file "~/Notes/RoamTemplates/DefaultTemplate.org")
      :if-new (file+head
               "Content/${slug}.org"
               "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: Person")
      :unnarrowed t)
     ("l" "Programming Language" plain
      (file "~/Notes/RoamTemplates/DefaultTemplate.org")
      :if-new (file+head
               "Content/${slug}.org"
               "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: ProgrammingLanguage")
      :unnarrowed t)
     ("b" "Programming Language Basics" plain
      (file "~/Notes/RoamTemplates/DefaultTemplate.org")
      :if-new (file+head
               "Content/${slug}.org"
               "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: ProgrammingLanguage Basics")
      :unnarrowed t)
     ("p" "Project" plain
      (file "~/Notes/RoamTemplates/ProjectTemplate.org")
      :if-new (file+head
               "Projects/${slug}.org"
               "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: Project")
      :unnarrowed t)
     ("r" "Reference" plain
      (file "~/Notes/RoamTemplates/DefaultTemplate.org")
      :if-new (file+head
               "Content/${slug}.org"
               "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: Reference")
      :unnarrowed t)
     ("t" "Tool" plain
      (file "~/Notes/RoamTemplates/DefaultTemplate.org")
      :if-new (file+head
               "Content/${slug}.org"
               "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n#+filetags: Tool")
      :unnarrowed t)
   ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:20}" 'face 'org-tag)))

(use-package org-roam-ui)

(use-package weblorg)

(add-hook 'markdown-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'flyspell-mode)

(add-hook 'css-mode-hook
          (lambda()
            (setq tab-width 2)
            (setq css-indent-offset 2)))
(add-hook 'css-mode-hook 'lsp)

(add-hook 'html-mode-hook 'lsp)

;; add support for "gohtml" files
(setq web-mode-engines-alist
      '(("go"    . "\\.gohtml\\'"))
)

(add-hook 'js-mode-hook
          (lambda()
            (setq tab-width 2)
            (setq js-indent-level 2)))
(add-hook 'js-mode-hook 'lsp)

(use-package docker-compose-mode)

(use-package terraform-mode)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package yasnippet)

(use-package jsonnet-mode)

(use-package mermaid-mode)

(use-package lua-mode)

(use-package go-mode)

(use-package protobuf-mode)

(use-package dockerfile-mode)

(use-package jinja2-mode)

(defun lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-;")
  :config
  (lsp-enable-which-key-integration t))

(use-package sly)

;; Add hook to go-mode for lsp and yasnippet
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

;; Set tab width and before-save-hook for go-mode
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)))

(electric-pair-mode 1)

(add-hook 'org-mode-hook (lambda ()
                           (setq-local electric-pair-inhibit-predicate
                                       `(lambda (c)
                                          (if (char-equal c ?<) t
                                            (,electric-pair-inhibit-predicate c))))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package company
  :after lsp-mode
  :hook ((lisp-mode lsp-mode) . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Install lsp-pyright
(use-package lsp-pyright
  :after lsp-mode)

(defun pyenv-pyright-config ()
  "Write a 'pyrightconfig.json' file with pyenv environment"
  (interactive)
  (setq current-dir (nth 1 (split-string (pwd) " ")))
  (setq pyenv-file (concat current-dir ".python-version"))
  (if (file-exists-p pyenv-file)
      (let* ((venv (f-read-text pyenv-file))
             (venvPath (file-truename "~/.pyenv/versions"))
             (out-file (concat current-dir "pyrightconfig.json"))
             (out-contents (json-encode
                            (list :venvPath venvPath :venv
                                  (substring venv 0 -1)))))
        (with-temp-file out-file (insert out-contents)))
    (message "No .python-version in current directory, skipping..."))
  )

;; Add hook to python-mode for lsp
(add-hook 'python-mode-hook #'lsp-deferred)

(use-package pyvenv)

;; jsonnet-language-server -- LSP registration for Emacs lsp-mode.
;; Commentary:
;; Code:
(require 'jsonnet-mode)
(require 'lsp-mode)

(defcustom lsp-jsonnet-executable "jsonnet-language-server"
  "Command to start the Jsonnet language server."
  :group 'lsp-jsonnet
  :risky t
  :type 'file)

;; Configure lsp-mode language identifiers.
(add-to-list 'lsp-language-id-configuration '(jsonnet-mode . "jsonnet"))
(add-to-list 'lsp-language-id-configuration '(jsonnet-mode . "libsonnet"))

;; Register jsonnet-language-server with the LSP client.
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-jsonnet-executable))
  :activation-fn (lsp-activate-on (and "jsonnet" "libsonnet"))
  :server-id 'jsonnet))

;; Start the language server whenever jsonnet-mode is used.
(add-hook 'jsonnet-mode-hook #'lsp-deferred)

(provide 'jsonnet-language-server)
;;; jsonnet-language-server.el ends here

;; magit configuration
(use-package magit)

;; auto-package-update lets you update your installed packages
(use-package auto-package-update)

(use-package exec-path-from-shell)

;; On MacOS make GUI emacs load user environment
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Get ssh agent from user shell
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
