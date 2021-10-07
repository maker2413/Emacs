(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(menu-bar-mode -1)

(setq visible-bell t)

(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 140)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 175 :weight 'regular)

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

;; Magit configuration -------------------------------------------------------------
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

;; Org mode configuration ----------------------------------------------------------
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  ;;  (auto-fill-mode 1)
  ;;  (visual-line-mode 1)
  )

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(with-eval-after-load 'org-faces
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files
        '("~/Code/Hephaestmacs/OrgFiles/Tasks.org")
        '("~/Code/Hephaestmacs/OrgFiles/Birthdays.org"))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;; Configure custom agenda views
  ;; (setq org-agenda-custom-commands
  ;;  '(("d" "Dashboard"
  ;;    ((agenda "" ((org-deadline-warning-days 7)))
  ;;     (todo "NEXT"
  ;;       ((org-agenda-overriding-header "Next Tasks")))
  ;;     (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

  ;;   ("n" "Next Tasks"
  ;;    ((todo "NEXT"
  ;;       ((org-agenda-overriding-header "Next Tasks")))))

  ;;   tasks tagged with work - tasks tagged with email
  ;;   ("W" "Work Tasks" tags-todo "+work-email")

  ;;   ;; Low-effort next actions
  ;;   ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
  ;;    ((org-agenda-overriding-header "Low Effort Tasks")
  ;;     (org-agenda-max-todos 20)
  ;;     (org-agenda-files org-agenda-files)))

  ;;   ("w" "Workflow Status"
  ;;    ((todo "WAIT"
  ;;           ((org-agenda-overriding-header "Waiting on External")
  ;;            (org-agenda-files org-agenda-files)))
  ;;     (todo "REVIEW"
  ;;           ((org-agenda-overriding-header "In Review")
  ;;            (org-agenda-files org-agenda-files)))
  ;;     (todo "PLAN"
  ;;           ((org-agenda-overriding-header "In Planning")
  ;;            (org-agenda-todo-list-sublevels nil)
  ;;            (org-agenda-files org-agenda-files)))
  ;;     (todo "BACKLOG"
  ;;           ((org-agenda-overriding-header "Project Backlog")
  ;;            (org-agenda-todo-list-sublevels nil)
  ;;            (org-agenda-files org-agenda-files)))
  ;;     (todo "READY"
  ;;           ((org-agenda-overriding-header "Ready for Work")
  ;;            (org-agenda-files org-agenda-files)))
  ;;     (todo "ACTIVE"
  ;;           ((org-agenda-overriding-header "Active Projects")
  ;;            (org-agenda-files org-agenda-files)))
  ;;     (todo "COMPLETED"
  ;;           ((org-agenda-overriding-header "Completed Projects")
  ;;            (org-agenda-files org-agenda-files)))
  ;;     (todo "CANC"
  ;;           ((org-agenda-overriding-header "Cancelled Projects")
  ;;            (org-agenda-files org-agenda-files)))))))

  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "✸" "○")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)
