;; Enable window switching with mouse hover
(setq mouse-autoselect-window t)

;; Function to set buffer name to appliction name
(defun exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

;; Function to launch application in the background
(defun run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts)
                            nil 0 nil
                            ,@(cdr command-parts)))))

;; Initialization
(defun exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Show the time and date in modeline
  (setq display-time-day-and-date t)
  (display-time-mode 1)
  ;; Also take a look at display-time-format and format-time-string

  ;; Launch apps that will run in the background
  (run-in-background "nitrogen --restore")
  (run-in-background "picom")
  )

(use-package exwm
  :config
  ;; Set resolution
  (run-in-background "xrandr -s 1920x1080")

  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'exwm-update-class)

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'exwm-init-hook)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x))

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)

  ;; (setq exwm-systemtray-height 32)
  (exwm-systemtray-enable)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([?\s-b] . windmove-left)
          ([?\s-f] . windmove-right)
          ([?\s-p] . windmove-up)
          ([?\s-n] . windmove-down)

          ;; Switch between line and char mode
          ([?\s-i] . exwm-input-toggle-keyboard)

          ;; Toggle Fullscreen
          ([?\s-F] . exwm-layout-toggle-fullscreen)

          ;; Switch workspaces
          ([?\s-w] . exwm-workspace-switch)
          ([s-escape] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-x") 'counsel-linux-app)

  (exwm-enable))
