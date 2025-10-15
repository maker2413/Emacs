;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Set frame parameters before GUI is initialized (fixes GNOME Wayland launcher issue)
;; Only apply on GNOME to avoid affecting other systems
(when (and (getenv "XDG_CURRENT_DESKTOP")
           (string-match-p "GNOME" (getenv "XDG_CURRENT_DESKTOP")))
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))
