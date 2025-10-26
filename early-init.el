;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Set frame parameters before GUI is initialized (fixes GNOME Wayland launcher issue)
;; Only apply on GNOME to avoid affecting other systems
(when (and (getenv "XDG_CURRENT_DESKTOP")
           (string-match-p "GNOME" (getenv "XDG_CURRENT_DESKTOP")))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (if (display-graphic-p)
      (scroll-bar-mode -1))
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (require 'fringe)
  (set-fringe-mode 10)
  (menu-bar-mode -1))
