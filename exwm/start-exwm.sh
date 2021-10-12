#!/bin/sh

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.config/emacs/exwm/exwm-configuration.el
