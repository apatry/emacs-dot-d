#!/bin/sh
# Run the screen compositor
picom &

# Enable screen locking on suspend
xss-lock -- slock &

# Start exwm
exec dbus-launch --exit-with-session emacs -mm --debug-init -l /home/alex/.emacs.d/exwm/exwm.el
