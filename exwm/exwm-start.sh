#!/bin/sh
# Run the screen compositor
picom &

# Enable screen locking on suspend
xss-lock -- slock &

# Start ssh-agent so that we don't have to type the passphrase all the time
eval `ssh-agent`
ssh-add &

# Start exwm
exec dbus-launch --exit-with-session emacs -mm --debug-init -l /home/alex/.emacs.d/exwm/exwm.el
