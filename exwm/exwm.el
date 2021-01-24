(defun my/run-in-background (command)
    "Run COMMAND in the background. This function is not robust to spaces in command arguments."
    (let ((command-parts (split-string command "[ ]+")))
      (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

  (defun my/exwm-update-title ()
    "Update the buffer name to the one of the window."
    (exwm-workspace-rename-buffer
     (if exwm-title
	 (format "%s: %s" exwm-class-name exwm-title)
       exwm-class-name)))

(defun my/update-displays ()
  "Call autorandr to update the display setting."
  (my/run-in-background "autorandr --change --force")
  (message "Display config: %s"
	   (string-trim (shell-command-to-string "autorandr --current"))))

(defun my/exwm-init ()
  "Initialize exwm to my tastes."

  ;; Start an emacs server. This is used for IPC with polybar
  (server-start)

  ;; Let's remove emacs menu bar, it is wasing useful estate.
  (menu-bar-mode -1)

  ;; start the Polybar panel
  (start-process-shell-command "polybar" nil "polybar panel")
  (my/run-in-background "nm-applet")
  (my/run-in-background "pasystray")
  (my/run-in-background "blueman-applet")

  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1))

(use-package exwm
  :ensure t
  :hook
  ((exwm-update-class . my/exwm-update-title)
   (exwm-update-title . my/exwm-update-title)
   (exwm-init . my/exwm-init))

  :init
  ;; Sent the mouse to the selected workspace display
  (setq exwm-worspace-warp-cursor t)

  ;; Focus follow the mouse
  (setq focus-follows-mouse t)

  ;; Set the screen resolution
  (require 'exwm-randr)
  (exwm-randr-enable)
  (add-hook 'exwm-randr-screen-change-hook #'my/update-displays)
  (my/update-displays)

  ;; Only workspace 0 is on the laptop screen when it is docked
  (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1"))

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
	'(?\C-x
	  ?\C-u
	  ?\C-h
	  ?\M-x
	  ?\M-`
	  ?\M-&
	  ?\M-:
	  ?\C-\M-j  ;; Buffer list
	  ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
	`(
	  ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
	  ([?\s-r] . exwm-reset)

	  ;; Move between windows
	  ([s-left] . windmove-left)
	  ([s-right] . windmove-right)
	  ([s-up] . windmove-up)
	  ([s-down] . windmove-down)

	  ;; Switch workspace
	  ([?\s-w] . exwm-workspace-switch)

	  ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
	  ,@(mapcar (lambda (i)
		      `(,(kbd (format "s-%d" i)) .
			(lambda ()
			  (interactive)
			  (exwm-workspace-switch-create ,i))))
		    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)

  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :init (desktop-environment-mode)
  :ensure t)

(defun efs/send-polybar-hook (module-name hook-index)
  "Send a message to polybar to execute HOOK-INDEX for MODULE-NAME."
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun efs/send-polybar-exwm-workspace ()
  (efs/send-polybar-hook "exwm-workspace" 1))

  ;; Update panel indicator when workspace changes
  (add-hook 'exwm-workspace-switch-hook #'efs/send-polybar-exwm-workspace)
