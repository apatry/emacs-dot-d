;;; DO NOT EDIT THIS FILE, USE emacs.org INSTEAD.

;; Run the elisp code in our main configuration file

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun my/last-modified (file)
  "Return the last modification time of FILE or nil if the file can't be read."
   (file-attribute-modification-time (file-attributes file)))

(defun my/is-newer (file1 file2)
  "Return t when FILE1 is newer than FILE2 or FILE2 doesn't exist."
  (let
    ((time-file1 (my/last-modified file1))
     (time-file2 (my/last-modified file2)))
    (or (not time-file2) (time-less-p time-file2 time-file1))))

(let
    ((source "~/.emacs.d/emacs.org")
     (compiled "~/.emacs.d/emacs.el"))
  (if (my/is-newer source compiled)
      (progn
	(message "Tangling %s to %s." source compiled)
	(org-babel-load-file source))
    (progn
      (message "Skip compilation of %s, reusing %s." source compiled)
      (load compiled))))
