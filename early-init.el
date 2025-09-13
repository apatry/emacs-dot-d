;; We now use straight for package management. We thus disable package
;; to avoid conflict between package.el and straight.el.

(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)
