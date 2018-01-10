(require 'package)

(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

(package-initialize)

(when (eq system-type 'darwin)
  ;; default Latin font
  (set-face-attribute 'default nil :family "SF Mono")
  (set-face-attribute 'default nil :height 160)
  )

(setq custom-file "~/.emacs.d/custom.el")
;; keep this last
(load custom-file)


