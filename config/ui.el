(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-autoface-mode nil)
 '(aquamacs-customization-version-id 305 t)
 '(aquamacs-tool-bar-user-customization nil t)
 '(default-frame-alist
    (quote
     ((cursor-type . box)
      (vertical-scroll-bars . right)
      (internal-border-width . 0)
      (modeline . t)
      (fringe)
      (mouse-color . "black")
      (cursor-color . "Red")
      (background-mode . light)
      (tool-bar-lines . 1)
      (menu-bar-lines . 1)
      (right-fringe . 11)
      (left-fringe . 3)
      (background-color . "White")
      (foreground-color . "Black")
      (font . "-*-Source Code Pro-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
      (fontsize . 0)
      (font-backend mac-ct ns))))
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(visual-line-mode nil t))

; Frame and window management:
 
(tabbar-mode -1)		     ; no tabbar
(one-buffer-one-frame-mode -1)       ; no one-buffer-per-frame
(setq special-display-regexps nil)   ; do not open certain buffers in special windows/frames
; (smart-frame-positioning-mode -1)  ; do not place frames behind the Dock or outside of screen boundaries
 
(tool-bar-mode 0) ; turn off toolbar
(menu-bar-mode 0) ; turn off menubar
; (scroll-bar-mode -1)  ; no scrollbars
 
;; Appearance
 
(set-face-attribute 'mode-line nil :inherit 'unspecified) ; show modeline in Monaco
(set-face-attribute 'echo-area nil :family 'unspecified)  ; show echo area in Monaco
 
 
;; Editing
 
; (global-smart-spacing-mode -1)  ; not on by default
; (remove-hook 'text-mode-hook 'smart-spacing-mode)   ; do not use smart spacing in text modes
; (global-visual-line-mode -1)  ; turn off Emacs 23 visual line
; (cua-mode nil)
; (transient-mark-mode nil)  ; (must switch off CUA mode as well for this to work)

