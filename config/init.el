(add-to-list 'load-path "~/share/emacs")
(add-to-list 'load-path "~/share/emacs/puppet-syntax-emacs")

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

;;;Add the following custom-set-variables to use 'lazy' modes
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js3-lazy-commas t)
 '(js3-lazy-operators t)
 '(js3-lazy-dots t)
 '(js3-expr-indent-offset 2)
 '(js3-paren-indent-offset 2)
 '(js3-square-indent-offset 2)
 '(js3-curly-indent-offset 2)
)

(autoload 'js3-mode "js3-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))


(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;;; from http://www.emacswiki.org/emacs/AquamacsEmacsCompatibilitySettings

;; Key bindings
 
(osx-key-mode -1)  ; no Mac-specific key bindings

(define-key osx-key-mode-map "\C-z" nil)  ;; reinstate  C-z to minimize 
 ;; (define-key osx-key-mode-map "\C-z" 'ns-do-hide-emacs)  ;; C-z to hide the application
 
(unless window-system   ;; in TTY (terminal) mode
  (normal-erase-is-backspace-mode nil)
  (set-face-inverse-video-p 'mode-line-inactive t)
  (define-key osx-key-mode-map "\C-z" 'suspend-emacs))

(setq
  ns-command-modifier 'meta         ; Apple/Command key is Meta
  ns-alternate-modifier nil         ; Option is the Mac Option key
  ns-use-mac-modifier-symbols  nil  ; display standard Emacs (and not standard Mac) modifier symbols)
)
 
;; Persistency and modes:
(setq
  aquamacs-scratch-file nil                        ; do not save scratch file across sessions
  initial-major-mode 'emacs-lisp-mode              ; *scratch* shows up in emacs-lisp-mode
  ;; aquamacs-default-major-mode 'emacs-lisp-mode  ; new buffers open in emacs-lisp-mode
)
 
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

(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/share/emacs/ac-dict")
(require 'auto-complete-config)
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
(setq ac-auto-start 2)
(setq ac-ignore-case nil)
(ac-config-default)


(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

; start emacs server
(server-start)
