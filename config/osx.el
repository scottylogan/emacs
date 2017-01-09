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
 
