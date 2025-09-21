;; ----- BACKUPS AND AUTOSAVE

;; from https://idiomdrottning.org/bad-emacs-defaults

;; put auto-saved files under ~/.emacs_autosave
(make-directory "~/.emacs_autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs_autosave/" t)))

;; save backup files under ~/.emacs_backups, with versioning
;; https://www.emacswiki.org/emacs/BackupDirectory

(make-directory "~/.emacs_backups/" t)
(setq backup-directory-alist '(("." . "~/.emacs_backups/")))

(setq backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; use versioned backups
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

