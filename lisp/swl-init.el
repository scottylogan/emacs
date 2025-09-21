(setq load-path (cons (concat (file-name-directory user-init-file) "/lisp") load-path))

;;; ----- PATH

(setq-default exec-path
              (list
               (substring
                (concat "~/bin/"
                        (symbol-name system-type)
                        "_"
                        (shell-command-to-string "uname -m"))
                0 -1)
                "~/bin"
                "~/.nodenv/shims"
                "/usr/local/bin"
                "/opt/homebrew/bin"
                "/opt/local/bin"
                "/usr/bin"
                "/bin"
                "/usr/local/sbin"
                "/opt/homebrew/sbin"
                "/opt/local/sbin"
                "/usr/sbin"
                "/sbin"
                "/Applications/Emacs.app/Contents/MacOS/libexec"
                "/Applications/Emacs.app/Contents/MacOS/bin"
                ))

(setq shell-file-name (executable-find "bash"))

;; ----- GENERIC SETTINGS
(setq default-directory "~/")

;; add newlines automatically at the end of files
(setq require-final-newline t)

;; highlight trailing spaces
(setq show-trailing-whitespace t)

;; highlight end of buffer
(setq-default indicate-empty-lines t)


;; kill-line (C-k) kill whole line inc. newline, when used at the beginning of a line
(setq kill-whole-line t)

;; enable downcase-region function (C-x C-l)
(put 'downcase-region 'disabled nil)

;; disable double spaces at sentence ends
(setq sentence-end-double-space nil)

;; ----- GLOBAL KEY MAPPING

(global-set-key (kbd "s-n")     #'scotty-new-empty-frame)
(global-set-key (kbd "s-r")     #'replace-string)
(global-set-key (kbd "s-R")     #'replace-regexp)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key [(control h)]   'delete-backward-char)
(global-set-key (kbd "C-s")     'isearch-forward-regexp)
(global-set-key (kbd "C-r")     'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")   'isearch-forward)
(global-set-key (kbd "C-M-r")   'isearch-backward)

;;; ----- SERVER

(require 'server)
(setq server-name user-login-name)

(when (and (window-system) (not (server-running-p)))
  (server-start))

(load "swl-elpaca")
(load "swl-packages")
(load "swl-ui")
(load "swl-backup")
(load "swl-coding")
(load "swl-orgmode")



;;; other things that might be interesting
;;;
;;; https://github.com/lewang/ws-butler
;;;
;;; https://github.com/emacscollective/no-littering
;;;
;;; display-line-numbers, display-line-numbers-mode,
;;; and global-display-line-numbers-mode

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
