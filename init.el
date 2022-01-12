(require 'package)

(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

(package-initialize)

(setq default-directory "~/")
(setq custom-file "~/.emacs.d/custom.el")
(setq inhibit-splash-screen t)

(setq frame-title-format "%f")

(defun scotty-frame-setup (&optional frame)
  "Configure new frames."
  (when (and (eq system-type 'darwin) (eq window-system 'ns))
    ;; default Latin font
    (set-face-attribute 'default frame :family "Input Mono")
    (set-face-attribute 'default frame :height
                        (cond ((< (display-pixel-height) 1080) 160)
                              ((>= (display-pixel-height) 2160) 300)
                              (t 220)))))
  
(add-hook 'after-make-frame-functions 'scotty-frame-setup)

(scotty-frame-setup)

;; based on https://stackoverflow.com/questions/25791605/emacs-how-do-i-create-a-new-empty-buffer-whenever-creating-a-new-frame

(defun scotty-new-empty-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

(global-set-key (kbd "s-n") #'scotty-new-empty-frame)
(global-set-key (kbd "s-r") #'replace-string)

;;(setq tab-width 2) ; or any other preferred value
(setq-default tab-width 2)
(setq-default c-basic-offset tab-width)
(setq-default c-basic-indent tab-width)
;; set this in all c-based programming modes
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label '+)))

(add-hook 'php-mode-hook #'(lambda() (setq c-basic-offset 2)))
(defvaralias 'cperl-indent-level 'tab-width)
;; keep this last
(load custom-file)

;; save backup files under ~/.saves, with versioning
;; https://www.emacswiki.org/emacs/BackupDirectory
(setq
   backup-by-copying t      ; don't clobber symlinks
   version-control t        ; use versioned backups
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   backup-directory-alist
    `(("." . "~/.saves/"))    ; don't litter my fs tree
   )

(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

(unless (and (fboundp 'server-running-p)
             (server-running-p))
  (server-start))



(put 'downcase-region 'disabled nil)

;;; org-mode setup
;;; https://orgmode.org/worg/org-tutorials/orgtutorial_dto.html
;;; http://pragmaticemacs.com/emacs/org-mode-basics-vi-a-simple-todo-list/
;;; http://doc.norang.ca/org-mode.html

;; set key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; file to save todo items
(setq default-agenda-dir "~/Dropbox/org-mode")
(setq org-agenda-files (list default-agenda-dir))
(setq default-agenda-file
      (concat (file-name-as-directory default-agenda-dir) "todo.org"))


;; set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?B)

;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))

;;capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline default-agenda-file "Tasks")
         "* TODO [#A] %?")))
