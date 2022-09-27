;;; ----- GENERIC SETTINGS
(setq default-directory "~/")
(setq custom-file "~/.emacs.d/custom.el")
(setq inhibit-splash-screen t)

(put 'downcase-region 'disabled nil)

(tool-bar-mode -1)
(tooltip-mode -1)
(column-number-mode)

(when (not window-system)
  (menu-bar-mode -1))


;;; ----- GLOBAL KEY MAPPING

(global-set-key (kbd "s-n") #'scotty-new-empty-frame)
(global-set-key (kbd "s-r") #'replace-string)


;;; ----- FRAMES

(setq frame-title-format "%f")

(defun scotty-frame-setup (&optional frame)
  "Configure new frames."
  (when (and (eq system-type 'darwin) (eq window-system 'ns))
    ;; default Latin font
    (set-face-attribute 'default frame :family "SF Mono")
    (set-face-attribute 'default frame :height
                        (cond ((< (display-pixel-height) 1080) 160)
                              ((>= (display-pixel-height) 2160) 300)
                              (t 220)))))
  
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (scotty-frame-setup)))
  (scotty-frame-setup))

;; based on https://stackoverflow.com/questions/25791605/emacs-how-do-i-create-a-new-empty-buffer-whenever-creating-a-new-frame

(defun scotty-new-empty-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))


;;; ----- PATH

(setq-default exec-path
              '(
                "~/bin/"
                "/usr/local/bin"
                "/usr/bin"
                "/bin"
                "/usr/local/sbin"
                "/usr/sbin"
                "/sbin"
                "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14"
                "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_14"
                "/Applications/Emacs.app/Contents/MacOS/libexec"
                "/Applications/Emacs.app/Contents/MacOS/bin"
                ))

;;; ----- PACKAGES

(require 'package)

(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

(package-initialize)

(setq-default package-selected-packages
              '(
                ac-html
                ac-html-bootstrap
                ac-js2
                ac-php
                ac-php-core
                ansi
                apache-mode
                apt-sources-list
                cask
                cask-mode
                commander
                composer
                dash
                dayone
                docker
                dockerfile-mode
                egg
                epl
                f
                feature-mode
                flymake-jshint
                flymake-jslint
                flymake-json
                flymake-php
                flymake-phpcs
                flymake-puppet
                flymake-shell
                flymake-yaml
                fold-this
                fontawesome
                freeradius-mode
                gist
                git
                git-timemachine
                gitattributes-mode
                gitconfig
                gitconfig-mode
                github-browse-file
                github-clone
                github-issues
                github-theme
                gitignore-mode
                gitlab
                go-mode
                google-this
                groovy-mode
                grunt
                hcl-mode
                homebrew-mode
                ini-mode
                iodine-theme
                ipcalc
                irfc
                jq-mode
                js-auto-beautify
                js-auto-format-mode
                js-doc
                js-format
                js-import
                js2-highlight-vars
                js2-refactor
                js3-mode
                json-navigator
                json-reformat
                jss
                jst
                kubernetes-tramp
                launchctl
                ldap-mode
                legalese
                lice
                logstash-conf
                markdown-mode
                markdown-mode+
                markdown-preview-eww
                markdown-preview-mode
                markdown-toc
                mocha
                mocha-snippets
                mocker
                nodejs-repl
                npm-mode
                osx-browse
                osx-clipboard
                osx-lib
                osx-plist
                package-build
                pbcopy
                php-mode
                php-refactor-mode
                phpunit
                pug-mode
                puppet-mode
                s
                shut-up
                systemd
                terraform-mode
                twilight-anti-bright-theme
                twilight-theme
                vagrant
                x509-mode
                xcode-mode
                xcode-project
                xterm-color
                xterm-title
                yaml-mode
                ))


;;; ----- INDENTATION and PROGRAMMING

(setq-default indent-tabs-mode nil)
(setq-default size-indication-mode t)

(setq-default tab-width 2)

(setq-default apache-indent-level tab-width)
(setq-default c-basic-indent tab-width)
(setq-default c-basic-offset tab-width)
(setq-default freeradius-indent-offset tab-width)
(setq-default js-indent-level tab-width)
(setq-default sh-basic-offset tab-width)
(setq-default sh-indentation tab-width)
(setq-default web-mode-code-indent-offset tab-width)

(add-to-list 'auto-mode-alist '("\\.js\\'"     . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.js.yml\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.vm\\'"     . html-mode))
(add-to-list 'auto-mode-alist '("\\.vm.yml\\'" . html-mode))

;; set this in all c-based programming modes
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label '+)))

(add-hook 'php-mode-hook (lambda() (setq c-basic-offset 2)))
(defvaralias 'cperl-indent-level 'tab-width)

;;; ----- BACKUP FILES

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

;;; ----- ORG-MODE

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

;;; ----- SERVER

(setq server-name user-login-name)

(unless (and (fboundp 'server-running-p)
             (server-running-p))
  (server-start))


;;; ----- Velocity Template Library
;;; from https://cwiki.apache.org/confluence/display/velocity/EmacsVtlMode

(add-to-list 'load-path "~/.emacs.d/packages/")
(load-library "vtl")
(autoload 'turn-on-vtl-mode "vtl" nil t)
(add-hook 'html-mode-hook 'turn-on-vtl-mode t nil)
(add-hook 'xml-mode-hook 'turn-on-vtl-mode t nil)
(add-hook 'text-mode-hook 'turn-on-vtl-mode t nil)


;; keep this last
(load custom-file)
