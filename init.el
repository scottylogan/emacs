;;; ----- GENERIC SETTINGS
(setq default-directory "~/")
(setq inhibit-splash-screen t)
(setq-default display-line-numbers t)

(load-file (expand-file-name "elpaca.el" user-emacs-directory))

; show colum number in mode line
(column-number-mode)
(scroll-bar-mode -1)

; add newlines automatically at the end of files
(setq require-final-newline t)

; highlight trailing spaces
(setq show-trailing-whitespace t)

; highlight end of buffer
(setq-default indicate-empty-lines t)


; kill-line (C-k) kill whole line inc. newline, when used at the beginning of a line
(setq kill-whole-line t)

; disable tool bar and tooltips
(tool-bar-mode -1)
(tooltip-mode -1)

; disable menu bar when running in a terminal
(when (not window-system)
  (menu-bar-mode -1))

; enable downcase-region function (C-x C-l)
(put 'downcase-region 'disabled nil)

; disable double spaces at sentence ends
(setq sentence-end-double-space nil)


;;; ----- BACKUPS AND AUTOSAVE

;; from https://idiomdrottning.org/bad-emacs-defaults

(make-directory "~/.emacs_backups/" t)
(make-directory "~/.emacs_autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs_autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs_backups/")))
(setq backup-by-copying t)


(add-to-list 'default-frame-alist '(height . 38))
(add-to-list 'default-frame-alist '(width . 160))

;;; ----- GLOBAL KEY MAPPING

(global-set-key (kbd "s-n") #'scotty-new-empty-frame)
(global-set-key (kbd "s-r") #'replace-string)
(global-set-key [(control h)] 'delete-backward-char)
;(keyboard-translate ?\C-h ?\C-?)

(when (window-system)
  (load-theme 'tango-dark))


;;; ----- FRAMES

(setq frame-title-format "%f")

(defun scotty-frame-setup (&optional frame)
  "Configure new frames."
  (when (and (eq system-type 'darwin) (eq window-system 'ns))
    (set-face-foreground 'mode-line "black")
    (set-face-background 'mode-line "orange")
    (set-face-foreground 'mode-line-inactive "gray")
    (set-face-background 'mode-line-inactive "graphite")

    ;; default Latin font
    (set-face-attribute 'default frame
                        :family "Iosevka Slab"
                        :slant 'normal
                        :height (cond ((< (display-pixel-height) 1080) 160)
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
                "/opt/homebrew/bin"
                "/opt/local/bin"
                "/usr/bin"
                "/bin"
                "/usr/local/sbin"
                "/opt/homebrew/sbin"
                "/opt/local/sbin"
                "/usr/sbin"
                "/sbin"
                "/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14"
                "/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_14"
                "/Applications/Emacs.app/Contents/MacOS/libexec"
                "/Applications/Emacs.app/Contents/MacOS/bin"
                ))

;;; ----- PACKAGES

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode))

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode))

;; build dependency tree for function
(use-package lsp-mode
    :ensure t
    :hook ((go-mode . lsp)
           (c-mode . lsp)
           (c++-mode . lsp))
    :commands lsp)
(setq lsp-warn-no-matched-clients nil)

(use-package lsp-ui :ensure t)
(use-package ansi :ensure t)

(use-package apache-mode :ensure t)
(use-package apt-sources-list :defer t)

(use-package cask-mode :ensure t)
(use-package docker :ensure t)
;;;(use-package s)
(use-package dockerfile-mode :ensure t)
(use-package fontawesome :ensure t)

(use-package freeradius-mode :ensure t)
(use-package go-mode :ensure t)


;;;(use-package vertico
;;;  :ensure t
;;;  :init
;;;  (vertico-mode))

(use-package bison-mode :ensure t)
(use-package ebnf-mode :ensure t)
(use-package google-this :ensure t)
(use-package haskell-mode :ensure t)
;;;(use-package homebrew-mode :ensure t)
(use-package ini-mode :ensure t)
(use-package ipcalc :ensure t)
(use-package launchctl :ensure t)
(use-package legalese :ensure t)
(use-package markdown-mode :ensure t)
(use-package markdown-toc :ensure t)

;;; Node / Javascript
(use-package mocha :ensure t)
(use-package nodejs-repl :ensure t)
(use-package npm-mode :ensure t)
(use-package jq-mode :ensure t)
(use-package js-doc :ensure t)
(use-package jq-format :ensure t)
(use-package js2-mode :ensure t)
(use-package json-mode :ensure t)
(use-package npm :ensure t)

;;;(use-package json-navigator :ensure t)

;;; Git
(elpaca magit :ensure t)
(elpaca magit-vcsh :ensure t)
(elpaca forge :ensure t)
(elpaca gist :ensure t)
(elpaca git :ensure t)
(elpaca git-modes :ensure t)
(elpaca git-timemachine :ensure t)
;;; github
(elpaca github-clone :ensure t)
(elpaca github-explorer :ensure t)
;;; gitlab
(elpaca gitlab :ensure t)
(elpaca gitlab-ci-mode :ensure t)
(elpaca gitlab-ci-mode-flycheck :ensure t)
;;;(use-package gitlab-pipeline :ensure t)

;;; docs and licenses
(elpaca lice :ensure t)                      ; License and Header Template
(elpaca md-readme :ensure t)
(elpaca mermaid-docker-mode :ensure t)
(elpaca mermaid-mode :ensure t)

;;;(use-package package-build)
;;;(use-package lex)
;;;(use-package nasm-mode)
;;;(use-package nhexl-mode)
;;;(use-package powerline)
;;;(use-package projectile)
;;;(use-package pug-mode)

(elpaca memory-usage :ensure t)


;;; various tools
(elpaca makefile-executor :ensure t)
(elpaca puppet-mode :ensure t)
(elpaca systemd :ensure t)
(elpaca terraform-mode :ensure t)
(elpaca vagrant :ensure t)
(elpaca hcl-mode :ensure t)


(elpaca x509-mode :ensure t)

(elpaca xterm-color :ensure t)

(elpaca yaml-mode :ensure t)


(when (eq system-type 'darwin)
  (use-package osx-browse :ensure t)
  (use-package osx-clipboard :ensure t)
  (use-package osx-lib :ensure t)
  (use-package osx-plist :ensure t)
  (use-package pbcopy :ensure t)
;;;  (use-package xcode-mode :ensure t)
  (use-package xcode-project :ensure t))

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

;;; ActivityWatch app helper
(use-package activity-watch-mode
  :ensure t
  :init (global-activity-watch-mode))

;;; ----- SERVER

(require 'server)
(setq server-name user-login-name)

(when (and (window-system) (not (server-running-p)))
  (server-start))

;;; ----- Velocity Template Library
;;; from https://cwiki.apache.org/confluence/display/velocity/EmacsVtlMode

;;;(add-to-list 'load-path "~/.emacs.d/packages/")
;;;(load-library "vtl")
;;;(autoload 'turn-on-vtl-mode "vtl" nil t)
;;;(add-hook 'html-mode-hook 'turn-on-vtl-mode t nil)
;;;(add-hook 'xml-mode-hook 'turn-on-vtl-mode t nil)
;;;(add-hook 'text-mode-hook 'turn-on-vtl-mode t nil)

;;; other things that might be interesting
;;;
;;; https://github.com/lewang/ws-butler
;;;
;;; https://github.com/emacscollective/no-littering
;;;
;;; display-line-numbers, display-line-numbers-mode,
;;; and global-display-line-numbers-mode

