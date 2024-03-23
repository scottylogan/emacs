;;; ----- GENERIC SETTINGS
(setq default-directory "~/")
(setq inhibit-splash-screen t)
(setq-default display-line-numbers t)

(put 'downcase-region 'disabled nil)

(tool-bar-mode -1)
(tooltip-mode -1)
(column-number-mode)
(scroll-bar-mode -1)

(when (not window-system)
  (menu-bar-mode -1))

(add-to-list 'default-frame-alist '(height . 38))
(add-to-list 'default-frame-alist '(width . 160))

;;; ----- GLOBAL KEY MAPPING

(global-set-key (kbd "s-n") #'scotty-new-empty-frame)
(global-set-key (kbd "s-r") #'replace-string)


;;; ----- FRAMES

(setq frame-title-format "%b - %f")
;;;(set-frame-font "Iosevka Slab-16:antialias=1:slant=normal")
;;;(set-frame-font "SF Mono:antialias=1:weight=light")
;;;(set-face-font 'default "-*-SF Mono-regular-normal-normal-*-16-*")
;;;(set-face-attribute 'default nil :family "SF Mono")
(set-face-attribute 'default nil
                    :family "Iosevka Slab"
                    :weight 'regular
                    :slant 'normal
                    :height (cond ((< (display-pixel-height) 1080) 180)
                                  ((>= (display-pixel-height) 2160) 300)
                                  (t 220)))

(when (window-system)
  (load-theme 'tango-dark))

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

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(when (not (package-installed-p 'diminish))
  (package-install 'diminish))

(when (not (package-installed-p 'bind-key))
  (package-install 'bind-key))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(custom-set-variables
 '(use-package-always-ensure t))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package bison-mode)
(use-package dockerfile-mode)
(use-package ebnf-mode)
(use-package fontawesome)
(use-package freeradius-mode)
(use-package git-modes)
(use-package git-timemachine)
(use-package github-clone)
(use-package github-explorer)
(use-package gitlab)
(use-package gitlab-ci-mode)
(use-package gitlab-ci-mode-flycheck)
(use-package gitlab-pipeline)
(use-package google-this)
(use-package go-mode)
(use-package haskell-mode)
(use-package hcl-mode)
(use-package homebrew-mode)
(use-package ini-mode)
(use-package ipcalc)
(use-package jq-format)
(use-package jq-mode)
(use-package js2-mode)
(use-package json-mode)
(use-package json-navigator)
(use-package legalese)                  ; License file stuff
(use-package lex)
(use-package lice)                      ; License and Header Template
(use-package magit)
(use-package magit-vcsh)
(use-package magithub)
(use-package makefile-executor)
(use-package markdown-mode)
(use-package markdown-toc)
(use-package md-readme)
(use-package memory-usage)
(use-package mermaid-docker-mode)
(use-package mermaid-mode)
(use-package mocha)
(use-package nasm-mode)
(use-package nhexl-mode)
(use-package nodejs-repl)
(use-package npm)
(use-package npm-mode)
(use-package powerline)
(use-package projectile)
(use-package pug-mode)
(use-package puppet-mode)
(use-package systemd)
(use-package terraform-mode)
(use-package yaml-mode)


(use-package osx-browse
  :if (eq system-type 'darwin))
(use-package osx-clipboard
  :if (eq system-type 'darwin))
(use-package osx-lib
  :if (eq system-type 'darwin))
(use-package osx-plist
  :if (eq system-type 'darwin))
(use-package xcode-project
  :if (eq system-type 'darwin))

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
(define-key global-map (kbd "C-c c") 'org-capture)

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

(setq server-name "none")

(when (not (or (fboundp 'server-running-p)
               (not window-system)))
  (setq server-name user-login-name)
  (server-start))


;;; ----- Velocity Template Library
;;; from https://cwiki.apache.org/confluence/display/velocity/EmacsVtlMode

;;;(add-to-list 'load-path "~/.emacs.d/packages/")
;;;(load-library "vtl")
;;;(autoload 'turn-on-vtl-mode "vtl" nil t)
;;;(add-hook 'html-mode-hook 'turn-on-vtl-mode t nil)
;;;(add-hook 'xml-mode-hook 'turn-on-vtl-mode t nil)
;;;(add-hook 'text-mode-hook 'turn-on-vtl-mode t nil)

