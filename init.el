;;; ----- GENERIC SETTINGS
(setq default-directory "~/")
;;;(setq custom-file "~/.emacs.d/custom.el")
(setq inhibit-splash-screen t)
(setq-default display-line-numbers t)

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

;;; ----- straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package company)

(add-hook 'after-init-hook 'global-company-mode)

;; build dependency tree for function
(use-package lsp-mode
    :ensure t
    :hook ((go-mode . lsp)
           (c-mode . lsp)
           (c++-mode . lsp))
    :commands lsp)
(setq lsp-warn-no-matched-clients nil)

(use-package lsp-ui)
(use-package ansi)
(use-package apache-mode)
(use-package apt-sources-list)
(use-package cask)
(use-package cask-mode)
(use-package dayone)
(use-package docker)
(use-package s)
(use-package dockerfile-mode)
(use-package flymake-jshint)
(use-package flymake-json)
(use-package flymake-puppet)
(use-package flymake-shell)
(use-package flymake-yaml)
(use-package fontawesome)
(use-package freeradius-mode)
(use-package gist)
(use-package git)
(use-package go-mode)

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(when (not (package-installed-p 'bind-key))
  (package-install 'bind-key))

(eval-when-compile
  (require 'use-package))
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

;;;(use-package vertico
;;;  :ensure t
;;;  :init
;;;  (vertico-mode))

(use-package bison-mode)
(use-package ebnf-mode)
(use-package git-modes)
(use-package git-timemachine)
(use-package github-clone)
(use-package github-explorer)
(use-package gitlab)
(use-package gitlab-ci-mode)
(use-package gitlab-ci-mode-flycheck)
(use-package gitlab-pipeline)
(use-package google-this)
(use-package haskell-mode)
(use-package hcl-mode)
(use-package homebrew-mode)
(use-package ini-mode)
(use-package ipcalc)
(use-package jq-mode)
(use-package js-doc)
(use-package launchctl)
(use-package ldap-mode)
(use-package legalese)
(use-package markdown-mode)
(use-package markdown-toc)
(use-package mocha)
(use-package nodejs-repl)
(use-package npm-mode)
(use-package osx-browse)
(use-package osx-clipboard)
(use-package osx-lib)
(use-package osx-plist)
(use-package package-build)
(use-package pbcopy)
(use-package jq-format)
(use-package js2-mode)
(use-package json-mode)
;;;(use-package json-navigator)
(use-package legalese)                  ; License file stuff
(use-package lex)
(use-package lice)                      ; License and Header Template
(use-package magit)
(use-package magit-vcsh)
(use-package magithub)
(use-package makefile-executor)
(use-package md-readme)
(use-package memory-usage)
(use-package mermaid-docker-mode)
(use-package mermaid-mode)
(use-package nasm-mode)
(use-package nhexl-mode)
(use-package npm)
(use-package powerline)
(use-package projectile)
(use-package pug-mode)
(use-package puppet-mode)
(use-package systemd)
(use-package terraform-mode)
(use-package vagrant)
(use-package x509-mode)
;;;(use-package xcode-mode)
(use-package xcode-project)
(use-package xterm-color)
;;(use-package xterm-title)
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

;;; other things that might be interesting
;;;
;;; https://github.com/lewang/ws-butler
;;;
;;; https://github.com/emacscollective/no-littering
;;;
;;; display-line-numbers, display-line-numbers-mode,
;;; and global-display-line-numbers-mode

;; keep this last
;;;(load custom-file)
