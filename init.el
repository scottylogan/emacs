;; elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; ----- GENERIC SETTINGS
(setq default-directory "~/")
(setq inhibit-splash-screen t)

;; show line numbers in wide windows
(setq-default display-line-numbers (> (frame-width) 80))

;; show colum number in mode line
(column-number-mode)
(scroll-bar-mode -1)

;; add newlines automatically at the end of files
(setq require-final-newline t)

;; highlight trailing spaces
(setq show-trailing-whitespace t)

;; highlight end of buffer
(setq-default indicate-empty-lines t)


;; kill-line (C-k) kill whole line inc. newline, when used at the beginning of a line
(setq kill-whole-line t)

;; disable tool bar and tooltips
(tool-bar-mode -1)
(tooltip-mode -1)

;; disable menu bar when running in a terminal, or X
;; but leave on for MacOS GUI Emacs
(when (or (not window-system) (eq window-system 'x))
  (menu-bar-mode -1))

;; enable downcase-region function (C-x C-l)
(put 'downcase-region 'disabled nil)

;; disable double spaces at sentence ends
(setq sentence-end-double-space nil)

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

(when (window-system)
  (load-theme 'tango-dark))


;;; ----- FRAMES

(add-to-list 'default-frame-alist '(height . 38))
(add-to-list 'default-frame-alist '(width . 160))
(setq frame-title-format "%f")

(when (or (eq window-system 'ns) (eq window-system 'x))
  (set-face-foreground 'mode-line "black")
  (set-face-background 'mode-line "orange")
  (set-face-foreground 'mode-line-inactive "gray")
  (set-face-background 'mode-line-inactive "graphite")

  ;; default Latin font
  (set-face-attribute 'default nil
                      :family "Iosevka Slab"
                      :slant 'normal
                      :height (cond ((< (display-pixel-height) 1080) 160)
                                    ((>= (display-pixel-height) 2160) 300)
                                    (t 220))))

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
(use-package lsp-ui :ensure t)
(setq lsp-warn-no-matched-clients nil)

(use-package ansi :ensure t)

(use-package apache-mode :ensure t)
(use-package apt-sources-list :defer t)

(use-package cask-mode :ensure t)
;;;(use-package docker :ensure t)
;;;(use-package s)
(use-package dockerfile-mode :ensure t)
(use-package fontawesome :ensure t)

(use-package freeradius-mode :ensure t)
(use-package go-mode :ensure t)
(use-package go-add-tags :ensure t)
(use-package go-autocomplete :ensure t)
(use-package go-complete :ensure t)
(use-package go-dlv :ensure t)
(use-package go-eldoc :ensure t)
(use-package go-errcheck :ensure t)
(use-package go-expr-completion :ensure t)
(use-package go-fill-struct :ensure t)
(use-package go-gen-test :ensure t)
(use-package go-guru :ensure t)
(use-package go-imports :ensure t)
(use-package go-playground :ensure t)

(use-package arduino-cli-mode :ensure t)
(use-package arduino-mode :ensure t)

(use-package verilog-mode :ensure t)
(use-package verilog-ts-mode :ensure t)
(use-package hydra :ensure t)
(use-package verilog-ext :ensure t)

(use-package ansible :ensure t)
(use-package ansible-doc :ensure t)
(use-package flymake-ansible-lint :ensure t)


(use-package forth-mode :ensure t)

(use-package projectile :ensure t)
(use-package go-rename :ensure t)
(use-package go-projectile :ensure t)


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
(use-package gh-md :ensure t)
;;;(use-package jekyll-modes :ensure t)

;;;(use-package copilot-chat :ensure t)


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
(use-package typescript-mode :ensure t)

;;;(use-package json-navigator :ensure t)

;;; Git
(elpaca transient :repo "magit/transient")
(use-package magit :ensure t :after transient)
(use-package magit-vcsh :ensure t)
;;;(use-package forge :ensure t)
(use-package gist :ensure t)
(use-package git :ensure t)
(use-package git-modes :ensure t)
(use-package git-timemachine :ensure t)
;;; github
(use-package github-clone :ensure t)
(use-package github-explorer :ensure t)
;;; gitlab
(use-package gitlab :ensure t)
(use-package gitlab-ci-mode :ensure t)
(use-package gitlab-ci-mode-flycheck :ensure t)
;;;(use-package gitlab-pipeline :ensure t)

;;; docs and licenses
(use-package lice :ensure t)                      ; License and Header Template
(use-package md-readme :ensure t)
(use-package mermaid-docker-mode :ensure t)
(use-package mermaid-mode :ensure t)

(use-package k8s-mode :ensure t)
(use-package kubectx-mode :ensure t)
(use-package kubedoc :ensure t)
(use-package kubernetes :ensure t)

(use-package slime :ensure t)
(use-package slime-docker :ensure t)
(use-package slime-repl-ansi-color :ensure t)

(use-package emacsql :ensure t)
(use-package emacsql-mysql :ensure t)
(use-package emacsql-sqlite :ensure t)


;;;(use-package package-build)
;;;(use-package lex)
;;;(use-package nasm-mode)
;;;(use-package nhexl-mode)
;;;(use-package powerline)
;;;(use-package projectile)
;;;(use-package pug-mode)

(use-package memory-usage :ensure t)

(use-package flymake-shellcheck :ensure t)

(use-package vterm :ensure t)

;;; various tools
;;;(use-package makefile-executor :ensure t)
(use-package puppet-mode :ensure t)
(use-package flymake-puppet :ensure t)
(use-package puppet-ts-mode :ensure t)

(use-package systemd :ensure t)

(use-package terraform-mode :ensure t)
(use-package terraform-doc :ensure t)
(use-package vagrant :ensure t)
(use-package hcl-mode :ensure t)


(use-package x509-mode :ensure t)

(use-package xterm-color :ensure t)

(use-package yaml-mode :ensure t)


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

;;; other things that might be interesting
;;;
;;; https://github.com/lewang/ws-butler
;;;
;;; https://github.com/emacscollective/no-littering
;;;
;;; display-line-numbers, display-line-numbers-mode,
;;; and global-display-line-numbers-mode

