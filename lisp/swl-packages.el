;;; ----- PACKAGES

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package vterm
  :ensure t
  :defer t)

(use-package flycheck
  :ensure t
  :defer t
  :init
  (setq flycheck-disabled-checkers '(go-errcheck))
  (global-flycheck-mode))

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

(use-package go-mode :ensure t)
(use-package go-add-tags :ensure t)
(use-package go-autocomplete :ensure t)
(use-package go-complete :ensure t)
(use-package go-dlv :ensure t)
(use-package go-eldoc
  :ensure t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))
(use-package go-errcheck
  :ensure t
  :init
  (setq go-errcheck-ignorepkg '("fmt" "http.ResponseWriter")))

(use-package go-expr-completion :ensure t)
(use-package go-fill-struct :ensure t)
(use-package go-gen-test :ensure t)
(use-package go-guru :ensure t)
(use-package go-imports :ensure t)
(use-package go-playground :ensure t)
(use-package go-rename :ensure t)
(use-package go-projectile :ensure t)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package ivy
  :ensure t
  :init
  (ivy-mode))
(use-package counsel
  :ensure t
  :init
  (counsel-mode))
(use-package neotree
  :ensure t
  :init
  (global-set-key [f8] 'scotty-neotree-project-dir))


;;;(use-package vertico
;;;  :ensure t
;;;  :init
;;;  (vertico-mode))

(use-package ini-mode :ensure t)
(use-package ipcalc :ensure t)
(use-package launchctl :ensure t)
(use-package legalese :ensure t)
(use-package markdown-mode :ensure t)
(use-package markdown-toc :ensure t)
(use-package gh-md :ensure t)

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

;;; Git
(elpaca transient :repo "magit/transient")
(use-package magit :ensure t :after transient)
(use-package magit-vcsh :ensure t)
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

;;; docs and licenses
(use-package lice :ensure t)                      ; License and Header Template
(use-package md-readme :ensure t)
(use-package mermaid-docker-mode :ensure t)
(use-package mermaid-mode :ensure t)

;;; k8s
(use-package k8s-mode :ensure t)
(use-package kubectx-mode :ensure t)
(use-package kubedoc :ensure t)
(use-package kubernetes :ensure t)

(use-package diminish
  :ensure t
  :init
  (mapc 'diminish '(projectile counsel-mode ivy-mode company-mode auto-revert-mode)))

(use-package memory-usage :ensure t)

;;; various tools
;;;(use-package makefile-executor :ensure t)

(use-package systemd :ensure t)

(use-package terraform-mode :ensure t)
(use-package terraform-doc :ensure t)
(use-package hcl-mode :ensure t)

(use-package x509-mode :ensure t)

(use-package xterm-color :ensure t)

(use-package yaml-mode :ensure t)

;;; ActivityWatch app helper
(use-package activity-watch-mode
  :ensure t
  :init (global-activity-watch-mode))

(when (eq system-type 'darwin)
  (use-package osx-browse :ensure t)
  (use-package osx-clipboard :ensure t)
  (use-package osx-lib :ensure t)
  (use-package osx-plist :ensure t)
  (use-package pbcopy :ensure t)
  (use-package xcode-project :ensure t))


