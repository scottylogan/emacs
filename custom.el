;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(apache-mode auto-package-update catppuccin-theme company diminish
                 dockerfile-mode doom-modeline exec-path-from-shell
                 flycheck gcmh gh-md go-errcheck go-fill-struct
                 go-gen-test go-lint go-mode go-playground
                 go-projectile go-tag golint htmlize ini-mode js-doc
                 js2-mode json-mode launchctl lice lsp-ivy lsp-mode
                 lsp-treemacs lsp-ui markdown-mode markdown-toc
                 md-readme mermaid-docker-mode mocha nodejs-repl
                 npm-mode org-superstar outline-toc rainbow-delimiters
                 systemd terraform-doc terraform-mode typescript-mode
                 vagrant vterm x509-mode yaml-mode))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook 'org-html-export-to-html)
     (eval add-hook 'after-save-hook 'org-md-export-to-markdown)
     (eval add-hook 'after-save-hook
           (lambda nil (if (y-or-n-p "Tangle?") (org-babel-tangle)))
           nil t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (if (y-or-n-p "Reload?") (load-file user-init-file)))
           nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
