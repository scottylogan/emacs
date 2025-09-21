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
