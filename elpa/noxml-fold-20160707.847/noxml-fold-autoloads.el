;;; noxml-fold-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "noxml-fold" "noxml-fold.el" (22554 48365 0
;;;;;;  0))
;;; Generated autoloads from noxml-fold.el

(autoload 'noxml-fold-mode "noxml-fold" "\
Minor mode for hiding and revealing XML tags.

The main entry point is `noxml-fold-dwim', by default bound to
\"C-c C-o C-f C-o\".  To unfold everything, call
`noxml-fold-clearout-buffer', \"C-c C-o C-f b\" by default.

Keyboard shortcuts: 

\\{noxml-fold-keymap}

See `noxml-fold-key-bindings' and `noxml-fold-command-prefix' to
configure keyboard shortcuts.

Called interactively, with no prefix argument, toggle the mode.
With universal prefix ARG (or if ARG is nil) turn mode on.
With zero or negative ARG turn mode off.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; noxml-fold-autoloads.el ends here
