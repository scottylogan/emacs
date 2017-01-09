;;; jabber-otr-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "jabber-otr" "jabber-otr.el" (22554 48903 0
;;;;;;  0))
;;; Generated autoloads from jabber-otr.el

(autoload 'jabber-otr-encrypt "jabber-otr" "\
Request to activate encryption in the current chat buffer.

\(fn)" t nil)

(eval-after-load "jabber-core" '(add-to-list 'jabber-message-chain 'jabber-otr--handle-message))

(autoload 'jabber-otr--handle-message "jabber-otr" "\


\(fn JC XML-DATA)" nil nil)

;;;***

;;;### (autoloads nil nil ("jabber-otr-pkg.el") (22554 48903 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; jabber-otr-autoloads.el ends here
