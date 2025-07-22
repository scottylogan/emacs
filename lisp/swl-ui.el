;; catppuccin theme
(use-package catppuccin-theme
  :ensure t
  :init
  (load-theme 'catppuccin :noconfirm)
  (setq catppuccin-flavor 'mocha)
  (catppuccin-reload))


;; show line numbers in wide windows
(setq-default display-line-numbers (> (frame-width) 80))

;; show colum number in mode line
(column-number-mode)
(scroll-bar-mode -1)

;; disable tool bar and tooltips
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(tooltip-mode -1)

;; disable menu bar when running in a terminal, or X
;; but leave on for MacOS GUI Emacs
(when (or (not window-system) (eq window-system 'x))
  (menu-bar-mode -1))

;;; ----- FRAMES

(add-to-list 'default-frame-alist '(height . 32))
(add-to-list 'default-frame-alist '(width . 100))
(setq frame-title-format "%f")

(when (or (eq window-system 'ns) (eq window-system 'x))
  ;; default Latin font
  (set-face-attribute 'default nil
                      :family "Iosevka Slab"
                      :slant 'normal
                      :height (cond ((< (display-pixel-height) 1550) 160)
                                    ((>= (display-pixel-height) 2160) 300)
                                    (t 220))))

(defun scotty-frame-setup (&optional frame)
  "Configure new frames."
  (when (and (eq system-type 'darwin) (eq window-system 'ns))

    ;; default Latin font
    (set-face-attribute 'default frame
                        :family "Iosevka Slab"
                        :slant 'normal
                        :height (cond ((< (display-pixel-height) 1550) 160)
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


;;; from https://www.emacswiki.org/emacs/NeoTree#h5o-11
(defun scotty-neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

