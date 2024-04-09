;;; early-init.el --- Early Init -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:
(setq package-enable-at-startup nil)

;; garbage collector config
(setq gc-cons-threshold (* 1024 1024 20))
(setq gc-cons-percentage 0.5)

;; the custom.el file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Set the frame size according to pixels and not based on the size of the font.
(setq frame-resize-pixelwise t)

;; Inhibit resizing the frame when changing the font
(setq frame-inhibit-implied-resize t)

(provide 'early-init)
;;; early-init.el ends here
