;;; early-init.el --- Early Init -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:
(setq package-enable-at-startup nil)

;; the custom.el file location
;; from Prot's article - https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; Set the frame size according to pixels and not based on the size of the font.
(setq frame-resize-pixelwise t)

;; Inhibit resizing the frame when changing the font
(setq frame-inhibit-implied-resize t)

;; remove the GUI features
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; disable splash and startup screens
(setq inhibit-splash-screen t
      inhibit-startup-screen t)

(provide 'early-init)
;;; early-init.el ends here
