;;; init.el --- My init.el file -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package initialization
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; use-package settings
(setq use-package-always-ensure t)
(setq package-native-compile t)
(setq warning-minimum-level :emergency)

;; remove the packages warning messages
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Native compilation
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
        native-comp-jit-compilation t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal configuration

(defvar my-config-dir "~/.emacs.d/"
  "Directory where my personal config files reside.
This is the code that will be commited.")

(defvar my-data-dir "~/Nextcloud/config/.emacs.d/"
  "Directory where my personal data files reside.")

;; working laptop vm
(defvar my-worksystem-p (equal (system-name) "lubuntuwork")
  "Name of my working machine.")

;; My desktop machines, able to run anything.
(defvar my-desktopsystem-p (or
			    (equal (system-name) "olimpo")
			    (equal (system-name) "doomslayer"))
  "Names of my personal machines.")

;; Writing machines
;; probably we can strip some features from them, as they are low end machines
(defvar my-writinglaptop-p (or
			    (equal (system-name) "argos")
			    (equal (system-name) "caliope")
			    (equal (system-name) "cerbero"))
  "Names of my writing laptops.")

(defvar my-homeenvironment-p (or
			      (string= (getenv "WORKING") "HOME")
			      (not (string= (getenv "WORKING") "WORK")))
  "My home environment predicate.")

(defvar my-workenvironment-p (string= (getenv "WORKING") "WORK")
  "My work environment predicate.")

;; define my clear directory
(defvar my-clear-directory (expand-file-name "Nextcloud_claro/gocryptfs_claro" (getenv "HOME"))
  "My decrypted directory.")

;; check the directory exists
;;(file-directory-p my-clear-directory)

;; check if a directory is a mount point - against "mount" command in the OS.
(defun is-mount-point-p (path)
  "Check if the given PATH is a mount point."
  (let ((mount-output (shell-command-to-string "mount")))
    (string-match (regexp-quote path) mount-output)))

;; This variable will control if my encrypted dir is mounted on the clear directory
(defvar my-clear-directory-is-mounted-p nil
  "My clear directory is correctly mounted - predicate.")

;; Check if the directory is mounted
(if (is-mount-point-p my-clear-directory)
    ;; if it is mounted, we change the variable
    (setq my-clear-directory-is-mounted-p t)
  ;; if it is not mounted, then we should launch the script here
  (progn
    (message "Encrypted directory not mounted")))

;; load my config
(org-babel-load-file "~/.emacs.d/emacs-config.org")

(provide 'init)
;;; init.el ends here
