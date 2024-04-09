;;; init.el --- My init.el file -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;; lisp evaluation of recursion
(setq max-lisp-eval-depth 5000)

;; amount of lisp variables before throwing an exception
;;(setq max-specpdl-size 5000) ;; obsolete from 29.1

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; disable splash and startup screens
(setq inhibit-splash-screen t
    inhibit-startup-screen t
    )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; straight.el bootstrap
;; develop branch
(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; While that enables the :straight t extension to use-package, let's just have that be the default:
(use-package straight
  :custom (straight-use-package-by-default t
					   straight-default-vc 'git))

(defvar my-config-dir "~/.emacs.d/")
(defvar my-data-dir "~/Nextcloud/config/.emacs.d/")

;; working laptop vm
(defvar my-worksystem-p (equal (system-name) "lubuntuwork"))
;; My desktop machine, able to run anything
(defvar my-desktopsystem-p (or
			    (equal (system-name) "olimpo")
			    (equal (system-name) "doomslayer")))
;; Writing machines
;; probably we can strip some features from them, as they are low end machines
(defvar my-writinglaptop-p (or
			    (equal (system-name) "argos")
			    (equal (system-name) "caliope")))

(defvar my-homeenvironment-p (or
			      (string= (getenv "WORKING") "HOME")
			      (not (string= (getenv "WORKING") "WORK")))
  )
(defvar my-workenvironment-p (string= (getenv "WORKING") "WORK"))

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))

;; define my clear directory
(setq my-clear-directory (expand-file-name "Nextcloud_claro/gocryptfs_claro" (getenv "HOME")))

;; check the directory exists
;;(file-directory-p my-clear-directory)

;; check if a directory is a mount point - against "mount" command in the OS.
(defun is-mount-point-p (path)
  "Check if the given PATH is a mount point."
  (let ((mount-output (shell-command-to-string "mount")))
    (string-match (regexp-quote path) mount-output)))

;; This variable will control if my encrypted dir is mounted on the clear directory
(defvar my-clear-directory-is-mounted-p 0
  "My clear directory is correctly mounted")

;; Check if the directory is mounted
(if (is-mount-point-p my-clear-directory)
    ;; if it is mounted, we change the variable
    (setq my-clear-directory-is-mounted-p t)
  ;; if it is not mounted, then we should launch the script here
  (progn
    ;; I think we should not launch this from here, but just examine what is wrong in the OS.
    ;;(shell-command "bash ~/Nextcloud/config/mount-encrypted-dirs.sh")
    )
  )

(use-package org
  :straight t
  )
(org-babel-load-file "~/.emacs.d/emacs-config.org")

(provide 'init)
;;; init.el ends here
