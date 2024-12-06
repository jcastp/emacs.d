;;; init.el --- My init.el file -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;; lisp evaluation of recursion
(setq max-lisp-eval-depth 5000)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; disable splash and startup screens
(setq inhibit-splash-screen t
      inhibit-startup-screen t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elpaca config
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))


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
        native-comp-deferred-compilation t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal configuration

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

;; get the org package
;; (use-package org
;;   :ensure t
;;   )
;; load my config
(org-babel-load-file "~/.emacs.d/emacs-config.org")

(provide 'init)
;;; init.el ends here
