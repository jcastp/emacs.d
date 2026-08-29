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
(setq warning-minimum-level :error)

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

;; Negative polarity on purpose: the machine lists are closed sets of
;; hostnames, so a machine that is in none of them -- a new laptop, a VM --
;; gets the full configuration rather than silently losing features.
(defvar my-full-system-p (not my-writinglaptop-p)
  "Non-nil unless this is one of the low-end writing laptops.
Used to keep heavyweight packages off those machines.  Note this is about
install footprint, not startup time: `:ensure' installs a package even
when its `use-package' form is deferred.")

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

;; check if a directory is a mount point - reads /proc/mounts directly,
;; avoiding a subprocess spawn on every startup.
(defun is-mount-point-p (path)
  "Check if the given PATH is a mount point."
  (with-temp-buffer
    (insert-file-contents "/proc/mounts")
    (string-match (regexp-quote path) (buffer-string))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config module loading

;; Modules are loaded in the order listed here.  The cdr is the condition
;; under which the module loads; see `my/module-enabled-p'.
;;
;; The numbering is spaced so a new module can be slotted in without
;; renumbering.  config/99-scratch.org is deliberately absent: it is the
;; staging area and tangles nothing.
(defvar my-config-modules
  '(("config/00-core"            . always)
    ("config/05-identity"        . always)
    ("config/10-ui"              . always)
    ("config/15-theme"           . always)
    ("config/20-completion"      . always)
    ("config/30-navigation"      . always)
    ("config/40-org"             . always)
    ("config/45-agenda"          . always)
    ("config/46-agenda-personal" . home)
    ("config/47-agenda-work"     . work)
    ("config/50-org-roam"        . always)
    ("config/55-org-export"      . home)
    ("config/60-writing"         . home)
    ("config/70-prog"            . always)
    ("config/80-apps"            . always)
    ("config/85-ai"              . (home full-system))
    ("config/90-keymap"          . always))
  "Config modules and the condition under which each one loads.
Each entry is a cons of a path relative to `my-config-dir', without
the .org extension, and a condition symbol understood by
`my/module-enabled-p'.  The keymap module must stay last: it assembles
entries contributed by the other modules.")

(defun my/module-enabled-p (condition)
  "Return non-nil when CONDITION holds for this machine and environment."
  (pcase condition
    ('always      t)
    ('home        my-homeenvironment-p)
    ('work        my-workenvironment-p)
    ('full-system my-full-system-p)
    ('clear       my-clear-directory-is-mounted-p)
    ;; a list means every condition in it must hold
    ((pred consp) (seq-every-p #'my/module-enabled-p condition))
    (_            nil)))

(defun my/load-config-modules ()
  "Tangle and load every enabled module in `my-config-modules'."
  (dolist (module my-config-modules)
    (when (my/module-enabled-p (cdr module))
      (org-babel-load-file
       (expand-file-name (concat (car module) ".org") my-config-dir)))))

(my/load-config-modules)

(provide 'init)
;;; init.el ends here
