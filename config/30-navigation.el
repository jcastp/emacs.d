;; -*- lexical-binding: t; -*-

;; (require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(use-package key-chord
  :ensure t
  :demand t
  :init
  (key-chord-mode 1)
  :config
  ;; Max time delay between two key presses to be considered a key chord
  (setq key-chord-two-keys-delay 0.15)
  ;; Max time delay between two presses of the same key to be considered a key chord.
  ;; Should normally be a little longer than `key-chord-two-keys-delay'.
  (setq key-chord-one-key-delay 0.25) ; default 0.2
  
  ;; eshell related commands
  (key-chord-define-global "ññ" 'eshell)
  (key-chord-define-global "ñl" 'my/eshell-new) ;; create a new eshell buffer

  ;; Window related commands
  ;; (key-chord-define-global "kk" 'other-window)
  (key-chord-define-global "hh" 'ace-window)

  ;; move related commands
  (key-chord-define-global "jj" 'avy-goto-char-2)
  (key-chord-define-global "jk" 'avy-goto-char-timer)
  (key-chord-define-global "jl" 'avy-goto-line))

(use-package imenu-list
  :ensure t
  :bind ("M-g I" . imenu-list-smart-toggle)
  :config
  ;; set the width to % of the screen
  (setq imenu-list-size 0.20)
  ;; rescan all the buffers
  (setq imenu-auto-rescan t)
  ;; put the imenu in the position we want
  (setq imenu-list-position 'left)
  ;; Establish the depth of the entries shown
  (setq org-imenu-depth 5)
  ;; Auto-resize after update to nil
  (setq imenu-list-auto-resize nil)
  ;; Once you open imenu, focus on it
  (setq imenu-list-focus-after-activation t))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; make the characters bigger, so it is easy to see
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 5.0))))))

(use-package avy
  :ensure t
  ;; only ever reached through the key-chords above, which autoload it
  :defer t)

(use-package perspective
  :ensure t
  :custom
  (persp-mode-prefix-key (kbd "C-c w"))  ; pick your own prefix key here
  :bind
  ("C-c w w" . persp-switch)
  ("C-c w b" . persp-ibuffer)         ; or use a nicer switcher, see below
  ;; Redefining the buffer search to align with persp philosophy.
  ("C-x b" . persp-switch-to-buffer*)
  ("C-x C-b" . persp-switch-to-buffer)
  :init
  (persp-mode)
  :config
  ;; sort the persps by the creation time
  (setq persp-sort 'created))

(with-eval-after-load 'perspective
  ;; where I define my own perspective saved files
  (setq my-persp-save-dir (expand-file-name  "perspectives/" my-data-dir))

  ;; a function to load a perspective saved file from a given directory
  (defun my/load-perspective ()
    "Given a directory with perspective saved files, the user chooses one, and it automatically
  loads it."
    ;;
    (interactive)
    (let ((file (read-file-name "Select a file: " my-persp-save-dir)))
      (persp-state-load file)
      ;; show a message
      (message "You chose: %s" file)))

  (define-key persp-mode-map (kbd "C-c w l") 'my/load-perspective))

(defvar my-persp-load nil
  "Define if we want to load the personal persp.")

(if (and my-homeenvironment-p my-persp-load)
    (progn
      (with-eval-after-load 'org
	(defvar my-persp-files '(
    				 "config-persp"
    				 "tareas-persp"
				 "escritura-persp"
    				 ))

	(dolist (my-file my-persp-files)
	  (persp-state-load (f-join my-persp-save-dir my-file))))))

;; `transient' is bundled with Emacs but not autoloaded for this macro.
;; This block used to work only because org-roam loaded it first; now that
;; org-roam is its own module, the dependency has to be explicit.
(require 'transient)

(transient-define-prefix my/resize-transient ()
  "Resize and arrange windows."
  [["Horizontal"
    ("f" "shrink" shrink-window-horizontally :transient t)
    ("b" "enlarge" enlarge-window-horizontally :transient t)]
   ["Vertical"
    ("p" "enlarge" enlarge-window :transient t)
    ("n" "shrink" shrink-window :transient t)]
   ["Other"
    ("s" "swap" ace-swap-window)
    ("t" "traspose" window-layout-transpose)
    ;;("r" "rotate" rotate-layout)
    ("<left>" "anticlockwise" window-layout-rotate-anticlockwise)
    ("<right>" "clockwise" window-layout-rotate-clockwise)
    ("<up>" "flipup" window-layout-flip-topdown)
    ("<down>" "flipdown" window-layout-flip-topdown) 
    ("q" "quit" transient-quit-one)]])

(global-set-key (kbd "C-c k") #'my/resize-transient)

(use-package speedbar
  :ensure nil
  :if (> emacs-major-version 30)
  :commands (speedbar)
  :config
  (setq speedbar-prefer-window t)
  (setq speedbar-use-images nil))
