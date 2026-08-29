;; -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(fringe-mode 4)

(setq line-number-mode nil)
(setq column-number-mode nil)

(blink-cursor-mode 0)

(global-hl-line-mode 1)

;; Change the background color to a dark grey
;; (set-face-attribute 'hl-line nil :background "#2d2d2d")

;; Change the text color (foreground) to yellow
;; (set-face-attribute 'hl-line nil :foreground "#ffff00")

;; Add a solid underline instead of a background color
;; (set-face-attribute 'hl-line nil :background nil :underline t)

;; Add a box around the line
;; (set-face-attribute 'hl-line nil :background nil :box '(:line-width 1 :color "red"))

;; Change the font JUST for the highlighted line (rare, but possible)
;; (set-face-attribute 'hl-line nil :family "Fira Code" :weight 'bold)

(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M--") 'text-scale-decrease)

(setq-default frame-title-format "%b - (%f)")

(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

(show-paren-mode 1)

(setq tab-width 4) ; or any other preferred value
;; (defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(setq org-fontify-whole-heading-line nil)

(custom-theme-set-faces
 'user
 `(org-level-4 ((t (:height 1.0))))
 `(org-level-3 ((t (:height 1.1))))
 `(org-level-2 ((t (:height 1.2))))
 `(org-level-1 ((t (:height 1.3))))
 `(org-document-title ((t (:height 1.6 :underline nil)))))

(defun my/org-tree-open-in-right-frame ()
  "Open the current Org subtree in an indirect buffer in the right window.
If there is only one window, create a right-side window first.
Then, always move focus to that right window."
  (interactive)
  (let ((right-win (window-right (selected-window))))
    ;; If there's no window to the right, create one
    (unless right-win
      (setq right-win (split-window-right)))
    ;; Show the subtree in an indirect buffer
    (org-tree-to-indirect-buffer)
    ;; Move to the right window
    (select-window right-win)))
 
  (global-set-key (kbd "C-c 8" ) 'my/org-tree-open-in-right-frame)

(defun my/org-tree-open-in-only-window ()
  "Open the current Org subtree in an indirect buffer, focus it, and hide all other windows."
  (interactive)
  (org-tree-to-indirect-buffer)
  (switch-to-buffer org-last-indirect-buffer)
  (delete-other-windows))

  (global-set-key (kbd "C-c 9" ) 'my/org-tree-open-in-only-window)

(setq ring-bell-function 'ignore)
;; (setq visible-bell t)

;; Calculate width based on total lines in buffer at start
(setq display-line-numbers-width-start t)
(setq display-line-numbers-offset 0)
(setq display-line-numbers-type t)
(setq display-line-numbers-grow-only t)
(global-display-line-numbers-mode 1)

(global-visual-line-mode 1) ; 1 for on, 0 for off.

;; visual fill column mode
(use-package visual-fill-column
  :ensure t
  :config
  ;; This sets the value to all the buffers, as initially it is a per-buffer variable
  (setq-default visual-fill-column-width 110)
  ;; set the text in the center of the window
  (setq-default visual-fill-column-center-text t))

;; From Prot's article - https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
  (use-package nerd-icons
    :ensure t)

  (use-package nerd-icons-completion
    :ensure t
    :after marginalia
    :config
    (nerd-icons-completion-marginalia-setup)
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
    :ensure t
    :hook
    (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :ensure t
  ;;:hook (after-init . doom-modeline-mode)
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  ;; Do we need to show the minor modes?
  (setq doom-modeline-minor-modes nil)
  ;; word count of the buffer disabled, as it slows operations when word count grows
  ;; (setq doom-modeline-enable-word-count t)
  ;; Major modes in which to display word count continuously.
  ;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
  ;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
  ;; remove the modes from `doom-modeline-continuous-word-count-modes'.
  ;; (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (setq doom-modeline-env-version t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-buffer-file-name-style 'auto)
  ;; github status in the modeline
  (setq doom-modeline-github nil)
  ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
  (setq doom-modeline-lsp t)
  ;; Whether display the time. It respects `display-time-mode'.
  (setq doom-modeline-time t))

(require 'pulse)

(defface my-pulse-line-face
  '((t :background "#26837e" :extend t))
  "Face for `my/pulse-line', readable on both dark and clear themes.")

;; nil in Emacs 32 by default, which means a static highlight instead of a fade
(setq pulse-flag t)
(setq pulse-iterations 12)
(setq pulse-delay 0.03)

(defun my/pulse-line (&rest _)
  "Briefly highlight the current line using built-in pulse.el."
  (interactive)
  (pulse-momentary-highlight-one-line (point) 'my-pulse-line-face))

;; Pulse after navigation commands
(dolist (cmd '(scroll-up-command scroll-down-command
               recenter-top-bottom other-window
               windmove-left windmove-right windmove-up windmove-down))
  (advice-add cmd :after #'my/pulse-line))

;; Pulse after consult jumps
(add-hook 'consult-after-jump-hook #'my/pulse-line)

(global-set-key (kbd "C-x l") #'my/pulse-line)

(use-package spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths
	'( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8))
  (spacious-padding-mode 1))

(defcustom my/centered-content-width 100
  "Width of the centered content area in characters."
  :type 'integer
  :group 'editing-basics)

(defun my/centered-content-width-value ()
  "Prompt the user for a value and set `my/centered-content-width` to it."
  (interactive)
  (setq my/centered-content-width (string-to-number (read-from-minibuffer "Value for centered width: "))))

(defvar my/centered-mode nil)

(define-minor-mode my/centered-mode
  "Minor mode to center buffer content using margins and visual line mode."
  :init-value nil
  :global nil
  :lighter " [Centered]"
  :group 'editing-basics
  (if my/centered-mode
      (progn
        (visual-line-mode 1)
        (add-hook 'window-configuration-change-hook 
                  #'my/centered--update-margins nil t)
        (my/centered--update-margins))
    (progn
      (visual-line-mode 1)
      (remove-hook 'window-configuration-change-hook 
                   #'my/centered--update-margins t)
      (set-window-margins nil 0 0))))

(defun my/centered--update-margins ()
  "Calculate and apply margins to center 100-character content."
  (let* ((width (frame-width))
         (margin (max 0 (/ (- width my/centered-content-width) 2))))
    (set-window-margins nil margin margin)))

(defvar my-hl-line-dark-background "#303030"
    "Background color for `hl-line' when a dark theme is active.")
  (defvar my-hl-line-dark-foreground "#f0f0f0"
    "Foreground color for `hl-line' when a dark theme is active.")
  (defvar my-hl-line-clear-background "#e0e0e0"
    "Background color for `hl-line' when a clear theme is active.")
  (defvar my-hl-line-clear-foreground "#202020"
    "Foreground color for `hl-line' when a clear theme is active.")

  (defun my/set-hl-line-face (dark-p &optional with-foreground)
    "Set the `hl-line' face colors for a dark or clear theme, based on DARK-P.
The foreground is only touched when WITH-FOREGROUND is non-nil, so that
the personal themes keep whatever foreground the theme itself picked."
    (set-face-attribute 'hl-line nil
                        :background (if dark-p
                                        my-hl-line-dark-background
                                      my-hl-line-clear-background))
    (when with-foreground
      (set-face-attribute 'hl-line nil
                          :foreground (if dark-p
                                          my-hl-line-dark-foreground
                                        my-hl-line-clear-foreground))))

(my/key-define "c" "centered mode" #'my/centered-mode)
