;; -*- lexical-binding: t; -*-

(defvar my/key-prefix-map (make-sparse-keymap)
    "My personal keymap, bound to `C-q'.
Modules register their own entries with `my/key-define' and
`my/key-define-submap'.")

  (keymap-set global-map "C-q" my/key-prefix-map)

  (defun my/key-define (key name def &optional map)
    "Bind KEY to DEF, labelled NAME, in MAP.
MAP defaults to `my/key-prefix-map'.  NAME is what which-key and
`describe-keymap' show for the binding."
    (keymap-set (or map my/key-prefix-map) key (cons name def)))

  (defun my/key-define-submap (key name &optional map)
    "Register a fresh sub-keymap under KEY, labelled NAME, and return it.
MAP defaults to `my/key-prefix-map'.  The caller fills the returned
keymap in with `my/key-define'."
    (let ((sub (make-sparse-keymap)))
      (keymap-set (or map my/key-prefix-map) key (cons name sub))
      sub))

(defun er-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(global-set-key [remap keyboard-quit] #'er-keyboard-quit-dwim)

(setq use-short-answers t)

(setq sentence-end-double-space nil)

(setq confirm-kill-emacs 'y-or-n-p)

(setq network-security-level 'high)

(setq create-lockfiles nil)

;; backup in one place. flat, no tree structure
;; TODO change this to the variables for directories as it does not work as expected
;; (setq backup-directory-alist '(("" . "~/Nextcloud/config/.emacs.d/backups")))
(setq backup-directory-alist `(("" . ,(expand-file-name "backups" my-data-dir))))
;; What if the file is linked? We copy the file
(setq backup-by-copying t)
;; Version control
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(set-language-environment "UTF-8")
(set-input-method nil)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(setq
 ;; makes killing/yanking interact with the system clipboard
 select-enable-clipboard t

 ;; This is explained here:
 ;; https://superuser.com/questions/90257/what-is-the-difference-between-the-x-clipboards
 ;; https://unix.stackexchange.com/questions/139191/whats-the-difference-between-primary-selection-and-clipboard-buffer
 select-enable-primary t

 ;; Save clipboard strings into kill ring before replacing them.
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t

 ;; Shows all options when running apropos. For more info,
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
 apropos-do-all t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)

(global-auto-revert-mode 1)

(electric-pair-mode 1)
;; make electric-pair-mode work on more sets of punctuation signs.
(setq electric-pair-pairs
      '(
        (?\¡ . ?\!)
        (?\¿ . ?\?)))

(setq electric-pair-inhibit-predicate
      (lambda (c)
        (if (char-equal c ?<) t
          (electric-pair-default-inhibit c))))

(use-package vundo
  :ensure t
  ;; `:bind' creates the autoload, so C-z works without loading vundo first.
  :bind ("C-z" . vundo))

(which-key-mode)

(setq project-list-file (expand-file-name "projects" my-data-dir))

(defun my/reload-config()
  "Reloads my emacs configuration."
  (interactive)
  (let ((my-init-file (expand-file-name "init.el" user-emacs-directory)))
    (message "This: %s" my-init-file)
    (if (file-exists-p my-init-file)
	;; if
	(progn
	  (load my-init-file)
	  (message "Reloaded my config."))
      ;; else
      (message "File %s not found." my-init-file))))

(global-set-key (kbd "C-c r r" ) 'my/reload-config)

(use-package jinx
:ensure t
:demand t
:hook (emacs-startup . global-jinx-mode)
:bind (("M-$" . jinx-correct)
       ("C-M-$" . jinx-languages)))
(setq jinx-languages "en es")

(use-package delsel
  :ensure nil ; no need to install it as it is built-in
  :hook (after-init . delete-selection-mode))

(use-package helpful
  :ensure t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command))

;;where do we read the abbrevs from
(setq abbrev-file-name (expand-file-name "abbrev_defs" my-data-dir))
(add-hook 'text-mode-hook 'abbrev-mode)
(add-hook 'prog-mode-hook 'abbrev-mode)
;; save abbrevs when files are saved
(setq save-abbrevs 'silently)

(save-place-mode 1)

(setq kill-region-dwim 'emacs-word)               ; EMACS-31: C-w with no region kills a word

;; A Protesilaos life savier HACK
;; Add option "d" to whenever using C-x s or C-x C-c, allowing a quick preview
;; of the diff (if you choose `d') of what you're asked to save.
(add-to-list 'save-some-buffers-action-alist
             (list "d"
                   (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                   "show diff between the buffer and its file"))

(defvar my-state-file-suffix (if my-workenvironment-p "-work" "")
  "Suffix distinguishing the work instance's state files from the personal ones.")

(defun my/state-file (name)
  "Return the path of state file NAME in `my-data-dir'.
The name is suffixed with `my-state-file-suffix' so that the home and
work instances keep separate history."
  (expand-file-name (concat name my-state-file-suffix) my-data-dir))

(setq recentf-save-file (my/state-file "recentf"))
(setq recentf-max-saved-items 100
      recentf-exclude '("/tmp/" "/ssh:" "/sudo:"))
(recentf-mode 1)

(setq savehist-file (my/state-file "savehist"))
(setq history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history t)
(setq savehist-additional-variables
      (append '(kill-ring
                search-ring
                regexp-search-ring)
              (if my-workenvironment-p
                  ;; remember the last direct report across sessions
                  '(my/last-report)
                ;; remember the last writing project across sessions
                '(my/last-writing-project))))
(savehist-mode 1)

(setq bookmark-default-file (my/state-file "bookmarks"))
(setq bookmark-save-flag 1)   ;save bookmarks after each entry
