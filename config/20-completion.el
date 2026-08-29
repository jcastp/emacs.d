;; -*- lexical-binding: t; -*-

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode 1)
  (setq prescient-save-file (expand-file-name "prescient-save" my-data-dir))
  )

;; To use prescient with vertico
(use-package vertico-prescient
  :ensure t
  :config
  (vertico-prescient-mode 1))

(use-package consult
  :ensure t
  :demand t
  :bind (
         ;; misc commands
         ;;("C-x b" . consult-buffer) ;; defined in perspective section
         ("C-x r b" . consult-bookmark)
         ("M-y" . consult-yank-from-kill-ring)
         ;; Go commands
         ("M-g M-g" . consult-goto-line)
         ("M-i" . consult-imenu)
         ("M-g i" . consult-imenu-multi)
         ("M-g h" . consult-org-heading)
         ("M-g a" . consult-org-agenda)
         ("M-g m" . consult-mark)
         ;; Search commands
         ("C-s" . consult-line)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)))

(use-package marginalia
  :ensure t
  :init
    (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-h B" . embark-bindings))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.4)
  (corfu-auto-prefix 3)
  (corfu-cycle t))

(use-package cape
  :ensure t
  :init
  (require 'dabbrev)
  (defun my/cape-dict-build (dic-file out-file)
    "Cache a plain, one-word-per-line copy of hunspell DIC-FILE at OUT-FILE."
    (unless (file-exists-p out-file)
      (with-temp-buffer
        (insert-file-contents dic-file)
        (goto-char (point-min))
        (delete-region (point-min) (progn (forward-line 1) (point)))
        (while (re-search-forward "/.*$" nil t)
          (replace-match ""))
        (write-region (point-min) (point-max) out-file)))
    out-file)
  (setq cape-dict-file
        (list (my/cape-dict-build "/usr/share/hunspell/en_US.dic"
                                  (expand-file-name "cape-dict-en_US" my-data-dir))
              (my/cape-dict-build "/usr/share/hunspell/es_ES.dic"
                                  (expand-file-name "cape-dict-es_ES" my-data-dir))))
  (add-hook 'completion-at-point-functions
            (cape-capf-super #'dabbrev-capf #'cape-dict)))

(setq enable-recursive-minibuffers t)  ; Use the minibuffer whilst in the minibuffer
(setq completion-cycle-threshold 1)    ; TAB cycles candidates
(setq completions-detailed t)          ; Show annotations
(setq tab-always-indent 'complete)     ; When I hit TAB, try to complete, otherwise, indent

(my/key-define "u" "outline"     #'consult-outline)
(my/key-define "r" "recent file" #'consult-recent-file)
