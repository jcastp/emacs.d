;; -*- lexical-binding: t; -*-

(use-package magit
  :ensure t
  :after transient
  :bind
  (:map global-map
	("C-x g" . magit-status)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
         ("C-c g n" . flymake-goto-next-error)
         ("C-c g p" . flymake-goto-prev-error)
         ("C-c g l" . flymake-show-buffer-diagnostics)))

(setq treesit-auto-install-grammar 'always) ; EMACS-31
(setq treesit-enabled-modes t)              ; EMACS-31

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp")))

  (setq-default eglot-workspace-configuration
		'((:pylsp . (:configurationSources ["flake8"] :plugins (:pycodestyle (:enabled nil) :mccabe (:enabled nil) :flake8 (:enabled t))))))

  :bind
  (:map eglot-mode-map
        ("C-c f" . eglot-format-buffer))

  :hook
  ((python-ts-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (c-ts-mode . eglot-ensure)
   (c++-ts-mode . eglot-ensure)))

(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

(setq python-shell-interpreter "python3")

(use-package markdown-ts-mode
:ensure nil
:defer t)

;; (use-package markdown-mode
;;   :ensure t
;;   :mode ("README\\.md\\'" . gfm-mode)
;;   :init (setq markdown-command '("pandoc" "--from=markdown" "--to=html5")))
