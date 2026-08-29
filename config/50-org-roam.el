;; -*- lexical-binding: t; -*-

(defun which-linux-distribution ()
  "from lsb_release"
  (interactive)
  (when (eq system-type 'gnu/linux)
    (shell-command-to-string "lsb_release -sd")))

(when (string-prefix-p "Debian" (which-linux-distribution))
  (use-package sqlite3
    :ensure t))

;; main org-roam config
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  ;; `directory-file-name' keeps this slash-free, exactly as the old
  ;; literal path was; org-roam compares against it.
  (setq org-roam-directory (file-truename (directory-file-name my-roam-dir)))
  (setq org-roam-db-location (file-truename
                              (expand-file-name "org-roam.db" my-roam-dir)))
  :bind (
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n r" . org-roam-ref-add)
         ("C-c n u" . org-roam-ui-open)

         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("U" . org-roam-dailies-capture-today)
         ("I" . org-roam-dailies-capture-tomorrow)
         )
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (setq org-roam-completion-everywhere t)
  (require 'org-roam-dailies)
  ;; org-roam-export pulls in the whole `ox' machinery, and is only needed
  ;; when actually exporting -- so load it with ox rather than at startup.
  (with-eval-after-load 'ox
    (require 'org-roam-export))
  ;; Opening and syncing the database is I/O we do not need on the critical
  ;; path; run it once Emacs is up and interactive instead.
  ;; `emacs-startup-hook' still runs before the session is usable, so use an
  ;; idle timer: the database opens once you have stopped typing.
  (run-with-idle-timer 1 nil (lambda () (org-roam-db-autosync-mode 1))))

(defun my/org-roam-fleeting-notes ()
  "Open my org-roam file with my fleeting notes"
  (interactive)
  (find-file (expand-file-name "personal/fleeting_notes.org" my-roam-dir)))
(global-set-key (kbd "C-c n F") 'my/org-roam-fleeting-notes)

(setq org-roam-capture-templates
      '(
        ;; personal note — lands in personal/
        ("p" "personal note" plain
         "* TODO ${title}\n%?"
         :if-new (file+head "personal/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)

        ;; work concept note — lands in work/
        ("k" "work concept" plain
         "* ${title}\n%?"
         :if-new (file+head "work/%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n#+filetags: work\n")
         :unnarrowed t)))

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.33)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))

(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:20}" 'face 'org-tag)))

;; Both exist only to support org-roam-ui, which is itself deferred.
(use-package websocket
  :ensure t
  :defer t)
(use-package simple-httpd
  :ensure t
  :defer t)
(use-package org-roam-ui
  :ensure t
  :hook (org-roam . org-roam-ui-mode))

(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n#+filetags: daily\n"))))

(if (not (require 'org-roam nil t))
    (message "org-roam not found")

  (setq my/roamtag "roamtag")

  (defun vulpea-buffer-prop-set (name value)
    "Set a file property called NAME to VALUE in buffer file.
    If the property is already set, replace its value."
    (setq name (downcase name))
    (org-with-point-at 1
      (let ((case-fold-search t))
        (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                               (point-max) t)
            (replace-match (concat "#+" name ": " value) 'fixedcase)
          (while (and (not (eobp))
                      (looking-at "^[#:]"))
            (if (save-excursion (end-of-line) (eobp))
                (progn
                  (end-of-line)
                  (insert "\n"))
              (forward-line)
              (beginning-of-line)))
          (insert "#+" name ": " value "\n")))))

  (defun vulpea-buffer-prop-set-list (name values &optional separators)
    "Set a file property called NAME to VALUES in current buffer.
    VALUES are quoted and combined into single string using
    `combine-and-quote-strings'.
    If SEPARATORS is non-nil, it should be a regular expression
    matching text that separates, but is not part of, the substrings.
    If nil it defaults to `split-string-default-separators', normally
    \"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t.
    If the property is already set, replace its value."
    (vulpea-buffer-prop-set
     name (combine-and-quote-strings values separators)))

  (defun vulpea-buffer-tags-set (&rest tags)
    "Set TAGS in current buffer.
      If filetags value is already set, replace it."
    (vulpea-buffer-prop-set "filetags" (string-join tags " ")))

  (defun vulpea-buffer-prop-get (name)
    "Get a buffer property called NAME as a string."
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                               (point-max) t)
        (buffer-substring-no-properties
         (match-beginning 1)
         (match-end 1)))))

  (defun vulpea-buffer-prop-get-list (name &optional separators)
    "Get a buffer property NAME as a list using SEPARATORS.
        If SEPARATORS is non-nil, it should be a regular expression
        matching text that separates, but is not part of, the substrings.
        If nil it defaults to `split-string-default-separators', normally
        \"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
    (let ((value (vulpea-buffer-prop-get name)))
      (when (and value (not (string-empty-p value)))
        (split-string-and-unquote value separators))))

  (defun vulpea-buffer-tags-get ()
    "Return filetags value in current buffer."
    (vulpea-buffer-prop-get-list "filetags" " "))

  (defun vulpea-buffer-tags-add (tag)
    "Add a TAG to filetags in current buffer."
    (let* ((tags (vulpea-buffer-tags-get))
           (tags (append tags (list tag))))
      (apply #'vulpea-buffer-tags-set tags)))

  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.

            TODO entries marked as done are ignored, meaning the this
            function returns nil if current buffer contains only completed
            tasks."
    (seq-find
     (lambda (type)
       (eq type 'todo))
     (org-element-map
         (org-element-parse-buffer 'headline)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defvar vulpea--updating-tag nil
    "Re-entry guard for `vulpea-project-update-tag'.")

  (defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (unless vulpea--updating-tag
      (let ((vulpea--updating-tag t))
        (when (and (not (active-minibuffer-window))
                   (vulpea-buffer-p))
          (save-excursion
            (goto-char (point-min))
            (let* ((tags (vulpea-buffer-tags-get))
                   (original-tags tags))
              (if (vulpea-project-p)
                  (setq tags (cons "roamtag" tags))
                (setq tags (remove "roamtag" tags)))
              (setq tags (seq-uniq tags))
              (when (or (seq-difference tags original-tags)
                        (seq-difference original-tags tags))
                (apply #'vulpea-buffer-tags-set tags))))))))

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun vulpea-project-files ()
    "Return a list of note files containing 'roamtag' tag."
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
                :from tags
                :left-join nodes
                :on (= tags:node-id nodes:id)
                :where (like tag (quote "%\"roamtag\"%"))]))))

  (add-hook 'find-file-hook #'vulpea-project-update-tag)
  (add-hook 'before-save-hook #'vulpea-project-update-tag))

(defun my/agenda-restrict-to-roam ()
  "Restrict agenda to current project"
  (interactive)
  (let ((org-agenda-files (list org-roam-directory)))
    (org-agenda)))
(global-set-key (kbd "C-c n a") 'my/agenda-restrict-to-roam)

(defun my/random-roam-node ()
  "Opens a random org-roam node."
  (interactive)
  (let ((todo-nodes
         (org-roam-db-query
          [:select [nodes:file]
                   :from tags
                   :left-join nodes
                   :on (= tags:node-id nodes:id)
                   ])))
    (find-file (car (nth (random (length todo-nodes)) todo-nodes)))))

(global-set-key (kbd "C-c n R") 'my/random-roam-node)

(defun my/random-roam-todo-node ()
  "Opens a random org-roam node with the roamtag tag."
  (interactive)
  (let ((todo-nodes
         (org-roam-db-query
          [:select [nodes:file]
                   :from tags
                   :left-join nodes
                   :on (= tags:node-id nodes:id)
                   :where (like tag (quote "%\"roamtag\"%"))])))
    (find-file (car (nth (random (length todo-nodes)) todo-nodes)))))

(global-set-key (kbd "C-c n T") 'my/random-roam-todo-node)

(defun my/org-roam-orphans ()
    "List notes with no backlinks (orphaned nodes) and visit the selected one."
    (interactive)
    (let ((orphans (org-roam-db-query
                    [:select [nodes:title nodes:file nodes:id]
                     :from nodes
                     :left-join links :on (= links:dest nodes:id)
                     :where (is links:dest nil)])))
      (if (not orphans)
          (message "No orphaned notes found")
        (let* ((choice (completing-read
                        (format "Orphaned notes (%d): " (length orphans))
                        (mapcar #'car orphans) nil t))
               (entry (cl-find choice orphans :key #'car :test #'string=)))
          (find-file (cadr entry))))))

  (defun my/org-roam-stale-notes ()
    "List notes not modified in the last 90 days and visit the selected one.
`mtime' lives on the `files' table, not `nodes', so we join and filter in
Elisp using `float-time' to handle org-roam's stored time format."
    (interactive)
    (let* ((cutoff (- (float-time) (* 90 86400.0)))
           (rows (org-roam-db-query
                  [:select [nodes:title nodes:file files:mtime]
                   :from nodes
                   :join files :on (= nodes:file files:file)]))
           (stale (seq-filter
                   (lambda (row) (< (float-time (nth 2 row)) cutoff))
                   rows)))
      (if (not stale)
          (message "No stale notes found")
        (let* ((choice (completing-read
                        (format "Stale notes (%d): " (length stale))
                        (mapcar #'car stale) nil t))
               (entry (cl-find choice stale :key #'car :test #'string=)))
          (find-file (cadr entry))))))

  (defun my/org-roam-creative-prompt ()
    "Generate a random creative connection between two notes."
    (interactive)
    (let* ((nodes (org-roam-node-list))
           (node1 (nth (random (length nodes)) nodes))
           (node2 (nth (random (length nodes)) nodes)))
      (message "Connect '%s' to '%s'"
               (org-roam-node-title node1)
               (org-roam-node-title node2))))

(when my-homeenvironment-p
  (add-to-list 'org-roam-capture-templates
               '("b" "book notes" plain
                 (file "~/.emacs.d/roamtemplates/BookNoteTemplate.org")
                 :if-new (file+head "personal/%<%Y%m%d%H%M%S>-${slug}.org"
                                    "#+title: ${title}\n#+filetags: book")
                 :unnarrowed t)
               t)

  (add-to-list 'org-roam-capture-templates
               '("w" "writing idea" plain
                 (file "~/.emacs.d/roamtemplates/WritingIdea.org")
                 :if-new (file+head "personal/%<%Y%m%d%H%M%S>-${slug}.org"
                                    "#+title: ${title}\n#+filetags: writing")
                 :unnarrowed t)
               t)

  (add-to-list 'org-roam-capture-templates
               '("c" "writing character" plain
                 (file "~/.emacs.d/roamtemplates/CharacterIdea.org")
                 :if-new (file+head "personal/%<%Y%m%d%H%M%S>-${slug}.org"
                                    "#+title: ${title}\n#+filetags: writing character")
                 :unnarrowed t)
               t)
  )
