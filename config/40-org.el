;; -*- lexical-binding: t; -*-

(setq
 ;; tag alignment
 ;; this value allows for modern themes and column fill to work well together.
 org-auto-align-tags t
 org-tags-column 0
 ;; org fold - edit in an invisible region
 org-fold-catch-invisible-edits 'show-and-error
 ;; C-a and C-e will move the cursor just the heading. No todo keyworks, no tags
 org-special-ctrl-a/e t
 ;; insert new heading after the current subtree
 org-insert-heading-respect-content t)

(setq org-structure-template-alist
      '(("a" . "export ascii")
	("c" . "comment")
	("C" . "center")
	("d" . "comment summary")
	("e" . "example")
	("E" . "export")
	("h" . "export html")
	("l" . "export latex")
	("n" . "comment note")
	("q" . "quote")
	("s" . "src")
	("S" . "src emacs-lisp")
	("v" . "verse")))

;; org-id are globally unique UUIDs
(setq org-id-method 'uuid)
;; It will use the CUSTOM_ID, if it exists
;;or will create a new one in the form or UUID, as seen above
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
;; file to store the local =org-id= locations
;; This way is synced to all my computers
(setq org-id-locations-file (expand-file-name "org-id-locations" my-data-dir))

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t))
          t)

(setq org-blank-before-new-entry '(
                                   (heading . t)
                                   (plain-list-item . nil)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)

(setq org-log-into-drawer t)

;; Don't log deadline/schedule date changes.
(setq org-log-redeadline nil)
(setq org-log-reschedule nil)

;; Define theme-adaptive faces for org-todo keywords
(defface my/org-todo-active
  '((((class color) (background light))
     :foreground "#a60000" :background "#ffd5d5" :weight bold)
    (((class color) (background dark))
     :foreground "#ff8f88" :background "#3f1210" :weight bold))
  "Face for active TODO keywords.")

(defface my/org-todo-waiting
  '((((class color) (background light))
     :foreground "#813e00" :background "#fff0c0" :weight bold)
    (((class color) (background dark))
     :foreground "#fec43f" :background "#4a3000" :weight bold))
  "Face for WAITING/HOLD keywords.")

(defface my/org-todo-canceled
  '((((class color) (background light))
     :foreground "#505050" :background "#e8e0f0" :strike-through t)
    (((class color) (background dark))
     :foreground "#b0a0b8" :background "#2e2540" :strike-through t))
  "Face for CANCELED keyword.")

(defface my/org-todo-done
  '((((class color) (background light))
     :foreground "#006800" :background "#ccf0cc" :weight bold)
    (((class color) (background dark))
     :foreground "#44bc44" :background "#002f00" :weight bold))
  "Face for DONE keyword.")

;; Apply faces to keywords
(setq org-todo-keyword-faces
      '(("TODO"      . my/org-todo-active)
        ("WAITING"   . my/org-todo-waiting)
        ("HOLD"      . my/org-todo-waiting)
        ("CANCELED" . my/org-todo-canceled)
        ("DONE"      . my/org-todo-done)))

(defun my/org-set-clock ()
  "One-off function for `org-mode' task clocking.

Behaviour:
 * When there is no running clock, start the clock for the item at point.
 * When there is already a running clock and `point' is at the item which is being clocked stop the corresponding clock.
 * When there is already a running clock but `point' is not at the item which is being clocked, stop the clock and restart it for item at `point'."
  (interactive)
  (let ((interrupting (and (not org-clock-resolving-clocks-due-to-idleness)
                           (org-clocking-p))))
    (if interrupting
        (if (save-excursion
              (org-back-to-heading t)
              (and (equal (marker-buffer org-clock-hd-marker)
                          (current-buffer))
                   (= (marker-position org-clock-hd-marker)
                      (point))
                   (equal org-clock-current-task (nth 4 (org-heading-components)))))
            (org-clock-out)
          (org-clock-in))
      (org-clock-in))))

;; keybindings
(global-set-key (kbd "C-c C-x C-i") 'org-clock-in)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "C-c C-x i") 'my/org-set-clock)

(defun my/capture-to-this-buffer ()
  "Capture a note to the current buffer’s file, under the headline \"Notes\")."
  (interactive)
  ;; Make sure we are in an Org buffer.
  (unless (derived-mode-p 'org-mode)
    (user-error "Can’t capture to a non‑Org buffer"))

    ;; Choose the destination that actually exists.
    (let ((target (buffer-file-name)))
      ;; Build a temporary capture template that points at TARGET.
      (let ((org-capture-templates
             `(("t" "Todo"
                entry
                (file+headline ,target "Notes")
                "** TODO %?"))))
        ;; Run the capture UI.
        (org-capture))))

(global-set-key (kbd "C-c C") 'my/capture-to-this-buffer)

(setq org-confirm-babel-evaluate nil)

;; Improve org mode looks
(setq org-pretty-entities t
      org-startup-with-inline-images t
      org-image-actual-width '(800)
      org-hide-emphasis-markers t)

;; Line spacing, in pixels
(setq-default line-spacing 0)

;; Change the ellipsis
(setq org-ellipsis "…")

(setq org-startup-indented t)

(with-eval-after-load 'flycheck
  (setq flycheck-checkers (delq 'org-lint flycheck-checkers)))

(use-package org-ql
  :ensure t
  :bind
  ("C-c q f" . org-ql-find-in-agenda)
  ("C-c q s" . org-ql-search)
  ("C-c q v" . org-ql-view))

(defun my/scratch-org-buffer ()
  "Create a new scratch buffer -- \*hello-world\*"
  (interactive)
  (let ((n 0)
        bufname buffer)
    (catch 'done
      (while t
        (setq bufname (concat "*org-scratch"
			      (if (= n 0) "" (int-to-string n))
			      "*"))
        (setq n (1+ n))
        (when (not (get-buffer bufname))
          (setq buffer (get-buffer-create bufname))
          (with-current-buffer buffer
            (org-mode)
	    (insert "# org-scratch buffer\n"))
          ;; When called non-interactively, the `t` targets the other window (if it exists).
          (throw 'done (select-window (display-buffer buffer t))))))))

(use-package org-modern
  :ensure t
  :config
  (with-eval-after-load 'org (global-org-modern-mode)))

(when my-homeenvironment-p
  (setq org-todo-keywords
        '(
          ;; Status for tasks
          (sequence "TODO(t)" "ONGOING(o)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@/!)")
          ;; Status for writing
  	(sequence "TOWRITE(h)" "WRITING(j@/!)" "REVIEW(k@/!)" "REWORK(n@/!)" "|" "FINISHED(l!)" "PURGE(ñ@/!)")))
  )

(when my-homeenvironment-p
  (setq org-tag-alist '(;; Areas of responsibility
                        ("personal" . ?p) ("health" . ?H) ("financial" . ?f)
                        ;; Contexts
                        ("@home" . ?h) ("@errands" . ?E) ("@computer" . ?c) ("@online" . ?o) ("@penpaper" . ?P)
                        ;; interests
                        ("emacs" . ?e) ("writing" . ?w) ("reading" . ?r) ("calls" . ?C) ("blog" . ?b)
                        ("books" . ?B)
                        ;; energy
                        ("low_energy" . ?l) ("high_energy" . ?g)))
  )

(when my-homeenvironment-p
  (setq org-capture-templates'(
  			     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                               ;; task related captures

  			     ("t" "Inbox task" entry
                                (file+headline "~/Nextcloud/agenda/tasks.org" "Inbox")
  			      "** TODO %i%?\n:PROPERTIES:\n:CATEGORY: task\n:END:\n"
                                :empty-lines-after 1)

  			     ("p" "New project." entry
                                (file+headline "~/Nextcloud/agenda/tasks.org" "Tasks")
                                "** TODO %? %^g\n:PROPERTIES:\n:COOKIE_DATA: todo recursive\n:CATEGORY: project\n:END:\n"
                                :empty-lines-after 1)

  			     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                               ;; Writing related things

  			     ;; Everything that I find interesting to create, no matter what it is
  			     ("h" "Compost heap" item
                                (file+headline "~/Nextcloud/escritura/retazos/compost_heap.org" "Compost heap")
                                "%i"
                                :empty-lines-after 1)

  			     ;; A possible writing idea
                               ("w" "Writing idea." entry
                                (file+headline "~/Nextcloud/escritura/retazos/ideas.org" "Ideas")
                                "** TODO %?\n*** Personajes\n- \n*** Ambientación\n*** Eventos\n"
                                :empty-lines-after 1)

                               ;; An interesenting character
                               ("P" "Personaje" entry
                                (file "~/Nextcloud/escritura/retazos/personajes.org")
                                "* %i%?"
                                :empty-lines-after 1)))
  )

(when my-workenvironment-p
  ;; tags are next to the title
  (setq org-tags-column 0)

  (setq org-tag-alist '(
                        ("detection" . ?d) ("automation" . ?a) ("emailsec" . ?E)
                        ("edr" . ?e) ("ai" . ?A) ("management" . ?m)
                        ;; other tags
                        ("documentation" . ?D) ("procedures" . ?P)
                        ("knowledge" . ?K) ("training" . ?T)
                        ("task" . ?t)
                        ;; based on my current role
                        ("okr" . ?o) ("people" . ?p)))
  )

(when my-workenvironment-p
  (setq org-todo-keywords
        '(
          ;; Status for tasks
          (sequence "TODO(t)" "WAITING(w@/!)" "ONGOING(o@/!)" "|" "DONE(d@/!)" "DELEGATED(D@/!)" "CANCELED(c@/!)")))
  )

(when my-workenvironment-p
  ;; (defvar my/direct-reports nil
  ;;   "List of direct reports for 1:1 capture templates. Loaded from my-data-dir.")
  ;; (defvar my/jira-url nil
  ;;   "Jira instance URL. Loaded from my-data-dir, not committed to git.")

  (defvar my/capture-person nil
    "Employee name selected during org-capture, shared between target and template body.")

  ;; Work owns the full template list — reset it, then add each entry.
  (setq org-capture-templates '())

  (add-to-list 'org-capture-templates
               '("i" "Inbox (work)" entry
                 (file+headline my-work-file "Inbox")
                 "** TODO %? %^g"))

  (add-to-list 'org-capture-templates
               '("b" "Backlog entry." entry
                 (file+headline my-backlog-file "Backlog")
                 "** TODO %?\n"))

  (add-to-list 'org-capture-templates
               '("m" "Work meeting notes" entry
                 (file my-meetings-file)
                 "* %T - %?\n** Notes\n\n** Actions\n** Questions\n"
                 :jump-to-captured t))

  ;; people meetings
  (add-to-list 'org-capture-templates
  	     '("p" "1:1 meeting" entry
  	       (function (lambda ()
  			   (let ((person (completing-read "Employee: "
  							  my/direct-reports)))
  			     (setq my/capture-person person)
  			     (find-file my-people-file)
  			     (goto-char (org-find-olp (list my-people-file person))))))
  	       "** %<%Y-%m-%d %a %H:%M> - follow-up\n*** Notes\n%?\n*** Actions\n*** Questions\n"
  	       :jump-to-captured t))

  ;; And a quick action capture from within a 1:1 note:
  (add-to-list 'org-capture-templates
  	     '("P" "People action item" entry
  	       (function (lambda ()
  			   (let ((person (completing-read "Employee: "
  							  my/direct-reports)))
  			     (setq my/capture-person person)
  			     (find-file my-people-file)
  			     (goto-char (org-find-olp (list my-people-file person))))))
  	       "*** TODO [%(identity my/capture-person)] %<%Y-%m-%d %a %H:%M> %? :people:%(downcase my/capture-person):\n    DEADLINE: %^t"
  	       :jump-to-captured t))
  )

(when my-workenvironment-p
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)
                                 (python . t)))
  )

(let ((buffers (my/key-define-submap "b" "buffers")))
  (my/key-define "o" "org scratch" #'my/scratch-org-buffer buffers))
