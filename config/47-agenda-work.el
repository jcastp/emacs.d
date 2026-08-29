;; -*- lexical-binding: t; -*-

;; Work agenda span: one week, starting Monday
(setq org-agenda-span 7)
(setq org-agenda-start-on-weekday 1)

;; Require CLOSED timestamp on done tasks (needed for weekly review)
(setq org-log-done 'time)

;; empty the agenda view, so we can use a modular approach
(setq org-agenda-custom-commands '())

;; ──────────────────────────────────────────────────────────────────
;; d — Daily driver
;; Open at start of day. Question: what do I work on right now?
;; ──────────────────────────────────────────────────────────────────
(add-to-list 'org-agenda-custom-commands
   	     '("d" "Daily driver"
   	       (
   		;; Priority A: explicit commitments
   		(tags-todo "+PRIORITY=\"A\""
   			   ((org-agenda-overriding-header "[ PRIORITY — must do today ]")
   			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("WAITING" "DONE" "DELEGATED" "CANCELED")))
   			    (org-agenda-sorting-strategy '(effort-up ts-up))
   			    (org-agenda-prefix-format " %i %-16:c %5e ")))

   		;; Ongoing — what you were already in the middle of
   		(tags-todo "/ONGOING"
   			   ((org-agenda-overriding-header "[ IN PROGRESS ]")
   			    (org-agenda-sorting-strategy '(priority-down effort-up ts-up))
   			    (org-agenda-prefix-format " %i %-16:c %5e ")))

   		;; Time-bound: scheduled for today, past due, active timestamps
   		(agenda ""
   			((org-agenda-span 1)
   			 (org-agenda-time-grid '((daily today require-timed)
   						 (800 1000 1200 1400 1600 1800)
   						 " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
   			 (org-deadline-warning-days 2)
   			 (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "DELEGATED" "CANCELED")))
   			 (org-agenda-sorting-strategy '(time-up priority-down))
   			 (org-agenda-overriding-header "[ SCHEDULE ]")))

   		;; Waiting — waiting on others, but worth a nudge if you have time
   		(tags-todo "/WAITING"
   			   ((org-agenda-overriding-header "[ WAITING — check in if possible ]")
   			    (org-agenda-sorting-strategy '(ts-up priority-down))
   			    (org-agenda-prefix-format " %i %-16:c ")))
   		)
   	       )
   	     )

;; ──────────────────────────────────────────────────────────────────
;; f — Focus check
;; During the day. Question: what am I doing right now?
;; ──────────────────────────────────────────────────────────────────
(add-to-list 'org-agenda-custom-commands
   	     '("f" "Focus — what am I doing right now"
   	       (
   		(tags-todo "/ONGOING"
   			   ((org-agenda-overriding-header "[ IN PROGRESS ]")
   			    (org-agenda-sorting-strategy '(priority-down effort-up ts-up))
   			    (org-agenda-prefix-format " %i %-16:c %5e ")))

   		(tags-todo "/WAITING"
   			   ((org-agenda-overriding-header "[ WAITING — waiting on others ]")
   			    (org-agenda-sorting-strategy '(ts-up priority-down))
   			    (org-agenda-prefix-format " %i %-16:c ")))
   		)
   	       )
   	     )

;; ──────────────────────────────────────────────────────────────────
;; W — Weekly planning
;; Monday morning. Question: what are my commitments this week?
;; ──────────────────────────────────────────────────────────────────
(add-to-list 'org-agenda-custom-commands
   	     '("W" "Weekly planning — Monday"
   	       (
   		;; Hard commitments: what has a deadline this fortnight
   		(agenda ""
   			((org-agenda-span 14)
   			 (org-agenda-time-grid nil)
   			 (org-agenda-entry-types '(:deadline))
   			 (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "DELEGATED" "CANCELED")))
   			 (org-agenda-overriding-header "[ DEADLINES — next 2 weeks ]")
   			 (org-agenda-sorting-strategy '(deadline-up priority-down))))

   		;; Strategic: OKR-linked tasks that need attention
   		(tags-todo "+okr"
   			   ((org-agenda-overriding-header "[ OKR — strategic tasks ]")
   			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "DELEGATED" "CANCELED")))
   			    (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))
   			    (org-agenda-prefix-format " %i %-16:c %5e ")))

   		;; In flight: what was already ongoing going into this week
   		(tags-todo "/ONGOING"
   			   ((org-agenda-overriding-header "[ ONGOING — carryover from last week ]")
   			    (org-agenda-sorting-strategy '(priority-down effort-up))
   			    (org-agenda-prefix-format " %i %-16:c %5e ")))

   		;; Waiting: may need escalation before week is out
   		(tags-todo "/WAITING"
   			   ((org-agenda-overriding-header "[ WAITING — may need escalation ]")
   			    (org-agenda-sorting-strategy '(ts-up priority-down))
   			    (org-agenda-prefix-format " %i %-16:c ")))

   		;; Priority A not yet started: strong candidates for this week
   		(tags-todo "+PRIORITY=\"A\"/TODO"
   			   ((org-agenda-overriding-header "[ PRIORITY A — not yet started ]")
   			    (org-agenda-sorting-strategy '(effort-up ts-up))
   			    (org-agenda-prefix-format " %i %-16:c %5e ")))

   		;; Priority B not yet started: good-if-you-get-to-it work
   		(tags-todo "+PRIORITY=\"B\"/TODO"
   			   ((org-agenda-overriding-header "[ PRIORITY B — fill-in work ]")
   			    (org-agenda-sorting-strategy '(effort-up ts-up))
   			    (org-agenda-prefix-format " %i %-16:c %5e ")))
   		)
   	       )
   	     )

;; ──────────────────────────────────────────────────────────────────
;; R — Weekly review
;; Friday afternoon. Question: what did I do, what is stuck?
;; ──────────────────────────────────────────────────────────────────
(add-to-list 'org-agenda-custom-commands
   	     '("R" "Weekly review — Friday"
   	       (
   		;; What you actually finished: the win list
   		(tags "+CLOSED>=\"<-7d>\""
   		      ((org-agenda-overriding-header "[ COMPLETED THIS WEEK ]")
   		       (org-agenda-skip-function
   			'(org-agenda-skip-entry-if 'nottodo '("DONE")))
   		       (org-agenda-sorting-strategy '(timestamp-down))
   		       (org-agenda-prefix-format " %i %-16:c ")))

   		;; What you handed off: track for follow-up
   		(tags "+CLOSED>=\"<-7d>\""
   		      ((org-agenda-overriding-header "[ DELEGATED THIS WEEK ]")
   		       (org-agenda-skip-function
   			'(org-agenda-skip-entry-if 'nottodo '("DELEGATED")))
   		       (org-agenda-sorting-strategy '(timestamp-down))
   		       (org-agenda-prefix-format " %i %-16:c ")))

   		;; What is still waiting: decide to chase or park
   		(tags-todo "/WAITING"
   			   ((org-agenda-overriding-header "[ STILL WAITING — chase or park? ]")
   			    (org-agenda-sorting-strategy '(ts-up priority-down))
   			    (org-agenda-prefix-format " %i %-16:c ")))

   		;; What has no schedule yet: needs to go somewhere next week
   		(tags-todo "/TODO"
   			   ((org-agenda-overriding-header "[ UNSCHEDULED — needs a home next week ]")
   			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
   			    (org-agenda-sorting-strategy '(priority-down effort-up))
   			    (org-agenda-prefix-format " %i %-16:c %5e ")))
   		)
   	       )
   	     )

;; ──────────────────────────────────────────────────────────────────
;; ca — By domain
;; Planning sessions / 1:1s. Question: status by area?
;; ──────────────────────────────────────────────────────────────────
(add-to-list 'org-agenda-custom-commands
   	     '("ca" "By domain area"
   	       (
   		(tags-todo "+management"
   			   ((org-agenda-overriding-header "[ MANAGEMENT ]")
   			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "DELEGATED" "CANCELED")))
   			    (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))
   			    (org-agenda-prefix-format " %i %-16:c %5e ")))

   		(tags-todo "+detection"
   			   ((org-agenda-overriding-header "[ DETECTION ]")
   			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "DELEGATED" "CANCELED")))
   			    (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))
   			    (org-agenda-prefix-format " %i %-16:c %5e ")))

   		(tags-todo "+automation"
   			   ((org-agenda-overriding-header "[ AUTOMATION ]")
   			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "DELEGATED" "CANCELED")))
   			    (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))
   			    (org-agenda-prefix-format " %i %-16:c %5e ")))

   		(tags-todo "+emailsec"
   			   ((org-agenda-overriding-header "[ EMAIL SECURITY ]")
   			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "DELEGATED" "CANCELED")))
   			    (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))
   			    (org-agenda-prefix-format " %i %-16:c %5e ")))

   		(tags-todo "+edr"
   			   ((org-agenda-overriding-header "[ EDR ]")
   			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "DELEGATED" "CANCELED")))
   			    (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))
   			    (org-agenda-prefix-format " %i %-16:c %5e ")))

   		(tags-todo "+ai"
   			   ((org-agenda-overriding-header "[ AI ]")
   			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "DELEGATED" "CANCELED")))
   			    (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))
   			    (org-agenda-prefix-format " %i %-16:c %5e ")))


   		;; Catch-all: tasks with no domain tag — they need one
   		(tags-todo "-detection-automation-emailsec-edr-ai-management"
   			   ((org-agenda-overriding-header "[ UNTAGGED — needs a domain tag ]")
   			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "DELEGATED" "CANCELED")))
   			    (org-agenda-sorting-strategy '(priority-down effort-up))
   			    (org-agenda-prefix-format " %i %-16:c %5e ")))
   		)
   	       )
   	     )

;; ──────────────────────────────────────────────────────────────────
;; co — OKR alignment
;; Quarterly check-ins / manager 1:1. Question: are OKRs on track?
;; ──────────────────────────────────────────────────────────────────
(add-to-list 'org-agenda-custom-commands
   	     '("co" "OKR alignment"
   	       (
   		(tags-todo "+okr/ONGOING"
   			   ((org-agenda-overriding-header "[ OKR — in progress ]")
   			    (org-agenda-sorting-strategy '(priority-down effort-up ts-up))
   			    (org-agenda-prefix-format " %i %-20:c %5e ")))

   		(tags-todo "+okr/WAITING"
   			   ((org-agenda-overriding-header "[ OKR — WAITING — escalate ]")
   			    (org-agenda-sorting-strategy '(ts-up priority-down))
   			    (org-agenda-prefix-format " %i %-20:c ")))

   		(tags-todo "+okr/TODO"
   			   ((org-agenda-overriding-header "[ OKR — not yet started ]")
   			    (org-agenda-sorting-strategy '(priority-down effort-up))
   			    (org-agenda-prefix-format " %i %-20:c %5e ")))

   		(tags "+okr/DONE"
   		      ((org-agenda-overriding-header "[ OKR — recently completed ]")
   		       (org-agenda-sorting-strategy '(timestamp-down))
   		       (org-agenda-max-entries 20)
   		       (org-agenda-prefix-format " %i %-20:c ")))

   		(tags "+okr/DELEGATED"
   		      ((org-agenda-overriding-header "[ OKR — delegated ]")
   		       (org-agenda-sorting-strategy '(timestamp-down))
   		       (org-agenda-prefix-format " %i %-20:c ")))
   		)
   	       )
   	     )

;; ──────────────────────────────────────────────────────────────────
;; ci — Inbox triage
;; After capture bursts. Question: what needs to be processed?
;; ──────────────────────────────────────────────────────────────────
(add-to-list 'org-agenda-custom-commands
   	     '("ci" "Inbox triage — process captured items"
   	       (
   		;; Tasks with no domain: they are floating and invisible in the domain view
   		(tags-todo "-detection-automation-emailsec-edr-ai-okr-management"
   			   ((org-agenda-overriding-header "[ NEEDS DOMAIN TAG ]")
   			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "DELEGATED" "CANCELED")))
   			    (org-agenda-sorting-strategy '(ts-up))
   			    (org-agenda-prefix-format " %i %-16:c ")))

   		;; Tasks without effort: cannot be scheduled
   		(tags-todo "EFFORT=\"\""
   			   ((org-agenda-overriding-header "[ NEEDS EFFORT ESTIMATE ]")
   			    (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "DELEGATED" "CANCELED")))
   			    (org-agenda-sorting-strategy '(priority-down ts-up))
   			    (org-agenda-prefix-format " %i %-16:c ")))
   		)
   	       )
   	     )

;; people tasks
(add-to-list 'org-agenda-custom-commands
   	     '("p" "People - direct reports"
	       (
		;; All open action items from 1:1s
		(tags-todo "+people"
			   ((org-agenda-overriding-header "[ OPEN PEOPLE ACTIONS ]")
			    (org-agenda-sorting-strategy '(priority-down ts-up))
			    (org-agenda-prefix-format " %i %-12:c ")))
		 )
	       )
   	     )

(org-super-agenda-mode 1)

;; Super-agenda groups for the default `C-c a a` view
(setq org-super-agenda-groups
      '((:name "Overdue"        :deadline past          :order 1)
        (:name "Today"          :time-grid t
                                :scheduled today        :order 3)
        (:name "Priority A"     :priority "A"           :order 10)
        (:name "OKR"            :tag "okr"              :order 11)
        (:name "In progress"    :todo "ONGOING"         :order 20)
        (:name "Waiting"        :todo "WAITING"         :order 21)
        (:name "Detection"      :tag "detection"        :order 30)
        (:name "Automation"     :tag "automation"       :order 31)
        (:name "Email Security" :tag "emailsec"         :order 32)
        (:name "EDR"            :tag "edr"              :order 33)
        (:name "AI"             :tag "ai"               :order 34)
        (:name "Management"     :tag "management"       :order 35)
        (:name "Priority C"     :priority "C"           :order 90)
        (:name "People"         :tag "people"           :order 95)
        (:name "Other"          :anything t             :order 99)))

(with-eval-after-load 'org-ql-view
  (add-to-list 'org-ql-views
               (cons "Work: All open tasks"
                     (list :buffers-files #'org-agenda-files
                           :query '(todo "TODO" "WAITING" "ONGOING")
                           :sort '(priority date)
                           :super-groups nil
                           :title "Work: All open tasks"))
               t)

  (add-to-list 'org-ql-views
               (cons "Work: All open tasks (by tag)"
                     (list :buffers-files #'org-agenda-files
                           :query '(todo "TODO" "WAITING" "ONGOING")
                           :sort '(priority date)
                           :super-groups '((:auto-tags t))
                           :title "Work: All open tasks (by tag)"))
               t)

  (add-to-list 'org-ql-views
               (cons "Work: All open tasks (by effort)"
                     (list :buffers-files #'org-agenda-files
                           :query '(todo "TODO" "WAITING" "ONGOING")
                           :sort '(effort)
                           :super-groups nil
                           :title "Work: All open tasks (by effort)"))
               t))

(defvar my/last-report nil
  "Last direct report opened via `my/open-report'.
Persisted across sessions via `savehist-additional-variables'.")

(defun my/open-report (person)
  "Open the per-person 1:1 notes for PERSON in `my-people-file'.
Interactively, prompt among `my/direct-reports', defaulting to the last report
opened (press RET to reopen it).  Create PERSON's heading if it does not exist
yet, then narrow to their subtree so the running 1:1 notes and open action
items sit together in one focused view."
  (interactive
   (let* ((reports (and (boundp 'my/direct-reports) my/direct-reports))
          ;; surface the last-used report first so it is the primary choice
          (ordered (if (and my/last-report (member my/last-report reports))
                       (cons my/last-report (remove my/last-report reports))
                     reports)))
     (list (completing-read (format-prompt "Report" my/last-report)
                            ordered nil nil nil nil my/last-report))))
  (setq my/last-report person)
  (find-file my-people-file)
  (widen)
  (let ((marker (condition-case nil
                    (org-find-olp (list my-people-file person))
                  (error nil))))
    (unless marker
      ;; first 1:1 with this person — create their heading
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* %s\t:%s:\n" person (downcase person)))
      (setq marker (org-find-olp (list my-people-file person))))
    (goto-char marker))
  (org-narrow-to-subtree)
  (if (fboundp 'org-fold-show-subtree)
      (org-fold-show-subtree)
    (org-show-subtree)))

(keymap-global-set "<f6>" #'my/open-report)

(when (file-exists-p my-work-file)
  ;;(find-file my-work-file)
  (persp-state-load "~/Nextcloud/config/.emacs.d/perspectives/work-persp")
  )

(my/key-define "p" "open report" #'my/open-report)
