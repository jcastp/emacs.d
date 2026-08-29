;; -*- lexical-binding: t; -*-

(when my-homeenvironment-p
  ;; Agenda styling
  (setq
   org-agenda-tags-column 'auto
   org-agenda-block-separator ?─
   org-agenda-breadcrumbs-separator " ❱ "
   ;;org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t%b% s")
   org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")  			    
  			    (todo . " %i %-12:c")
  			    (tags . " %i %-12:c")
  			    (search . " %i %-12:c"))
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  )

(when my-homeenvironment-p
  (customize-set-value
   'org-agenda-category-icon-alist
   `(
     ("calendar" "~/Nextcloud/config/icons/calendar.svg" nil nil :ascent center :mask heuristic)
     ("tasks" "~/Nextcloud/config/icons/check-square.svg" nil nil :ascent center :mask heuristic)
     ("projects" "~/Nextcloud/config/icons/list.svg" nil nil :ascent center :mask heuristic)
     ("financial" "~/Nextcloud/config/icons/dollar-sign.svg" nil nil :ascent center :mask heuristic)
     ("birthdays" "~/Nextcloud/config/icons/heart.svg" nil nil :ascent center :mask heuristic)
     ("revision" "~/Nextcloud/config/icons/shuffle.svg" nil nil :ascent center :mask heuristic)
     ("habits" "~/Nextcloud/config/icons/refresh-ccw.svg" nil nil :ascent center :mask heuristic)
     ("care" "~/Nextcloud/config/icons/heart.svg" nil nil :ascent center :mask heuristic)))
  )

(when my-homeenvironment-p
  (setq org-agenda-files (list
                           "~/Nextcloud/agenda/tasks.org"
  			 "~/Nextcloud/agenda/someday.org"
  			 my-booklist-file))
  )

(when my-homeenvironment-p
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-timestamp-if-done t)
  )

(when my-homeenvironment-p
  ;; all the targets
  (setq org-refile-targets '(
                             (org-agenda-files :maxlevel . 9)
                             ("referencias.org" :maxlevel . 9)
                             ("someday.org" :maxlevel . 9) ;; this is called someday.org now
                             ("~/Nextcloud/escritura/retazos/ideas.org" :maxlevel . 9)))

  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  ;;(setq org-refile-use-outline-path t)                  ; Show full paths for refiling
  (setq org-refile-use-outline-path 'file)                  ; Show full paths for refiling
  )

(when my-homeenvironment-p
  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-habit t))
  )

(when my-workenvironment-p
  (customize-set-value
   'org-agenda-category-icon-alist
   `(
     ("calendar" "~/Nextcloud/config/icons/calendar.svg" nil nil :ascent center :mask heuristic)
     ("tasks" "~/Nextcloud/config/icons/check-square.svg" nil nil :ascent center :mask heuristic)
     ("projects" "~/Nextcloud/config/icons/list.svg" nil nil :ascent center :mask heuristic)
     ("okr" "~/Nextcloud/config/icons/target.svg" nil nil :ascent center :mask heuristic)
     ("management" "~/Nextcloud/config/icons/users.svg" nil nil :ascent center :mask heuristic)
     ("inbox" "~/Nextcloud/config/icons/inbox.svg" nil nil :ascent center :mask heuristic)))
  )

(when my-workenvironment-p
  (setq-default visual-fill-column-width 100)
  )

(when my-workenvironment-p
  ;; This needs to be changed when you change company!!!
  (setq org-agenda-files (list
  			my-work-file     ;; tasks file
  			my-meetings-file ;; meetings file
  			my-people-file   ;; people file
  			))
  )

(when my-workenvironment-p
    (setq org-refile-targets '(
                              (nil :maxlevel . 9)
                              (org-agenda-files :maxlevel . 9)
  			    (my-backlog-file :maxlevel . 9)))

    (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
    (setq org-refile-use-outline-path t)                  ; Show full paths for refiling
  )

(my/key-define "d" "daily agenda"
               (lambda () (interactive) (org-agenda nil "d")))
