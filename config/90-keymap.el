;; -*- lexical-binding: t; -*-

(defun my/find-file-command (path)
  "Return an interactive command that opens PATH."
  (lambda ()
    (interactive)
    (find-file path)))

(let ((files (my/key-define-submap "q" "direct file access")))
  (my/key-define "c" "emacs config"
                 (my/find-file-command
                  (expand-file-name "config/" my-config-dir))
                 files)
  (my/key-define "k" "emacs keys"
                 (my/find-file-command
                  (expand-file-name "tech/org-keys.org" my-nextcloud-dir))
                 files)
  (my/key-define "t" "tasks file"
                 (my/find-file-command
                  (expand-file-name "tasks.org" my-agenda-dir))
                 files)
  (when my-homeenvironment-p
    (my/key-define "e" "escritura"
                   (my/find-file-command
                    (expand-file-name "historias/novela" my-escritura-dir))
                   files)))
