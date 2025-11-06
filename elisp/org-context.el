;;; org-context.el --- Helper functions for detecting Org mode context

;; Author: jcastp
;; Created: 2025-01-06
;; Keywords: org, convenience
;; Package-Requires: ((emacs "24")(cl-lib "0.5"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides helper functions for detecting various Org mode
;; contexts at point. These functions are useful for word counting,
;; navigation, and other operations that need to understand the context
;; of the current position in an Org mode buffer.
;;
;; Functions provided:
;; - org-context-in-commented-line: Check if point is in a commented line
;; - org-context-in-drawer-p: Check if point is in a drawer
;; - org-context-in-heading-p: Check if point is in a heading
;; - org-context-at-property-p: Check if point is at a property
;; - org-context-in-block-p: Check if point is inside a #+BEGIN...#+END block
;; - org-context-under-heading-with-tag-p: Check if point is under a heading with specific tags

;;; Code:

(require 'org)
(require 'cl-lib)

(defun org-context-in-commented-line ()
  "Return non-nil if point is in a commented line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*#")))

(defun org-context-in-drawer-p ()
  "Return non-nil if point is in an Org mode drawer."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*:[a-zA-Z]+:")))

(defun org-context-in-heading-p ()
  "Return non-nil if point is in an Org mode heading."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^*")))

(defun org-context-at-property-p ()
  "Return non-nil if point is at an Org mode property."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*:[a-zA-Z0-9_-]+:")))

(defun org-context-in-block-p ()
  "Return non-nil if point is inside an Org mode #+BEGIN...#+END block.
Checks for any block type (SRC, EXAMPLE, QUOTE, EXPORT, etc.)."
  (save-excursion
    (let ((current-line (line-number-at-pos)))
      ;; Search backward for opening #+BEGIN_
      (when (re-search-backward "^[ \t]*#\\+BEGIN_" nil t)
        (let ((begin-line (line-number-at-pos)))
          ;; Search forward for closing #+END_
          (when (re-search-forward "^[ \t]*#\\+END_" nil t)
            (let ((end-line (line-number-at-pos)))
              ;; Return t if current line is between the markers
              (and (>= current-line begin-line)
                   (<= current-line end-line)))))))))

(defun org-context-under-heading-with-tag-p (tags-to-ignore)
  "Return non-nil if point is under a heading that has any tag from TAGS-TO-IGNORE.
TAGS-TO-IGNORE should be a list of strings like (\"noexport\" \"ignore\" \"nowc\").
Returns nil if point is before the first heading or not under a tagged heading."
  (when tags-to-ignore
    (save-excursion
      ;; Try to find the current heading, return nil if before first heading
      (condition-case nil
          (when (org-back-to-heading t)
            (let ((heading-tags (org-get-tags)))
              ;; Check if any of the heading tags match our ignore list
              (when heading-tags
                (cl-some (lambda (tag)
                           (member tag tags-to-ignore))
                         heading-tags))))
        ;; Catch the error when before first headline and return nil
        (error nil)))))

(provide 'org-context)
;;; org-context.el ends here
