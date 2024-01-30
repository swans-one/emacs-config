;;; org-slug.el --- Link to org files based on slugs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Erik Swanson

;; Author: Erik Swanson <theerikswanson@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a new link type for org-mode using the schema
;; "slug:".
;;
;; The "slug" link type is used to link to another org file under the
;; same "slug-root". The slug for a file is specified by an org
;; keyword in the format `#+SLUG: <slug>'.
;;
;; The "slug-root" is determined by an empty `.slug-root' file at the
;; root of the project.

;;; Code:

;; TODO: Actually consume this variable
(defcustom org-slug-create-for-new-slug t
  "Should a new buffer / file be created when a slug doesn't exist"
  :type 'boolean
)


(defun org-slug-find-file-from-slug (slug &optional root)
  "Find a file containing a slug, SLUG, in a file under directory ROOT."
  (unless root (setq root "./"))
  (let* ((command (format "grep -iRls --include=\*.org '#+SLUG: %s' %s" slug root))
         (grep-output (shell-command-to-string command)))
    (string-trim grep-output)))

(defun org-slug-get-root ()
  "Find a directory containing .slug-root under the current "
  (locate-dominating-file (buffer-file-name) ".org-slug"))

(defun org-slug-follow (path &optional root)
  "Follow a slug link PATH to the appropriate file.

PATH will be the part of the link after 'slug:'. If there is no
file with that slug and 'org-slug-create-for-new-slug' is 't'
then create a new file with that slug in the current directory."
  (unless root (setq root (org-slug-get-root)))
  (let* ((slug path)
         (path (org-slug-find-file-from-slug slug root)))
    (if (string= path "")
        (message (format "No file with slug: %s; under dir: %s" slug root))
      (find-file path))))


(defun tag-find-command (tag root)
  (mapconcat 'identity
             `("find"
               ,root
               "-type f"
               "-name '*.org'"
               "-exec grep --color -nH --null -e"
               ,(concat "'^" "#+.*TAG.*:.*" tag ".*" "$'")
               "\{\} ';'")
             " "))

(defun org-slug-follow-tag (path &optional root)
  "Follow a slug-tag link PATH to a listing of matching org files
in the project.

PATH will be the part of the link after 'slug-tag:'. The function
grep-find is used to look for files with matching tag directives."
  (unless root (setq root (org-slug-get-root)))
  (let* ((tag path)
         (find-command (tag-find-command tag root)))
    (grep-find find-command)))

(defun org-slug-slugify (start end)
  (interactive "r")
  (if (use-region-p)
      (let ((slug (buffer-substring start end)))
        (kill-region start end)
        (insert (format "[[slug:%s][%s]]" slug slug)))
      (message "no slug in active region")))

(defun org-slug-tagify (start end)
  (interactive "r")
  (if (use-region-p)
      (let ((tag (buffer-substring start end)))
        (kill-region start end)
        (insert (format "[[slug-tag:%s][#%s]]" tag tag)))
      (message "no tag in active region")))

(org-link-set-parameters "slug" :follow #'org-slug-follow)
(org-link-set-parameters "slug-tag" :follow #'org-slug-follow-tag)

(provide 'org-slug)
;;; org-slug.el ends here
