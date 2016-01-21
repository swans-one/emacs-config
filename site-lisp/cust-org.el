;; These are the customizations related to org mode.
;;
;; All org related customizations belong in this file. Even if they
;; would otherwise fit somewhere else (such as defuns or
;; keybindings).
;;
;; Note that a lot of inspiration was taken from:
;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgheadline46

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c r") 'org-refile)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-directory "~/org")
(setq org-default-notes-file "~/org/captured.org")

;; Set up agenda files and refile targets
(setq org-agenda-files
      '("~/org/captured.org"
        "~/org/work.org"
        "~/org/codeandsupply.org"
        "~/org/abstractions.org"
        "~/org/learning.org"
        "~/org/life.org"))
(setq org-refile-targets '((nil :maxlevel . 6)
                           (org-agenda-files :maxlevel . 6)))
(setq org-refile-use-outline-path 'file)
(setq org-completion-use-ido t)
(setq org-outline-path-complete-in-steps nil)

(defun my/open-default-notes-file ()
  (interactive)
  (find-file org-default-notes-file))
(global-set-key (kbd "C-c t") 'my/open-default-notes-file)

;; Capture Templates
;;   - task
;;   - email
;;   - blog post
;;   - thoughts
;;   - recipes
;;   - links
(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/org/captured.org" "Tasks")
        "* TODO %?")
   ("e" "Email" entry (file+headline "~/org/captured.org" "Emails")
    "* To: %^{To:} Subject: %^{Subject}\n%?")
   ("b" "Blog Post Idea" entry (file+headline "~/org/captured.org" "Blog")
    "* %?")
   ("n" "Thoughts/Notes (No action needed)" entry
    (file+headline "~/org/thoughts.org" "Thoughts") "* %?")
   ("r" "Recipe" entry (file "~/org/recipes.org")
    "* %^{Name}\n** Ingredients\n%?\n** Steps\n** Notes")
   ("l" "Link for later reading/review" entry
    (file+headline "~/org/captured.org" "Links") "* [[%c][%?]]")))

;; Think about tags
;;   - buy


(provide 'cust-org)
