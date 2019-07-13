(autoload 'org "org" "Org mode." t)

(setq bt/org-dir "~/sync/org/")
(defun bt/org-path (x) (concat bt/org-dir x))

(setq org-agenda-files (list (bt/org-path "todo.org")))

(setq org-todo-keywords
      '((sequence
         "TODO"
         "{ }"
         "{waiting}"

         "|"

         "DONE"
         "{skip}"
         "{followup}"
         "{X}"
         )))


;; this is the default, some files override this
(setq org-archive-location (bt/org-path "archive/_.org::"))

(setq org-startup-indented t)      ;; indent tasks and only show one star
(setq org-catch-invisible-edits t) ;; don't allow edits to collapsed parts of a buffer
;; (setq org-startup-folded 'content) ;; show all headings at startup
(setq org-enforce-todo-dependencies t) ;; can't finish a task with unfinished children
(setq org-blank-before-new-entry nil) ;; no blank line between heading for org-metareturn
                                      ;; (`auto` setting wasn't working very well)


(add-hook
 'org-mode-hook
 (lambda ()
   (defun bt/org-update-all-statistics ()
     (interactive)
     (let ((current-prefix-arg 4)) ;; emulate C-u
       (call-interactively 'org-update-statistics-cookies)
       ))

   (add-hook 'before-save-hook 'org-align-all-tags)
   (add-hook 'before-save-hook 'bt/org-update-all-statistics)

   (define-key my-keys-minor-mode-map (kbd "M-'")   '(lambda () (interactive)))

   (defun bt/org-journal-entry ()
     (interactive)
     (end-of-buffer)
     (insert "\n\n")
     (delete-blank-lines)
     (insert "\n")
     (bt/org-insert-datetime)
     (insert "\n"))

   (defun bt/org-insert-datetime ()
     (interactive)
     (setq current-prefix-arg '(16))      ; C-u C-u
     (call-interactively 'org-time-stamp)
     )

   (defun bt/org-insert-date ()
     (interactive)
     (bt/org-insert-datetime)     ;; <2019-03-17 Sun 13:42>|
     (backward-char)              ;; <2019-03-17 Sun 13:42|>
     (org-delete-backward-char 6) ;; <2019-03-17 Sun|>
     (forward-char)               ;; <2019-03-17 Sun>|
     )

   (visual-line-mode)    ;; wrap long lines

   (defun bt/org-beginning-of-line ()
     (interactive)
     (beginning-of-line)
     (search-forward " ")
     )

   (defun bt/org-insert-todo ()
     (interactive)
     (let ((current-prefix-arg 4)) ;; emulate C-u
       (call-interactively 'org-meta-return)
       )
     (org-todo "{ }")
     )

   (defun bt/org-add-todo-counter ()
     (interactive)
     (bt/org-beginning-of-line)
     (org-todo "")
     (insert "[/]")
     (org-ctrl-c-ctrl-c)
     (org-todo "{ }")
     (insert " "))

   ;; (defun bt/org-insert-todo-with-counter ()
   ;;   (interactive)
   ;;   (bt/org-insert-todo)
   ;;   (insert "[/]")
   ;;   (org-ctrl-c-ctrl-c))

   (defun bt/org-journaling-on ()
     (interactive)
     (define-key my-keys-minor-mode-map (kbd "M-RET") 'bt/org-journal-entry)
     ;; (define-key my-keys-minor-mode-map (kbd "M-RET") 'org-return)
     )

   (defun bt/org-journaling-off ()
     (interactive)
     (define-key my-keys-minor-mode-map (kbd "RET") 'org-return)
     )

   ;; (define-key my-keys-minor-mode-map (kbd "M-# C-n") 'bt/org-journal-entry)
   (define-key my-keys-minor-mode-map (kbd "M-# D") 'bt/org-insert-date)
   (define-key my-keys-minor-mode-map (kbd "M-# T") 'bt/org-insert-datetime)

   (define-key my-keys-minor-mode-map (kbd "M-# C-c") 'bt/org-add-todo-counter)
   (define-key my-keys-minor-mode-map (kbd "M-# C-t") (lambda () (interactive) (org-todo "{ }")))
   (define-key my-keys-minor-mode-map (kbd "M-# C-d") (lambda () (interactive) (org-todo "{X}")))
   (define-key my-keys-minor-mode-map (kbd "M-# C-s") (lambda () (interactive) (org-todo "{skip}")))
   (define-key my-keys-minor-mode-map (kbd "M-# C-M-h") (lambda () (interactive) (org-todo "{waiting}")))
   (define-key my-keys-minor-mode-map (kbd "M-# C-f") (lambda () (interactive) (org-todo "{followup}")))
   (define-key my-keys-minor-mode-map (kbd "M-# C-SPC") (lambda () (interactive) (org-todo "")))

   (define-key my-keys-minor-mode-map (kbd "M-# M-a") 'bt/org-beginning-of-line)
   (define-key my-keys-minor-mode-map (kbd "M-n") 'org-metadown)
   (define-key my-keys-minor-mode-map (kbd "M-p") 'org-metaup)

   (defun insert-checkbox-item-on-next-line ()
     (interactive)
     (end-of-line)
     (insert "\n - [ ] ")
     )

   (defun insert-checkbox-item ()
     (interactive)
     (beginning-of-line)
     (insert " - [ ] ")
     )

   (defun current-line-empty-p ()
     (save-excursion
       (beginning-of-line)
       (looking-at "[[:space:]]*$")))

   (defun dwim-insert-checkbox-item ()
     (interactive)
     (if (current-line-empty-p)
         (insert-checkbox-item)
       (insert-checkbox-item-on-next-line)
       ))

   (define-key my-keys-minor-mode-map (kbd "M-# C-n") 'org-meta-return)
   (define-key my-keys-minor-mode-map (kbd "M-# C-m") 'bt/org-insert-todo)

   ;; iterm remaps C-; to M-#
   (define-key my-keys-minor-mode-map (kbd "M-# b") 'org-backward-heading-same-level)
   (define-key my-keys-minor-mode-map (kbd "M-# f") 'org-forward-heading-same-level)
   (define-key my-keys-minor-mode-map (kbd "M-# u") 'outline-up-heading)
   (define-key my-keys-minor-mode-map (kbd "M-# p") 'outline-previous-visible-heading)
   (define-key my-keys-minor-mode-map (kbd "M-# n") 'outline-next-visible-heading)

   ;; (define-key my-keys-minor-mode-map (kbd "C-M-n B") 'org-timestamp-down)
   ;; (define-key my-keys-minor-mode-map (kbd "C-M-e B") 'org-timestamp-up)

   ;; subtree modification
   (define-key my-keys-minor-mode-map (kbd "M-# B") 'org-promote-subtree)
   (define-key my-keys-minor-mode-map (kbd "M-# F") 'org-demote-subtree)
   (define-key my-keys-minor-mode-map (kbd "M-# A") 'org-archive-subtree)

   (define-key my-keys-minor-mode-map (kbd "M-# C-k") 'org-cut-subtree)
   (define-key my-keys-minor-mode-map (kbd "M-# C-v") 'org-cut-subtree)
   (define-key my-keys-minor-mode-map (kbd "M-# M-v") 'org-copy-subtree)
   (define-key my-keys-minor-mode-map (kbd "M-# C-y") 'org-paste-subtree)

   (smartrep-define-key
       my-keys-minor-mode-map
       "M-# M-#"
     '(
       ;; basic navigation
       ("C-b" . 'backward-char)
       ("C-f" . 'forward-char)
       ("C-n" . 'next-line)
       ("C-p" . 'previous-line)
       ("C-a" . 'org-beginning-of-line)
       ("C-e" . 'org-end-of-line)

       ;; subtree navigation
       ("b" . 'org-backward-heading-same-level)
       ("f" . 'org-forward-heading-same-level)
       ("u" . 'outline-up-heading)
       ("p" . 'outline-previous-visible-heading)
       ("n" . 'outline-next-visible-heading)
       ("TAB" . 'org-cycle)
       ("C-i" . 'org-cycle)

       ;; subtree modification
       ("B" . 'org-promote-subtree)
       ("F" . 'org-demote-subtree)
       ("A" . 'org-archive-subtree)

       ("1" . (lambda () (interactive) (org-priority ?A)))
       ("2" . (lambda () (interactive) (org-priority ?B)))
       ("3" . (lambda () (interactive) (org-priority ?C)))

       ("-" . 'org-ctrl-c-minus)
       ("*" . 'org-ctrl-c-star)

       ;; these are canceling the smartrep
       ;; ("M-p" . 'org-metaup)
       ;; ("M-n" . 'org-metadown)
       ("t" . (lambda () (interactive) (org-todo "{ }")))
       ("d" . (lambda () (interactive) (org-todo "{X}")))
       ("s" . (lambda () (interactive) (org-todo "{skip}")))
       ("w" . (lambda () (interactive) (org-todo "{waiting}")))
       ("l" . (lambda () (interactive) (org-todo "{followup}")))
       ("M-C-f" . (lambda () (interactive) (org-todo "{followup}")))
       ;; not working
       ;; ("C-M-p" . (lambda () (interactive) (org-todo "PEND")))
       ;; ("C-M-n" . (lambda () (interactive) (org-todo "NOTE")))
       ("SPC" . (lambda () (interactive) (org-todo "")))
       ;; looks like anything with M- will cancel smartrep
       ;; ("M-v" . 'org-copy-subtree)
       ("C-v" . 'org-cut-subtree)
       ("C-y" . 'org-paste-subtree)

       ))

   (add-to-list 'org-structure-template-alist '("r" "#+BEGIN_SRC ruby\n?\n#+END_SRC"))
   (add-to-list 'org-structure-template-alist '("o" "#+BEGIN_SRC ocaml\n?\n#+END_SRC"))

   ))
