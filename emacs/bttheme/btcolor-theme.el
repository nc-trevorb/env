;;; Local Variables:
;;; eval: (rainbow-mode)
;;; End:

(deftheme btcolor
  "simple theme that uses terminal's colors, mostly copied from badger-theme.el")

(defvar btcolor/default-colors-alist
  '(
    ("white" . "white")
    ("brightwhite" . "brightwhite")
    ("black" . "black")
    ("brightblack" . "brightblack")

    ("red" . "red")
    ("brightred" . "brightred")

    ("green" . "green")
    ("brightgreen" . "brightgreen")

    ("blue" . "blue")
    ("brightblue" . "brightblue")

    ("yellow" . "yellow")
    ("brightyellow" . "brightyellow")

    ("cyan" . "cyan")
    ("brightcyan" . "brightcyan")

    ("magenta" . "magenta")
    ("brightmagenta" . "brightmagenta")

    ("darkgray" . "color-234")
    ("gray" . "color-237")
    ("lightgray" . "color-240")

    ))

(defmacro btcolor/with-color-variables (&rest body)
  "`let' bind all colors defined in `btcolor/colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
		   btcolor/default-colors-alist))
     ,@body))

(btcolor/with-color-variables
  (custom-theme-set-faces
   'btcolor

   `(default ((t (:foreground ,white :background ,black))))
   `(linum ((t (:foreground ,lightgray :background ,brightblack))))
   `(region ((t (:background ,gray))))

   ;; match at cursor
   `(isearch ((t (:background ,brightyellow :foreground ,black))))
   ;; other matches
   `(lazy-highlight ((t (:background ,green :foreground ,black :weight bold))))

   `(success ((t (:foreground ,green :weight bold))))
   `(warning ((t (:foreground ,red :weight bold))))
   `(error ((t (:background "color-124" :weight bold))))

   `(whitespace-newline ((t (:foreground ,black :background ,black))))
   `(whitespace-trailing ((t (:foreground ,darkgray :background ,darkgray))))

   `(Mode-line ((t (:background ,green :foreground ,brightblack ))))
   ;; `(mode-line-inactive ((t (:background ,gray :foreground ,white ))))
   `(mode-line-inactive ((t (:inherit linum))))
   `(mode-line-buffer-id ((t (:foreground ,white :weight bold))))
   `(minibuffer-prompt ((t (:foreground ,blue))))

   `(font-lock-builtin-face ((t (:foreground ,brightblue))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,lightgray))))
   `(font-lock-comment-face ((t (:foreground ,lightgray))))
   `(font-lock-constant-face ((t (:foreground ,magenta))))
   `(font-lock-function-name-face ((t (:foreground ,red))))
   `(font-lock-keyword-face ((t (:foreground ,blue))))
   `(font-lock-negation-char-face ((t (:foreground ,red))))
   `(font-lock-string-face ((t (:foreground ,yellow))))
   `(font-lock-type-face ((t (:foreground ,green))))
   `(font-lock-variable-name-face ((t (:foreground ,magenta))))

   `(helm-selection ((t (:background ,gray :weight bold))))
   `(helm-source-header ((t (:background ,blue :weight bold))))
   `(helm-match ((t (:foreground ,brightmagenta :weight bold))))

   `(deadgrep-filename-face ((t (:foreground ,green :weight bold))))
   `(deadgrep-match-face ((t (:background ,red))))


   ;; other sections
   `(diff-removed ((t (:foreground ,red))))
   `(diff-added ((t (:foreground ,green))))

   ;; current section
   `(diff-refine-removed ((t (:foreground ,brightred :background ,darkgray))))
   `(diff-refine-added ((t (:foreground ,brightgreen :background ,darkgray))))


;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,darkgray))))
   `(magit-section-heading             ((t (:foreground ,yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,red :weight bold))))

   `(magit-diff-added ((t (:inherit diff-added))))
   `(magit-diff-added-highlight ((t (:inherit diff-refine-added))))
   `(magit-diff-removed ((t (:inherit diff-removed))))
   `(magit-diff-removed-highlight ((t (:inherit diff-refine-removed))))

   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,darkgray :weight bold))))
   ;; `(magit-diff-file-heading-selection ((t (:background ,green :foreground ,cyan :weight bold))))
   ;; current section
   `(magit-diff-hunk-heading-highlight ((t (:background ,darkgray :foreground ,brightcyan))))
   `(magit-diff-hunk-heading           ((t (:foreground ,cyan))))
                                        ; `(magit-diff-hunk-heading-selection ((t (:background ,brightmagenta :foreground ,cyan))))
   `(magit-diff-lines-heading          ((t (:foreground ,cyan))))
   `(magit-diff-context-highlight      ((t (:background ,darkgray))))

   `(magit-diffstat-added   ((t (:foreground ,green))))
   `(magit-diffstat-removed ((t (:foreground ,red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,brightgreen :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,lightgray    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,blue  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,cyan))))
   `(magit-log-date      ((t (:foreground ,gray))))
   `(magit-log-graph     ((t (:foreground ,gray))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,yellow))))
   `(magit-sequence-stop ((t (:foreground ,green))))
   `(magit-sequence-part ((t (:foreground ,brightyellow))))
   `(magit-sequence-head ((t (:foreground ,blue))))
   `(magit-sequence-drop ((t (:foreground ,red))))
   `(magit-sequence-done ((t (:foreground ,gray))))
   `(magit-sequence-onto ((t (:foreground ,gray))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,green))))
   `(magit-bisect-skip ((t (:foreground ,yellow))))
   `(magit-bisect-bad  ((t (:foreground ,red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,gray :foreground ,blue))))
   `(magit-blame-hash    ((t (:background ,gray :foreground ,blue))))
   `(magit-blame-name    ((t (:background ,gray :foreground ,cyan))))
   `(magit-blame-date    ((t (:background ,gray :foreground ,cyan))))
   `(magit-blame-summary ((t (:background ,gray :foreground ,blue :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,gray))))
   `(magit-hash           ((t (:foreground ,gray))))
   `(magit-tag            ((t (:foreground ,cyan :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,blue   :weight bold))))
   `(magit-refname        ((t (:background ,lightgray :foreground ,white :weight bold))))
   `(magit-refname-stash  ((t (:background ,lightgray :foreground ,white :weight bold))))
   `(magit-refname-wip    ((t (:background ,lightgray :foreground ,white :weight bold))))
   `(magit-signature-good      ((t (:foreground ,green))))
   `(magit-signature-bad       ((t (:foreground ,red))))
   `(magit-signature-untrusted ((t (:foreground ,yellow))))
   `(magit-signature-expired   ((t (:foreground ,cyan))))
   `(magit-signature-revoked   ((t (:foreground ,magenta))))
   `(magit-cherry-unmatched    ((t (:foreground ,cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,magenta))))
   `(magit-reflog-commit       ((t (:foreground ,green))))
   `(magit-reflog-amend        ((t (:foreground ,magenta))))
   `(magit-reflog-merge        ((t (:foreground ,green))))
   `(magit-reflog-checkout     ((t (:foreground ,blue))))
   `(magit-reflog-reset        ((t (:foreground ,red))))
   `(magit-reflog-rebase       ((t (:foreground ,magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,green))))
   `(magit-reflog-remote       ((t (:foreground ,cyan))))
   `(magit-reflog-other ((t (:foreground ,cyan))))

;;;;; ocaml
   `(tuareg-font-lock-extension-node-face ((t (:background ,gray :foreground ,blue))))
   `(tuareg-font-lock-infix-extension-node-face ((t (:background ,gray :foreground ,blue))))


;;;;; org-mode
   `(outline-1 ((t (:foreground ,magenta))))
   `(outline-2 ((t (:foreground ,cyan))))
   `(outline-3 ((t (:foreground ,green))))
   `(outline-4 ((t (:foreground ,blue))))
   `(outline-5 ((t (:inherit outline-1))))
   `(outline-6 ((t (:inherit outline-2))))
   `(outline-7 ((t (:inherit outline-3))))
   `(outline-8 ((t (:inherit outline-4))))

   `(org-checkbox ((t (:foreground ,brightblue :weight bold))))
   `(org-date ((t (:foreground ,brightmagenta))))
   ;; `(org-headline-done ((t (:foreground ,zenburn-green+3))))
   ;; `(org-hide ((t (:foreground ,zenburn-bg-1))))
   `(org-link ((t (:foreground ,brightcyan))))
   `(org-table ((t (:foreground ,brightcyan))))
   `(org-tag ((t (:weight bold))))

   `(org-done ((t (:weight bold :foreground ,brightgreen))))
   `(org-todo ((t (:weight bold :foreground ,brightred))))

   `(org-warning ((t (:foreground ,red))))
   `(org-code ((t (:background ,gray :foreground ,yellow))))

   `(org-ellipsis ((t (:foreground ,lightgray))))

   `(org-agenda-dimmed-todo-face ((t (:weight bold))))

   `(tuareg-font-lock-governing-face ((t (:foreground ,blue))))
   `(tuareg-font-lock-operator-face ((t (:foreground ,blue))))

   ))

(provide-theme 'btcolor)
