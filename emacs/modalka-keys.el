;; -*- lexical-binding: t; -*-

(use-package selected
  :ensure t
  :commands selected-minor-mode

  :bind
  (:map selected-keymap
        ("w" . %wipe-selection)
        ("b" . %become-selection)
        ("t" . %trade-selection)
        ("c" . %copy-selection)
        ("d" . %dupe-selection)
        ("v" . %vanish-selection)
        )

  :config
  (%define-prefix-action "w" '%wipe)
  (%define-prefix-action "b" '%become)
  (%define-prefix-action "t" '%trade)
  (%define-prefix-action "c" '%copy)
  (%define-prefix-action "d" '%dupe)
  ;; (%define-prefix-action " " '%noop) ;; selection stays activated so this is like "select"
  )

(use-package modalka
  :init
  (defconst whitespace "[[:space:]]\\|\n")
  (defconst non-whitespace "[[:graph:]]")
  (defalias '%left 'backward-char)
  (defalias '%down 'next-line)
  (defalias '%up 'previous-line)
  (defalias '%right 'forward-char)

  (defun %repeat ()
    (interactive)
    (if %repeat-macro (call-interactively '%repeat-macro))
    (%normal))

  (defalias '%m-x-menu 'helm-M-x)

  ;; emacs uses "up"/"down" to refer to the text in the buffer moving,
  ;; % uses direction that the cursor moves
  (defalias '%scroll-up 'scroll-down-line)
  (defalias '%scroll-down 'scroll-up-line)
  (defalias '%scroll-up-page 'scroll-down)
  (defalias '%scroll-down-page 'scroll-up)

  (defalias '%switch-window-left 'windmove-left)
  (defalias '%switch-window-down 'windmove-down)
  (defalias '%switch-window-up 'windmove-up)
  (defalias '%switch-window-right 'windmove-right)

  (defalias '%split-left 'split-window-right)
  (defalias '%split-up 'split-window-below)

  (defun %split-right ()
    (interactive)
    (%split-left)
    (%switch-window-right))

  (defun %split-down ()
    (interactive)
    (%split-up)
    (%switch-window-down))

  (defalias '%open-file 'helm-projectile-find-file)
  (defalias '%buffer-list 'ibuffer)
  (defalias '%go-to-buffer 'switch-to-buffer)
  (defalias '%dired 'dired-jump)
  ;; (defalias '%locate-file 'find-file)

  (defalias '%maximize 'delete-other-windows)
  (defalias '%quit-window 'delete-window)
  (defalias '%window-undo 'winner-undo)
  (defalias '%window-redo 'winner-redo)

  (defun %quit-popups ()
    (interactive)
    (dolist (buf (buffer-list))
      (when (and (get-buffer-window buf 'visible)
                 (string-match "^\\*" (buffer-name buf)))
        (kill-buffer buf))))

  (defun %checkout-buffer ()
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

  (defalias '%start-macro 'kmacro-start-macro)
  (defalias '%end-macro 'kmacro-end-macro)
  (defalias '%run-macro 'kmacro-end-and-call-macro)

  (defun %wipe-bol () (interactive) (kill-line 0))
  (defalias '%bw-wipe-word 'backward-kill-word)
  (defalias '%wipe-word 'kill-word)
  ;; (defalias '%wipe-eol 'kill-line)
  (defalias '%indent 'indent-for-tab-command)
  (defalias '%to-line 'goto-line)

  (defalias '%bw-bow 'backward-word)
  (defalias '%bow 'forward-to-word)
  (defalias '%eow 'forward-word)
  (defalias '%bob 'beginning-of-buffer)
  (defalias '%eob 'end-of-buffer)
  (defalias '%bop 'backward-paragraph)
  (defalias '%eop 'forward-paragraph)
  (defalias '%bov 'back-to-indentation) ;; visible
  (defalias '%eov 'forward-paragraph)

  (defun %eol ()
    (interactive)
    (if visual-line-mode
        (end-of-visual-line)
      (end-of-line)))

  (defun %bol ()
    (interactive)
    (if visual-line-mode
        (beginning-of-visual-line)
      (beginning-of-line)))

  (defun %bw-boW ()
    (interactive)
    (if (string-match whitespace (string (preceding-char)))
        (re-search-backward non-whitespace))
    (re-search-backward whitespace)
    (%right))

  (defun %boW ()
    (interactive)
    (re-search-forward whitespace)
    (if (string-match whitespace (string (following-char)))
        (progn (re-search-forward non-whitespace)
               (%left))))

  (defun %eoW ()
    (interactive)
    (if (string-match whitespace (string (following-char)))
        (re-search-forward non-whitespace))
    (re-search-forward whitespace)
    (%left))

  ;; (defun %deadgrep-app ()
  ;;   (interactive)
  ;;   (let ((default-directory (concat (projectile-project-root) "/app")))
  ;;     (call-interactively '%search-project)))

  ;; (defun %deadgrep-lib ()
  ;;   (interactive)
  ;;   (let (default-directory "lib")
  ;;     (call-interactively '%search-project)))

  (defalias '%search 'isearch-forward-regexp)
  ;; (defalias '%search-project 'deadgrep)
  (defun %search-project ()
    (interactive)
    (if (region-active-p)
        (%adjust-selection-for-edit))
    (setq deadgrep--search-type 'string)
    (call-interactively 'deadgrep))
  ;; (defalias '%search-project-app '%deadgrep-app)
  ;; (defalias '%search-project-lib '%deadgrep-lib)
  ;; (defalias '%search-project-spec '%deadgrep-spec)

  (defun %regex-search-project ()
    (interactive)
    (setq deadgrep--search-type 'regexp)
    (call-interactively 'deadgrep))

  (defun %search-swoop ()
    (interactive)
    ;; (%mark-set ?M)
    (helm-swoop :$query ""))

  ;; (defun %to-search-start ()
  ;;   (%mark-jump ?M))

  (defalias '%exchange-regex 'replace-regexp)
  (defalias '%exchange 'replace-string)

  (defun %query-exchange-regex ()
    (interactive)
    (let ((scroll-conservatively 0))
      (call-interactively 'query-replace-regexp)))

  (defun %query-exchange ()
    (interactive)
    (let ((scroll-conservatively 0))
      (call-interactively 'query-replace)))

  ;; (defun %mark-set (c)
  ;;   (interactive "cset mark: ")
  ;;   (point-to-register (capitalize c)))

  (defalias '%recenter 'recenter-top-bottom)

  (defalias '%bookmark 'point-to-register)
  (defalias '%find-bookmark 'jump-to-register)

  (defun %quick-bookmark ()
    (interactive)
    (%bookmark ?q))

  (defun %quick-find-bookmark ()
    (interactive)
    (%find-bookmark ?q))

  (defalias '%rect-wipe 'kill-rectangle)
  (defalias '%rect-trade 'string-rectangle)
  (defalias '%rect-paste 'yank-rectangle)

  (defun %rect-add ()
    (interactive)
    (let ((pcol (current-column))
          (mcol (save-excursion
                  (%flip-selection)
                  (current-column))))
      (if (= pcol mcol)
          (call-interactively '%rect-trade)
        (message "`=a` should only add text, use `=t` if you want to trade"))))

  (defalias '%downcase 'downcase-word)
  (defalias '%uppercase 'upcase-word)
  (defalias '%capitalize 'capitalize-word)

  (defun %note (s)
    (interactive "snote: ")
    (let* ((org-file "~/org/inbox.org")
           (padded-output (shell-command-to-string (concat "cat " org-file " | wc -l")))
           (output (projectile-trim-string padded-output)))
      (if (string= s "count")
          (message (concat (number-to-string (- (string-to-number output) 1)) " items total"))
        (progn
          (shell-command (concat "echo '** <" (format-time-string "%Y-%m-%d %a") "> '" s " >> " org-file))
          (message (concat "added to inbox.org (" output " items total)"))))))

  (defun %editable ()
    (and (not buffer-read-only)
         (buffer-file-name)))

  (defun %save ()
    (interactive)
    (if (%editable) (save-buffer) (message "not saving...")))

  (defun %full-save ()
    (interactive)
    (%whitespace)
    (%normal))

  (defun %whitespace ()
    (interactive)
    (delete-trailing-whitespace)
    (%save))

  (defun %select ()
    (interactive)
    (setq %selection-line nil)
    (set-marker %mark-anchor (point))
    (set-mark-command nil))

  (defun %select-line ()
    (interactive)
    (%bol)
    (%select)
    (setq %selection-line (line-number-at-pos))
    (%eol)
    (set-marker %mark-anchor-eol (+ 1 (point))))

  (defalias '%deselect 'deactivate-mark)
  (defalias '%reselect 'exchange-point-and-mark)
  (defalias '%flip-selection 'exchange-point-and-mark)
  (defalias '%autocomplete 'hippie-expand)
  (defalias '%help 'help-command)
  (defalias '%help-search 'describe-symbol)
  (defalias '%help-key 'describe-key)
  (defalias '%help-messages 'view-echo-area-messages)
  (defalias '%help-all-keys 'describe-bindings)
  (defalias '%paste-from-history 'helm-show-kill-ring)

  (defalias '%eval-expr 'eval-expression)
  (defalias '%eval-inline 'eval-last-sexp)

  (defalias '%undo 'undo-tree-undo)
  (defalias '%redo 'undo-tree-redo)
  (defalias '%undo-tree 'undo-tree-visualize)

  (setq %mark-anchor (make-marker))
  (setq %mark-anchor-eol (make-marker))

  (defun %adjust-selection (anchor)
    (let* ((mark- (or (mark) 0))
           (diff (- mark- anchor)))
      (if (or (= 0 diff) (= 1 diff))
          (let ((new-mark (if (<= (point) anchor)
                              (1+ anchor)
                            anchor)))
            (set-mark new-mark)))))

  (defun %adjust-line-selection (anchor anchor-eol)
    (if (< (line-number-at-pos) %selection-line) ;; above selection-line
        (progn
          (set-mark anchor-eol)
          (%bol))
      (progn
        (set-mark anchor)
        (%eol))))

  (defun %adjust-selection-hook ()
    (if (region-active-p)
        (let ((anchor (marker-position %mark-anchor))
              (anchor-eol (marker-position %mark-anchor-eol)))
          (if anchor
              (if %selection-line
                  (%adjust-line-selection anchor anchor-eol)
                (%adjust-selection anchor))))))

  (defun %adjust-selection-for-edit ()
    (if (and (> (point) (mark))
             (not (eobp)))
        (%right)))

  (defmacro %defun-add (fn original)
    `(defun ,fn ()
       (interactive)
       (,original)
       (%add)))

  (defmacro %defun-add-fn (fn &rest body)
    `(defun ,fn ()
       (interactive)
       ,@body
       (%add)))

  (put '%defun-add 'lisp-indent-function 'defun)
  (put '%defun-add-fn 'lisp-indent-function 'defun)
  (put '%defun-selection-fn 'lisp-indent-function 'defun)

  (defun %strip-until-command (s)
    (let ((idx (string-match "[twcdaA]" s)))
      (interactive)
      (if idx (substring s idx nil) s)))

  (defun %noop () (interactive) )
  (defun %add () (interactive) (modalka-mode -1))
  (defun %normal () (interactive) (modalka-mode) (%deselect) (%save))
  (defun %normal-without-save () (interactive) (modalka-mode))
  (defun %escape ()
    (interactive)
    ;; (if (not modalka-mode)
    ;;     (if defining-kbd-macro
    ;;         (progn
    ;;           (message "resetting macro")
    ;;           (call-interactively 'kmacro-end-macro)
    ;;           (setq last-kbd-macro (%strip-until-command last-kbd-macro))
    ;;           (setq %repeat-macro nil)
    ;;           (kmacro-name-last-macro '%repeat-macro)
    ;;           )))
    ;; (%start-macro nil)
    (%normal))

  (defun %copy () (interactive) (whole-line-or-region-kill-ring-save 1))
  (defun %wipe () (interactive) (whole-line-or-region-kill-region 1))
  (defun %vanish ()
    (interactive)
    (if (region-active-p)
        (whole-line-or-region-delete 1)
      (delete-char 1)))

  (%defun-add %trade %wipe)

  (defun %become ()
    (interactive)
    (%vanish)
    (%paste-fmt))

  (defun %become-eol ()
    (interactive)
    (%select-eol)
    (%become))

  (defun %dupe ()
    (interactive)
    (%copy)
    (%flip-selection)
    (%paste-fmt)
    (%flip-selection))

  (defmacro %defun-selection-fn (name fn)
    `(defun ,name ()
       (interactive)
       (%adjust-selection-for-edit)
       (,fn)))

  (%defun-selection-fn %vanish-selection %vanish)
  (%defun-selection-fn %dupe-selection %dupe)
  (%defun-selection-fn %trade-selection %trade)
  (%defun-selection-fn %wipe-selection %wipe)
  (%defun-selection-fn %become-selection %become)

  (defmacro %defun-line-fn (name fn)
    `(defun ,name ()
       (interactive)
       (%old-select "ol")
       (,fn)))

  (%defun-line-fn %dupe-line %dupe)
  (%defun-line-fn %wipe-line %wipe)
  (%defun-line-fn %become-line %become)
  ;; (%defun-line-fn %trade-line %trade)

  (defmacro %defun-copy (fn &rest body)
    `(defun ,fn ()
       (interactive)
       (save-excursion
         ,@body
         (%copy))))
  (put '%defun-copy 'lisp-indent-function 'defun)

  (%defun-copy %copy-line
    (%old-select "ol"))

  (%defun-copy %copy-selection
    (%adjust-selection-for-edit))

  (%defun-copy %copy-eol
    (%select-eol))

 (defun %bs ()
    (interactive)
    (delete-char -1)
    (%add))

  (defun %m-bs ()
    (interactive)
    (call-interactively '%bw-wipe-word)
    (%add))

  (defun %del ()
    (interactive)
    (%vanish)
    (%add))

  (defun %m-del ()
    (interactive)
    (call-interactively '%wipe-word)
    (%add))

  (defun %drag ()
    (interactive)
    (%right)
    (transpose-chars 1)
    (%left))

  (defun %bw-drag ()
    (interactive)
    (transpose-chars 1)
    (%left 2))

  (defun %drag-word ()
    (interactive)
    (transpose-words 1))

  (defun %bw-drag-line ()
    ;; neither save-excursion nor setting a marker was working here, probably because point moves
    (interactive)
    (let ((col (current-column)))
      (transpose-lines 1)
      (%up 2)
      (%right col)))

  (defun %drag-line ()
    (interactive)
    (let ((col (current-column)))
      (%down)
      (transpose-lines 1)
      (%up)
      (%right col)
      ))

  (defun %bw-drag-word ()
    (interactive)
    (transpose-words -1)
    )

  (defun %past (target)
    (interactive "cpast: ")
    (search-forward (string target)))

  (defun %bw-past (target)
    (interactive "cbw past: ")
    (search-backward (string target)))

  (defun %until (target)
    (interactive "cuntil: ")
    (%past target)
    (%left))

  (defun %bw-until (target)
    (interactive "cback until: ")
    (%bw-past target)
    (%right))

  (defun %fold ()
    (interactive)
    (%bov)
    (set-selective-display (1+ (current-column))))

  (defun %fold-max ()
    (interactive)
    (%bol)
    (set-selective-display (1+ (current-column))))

  (defun %unfold ()
    (interactive)
    (set-selective-display 0))

  (defun %old-select (target)
    (interactive)
    (cond
     ((string= "iw" target)
      (%right)
      (%bw-bow)
      (%select)
      (%eow))

     ((string= "ow" target)
      (%old-select "iw")
      (%right))

     ((string= "oW" target)
      (re-search-backward whitespace)
      (%right)
      (set-mark-command nil)
      (re-search-forward whitespace))

     ((string= "iW" target)
      (%old-select "oW")
      (%left))

     ((string= "ip" target)
      (mark-paragraph)
      (%right)
      (%flip-selection))

     ((string= "op" target)
      (%old-select "ip")
      (%right))

     ((string= "il" target)
      (%bol)
      (%select)
      (%eol))

     ((string= "ol" target)
      (%old-select "il")
      (%right))))

  (defun %old-select-line ()
    (interactive)
    (%old-select "il"))

  (defun %select-eol ()
    (interactive)
    (%select)
    (%eol)
    (%flip-selection))

  (setq %toggle-comments-state "showing")

  (defun %toggle-comments ()
    (interactive)
    (let ((new-state (if (string= %toggle-comments-state "showing") "hiding" "showing")))
      (setq %toggle-comments-state new-state)
      (call-interactively 'hide/show-comments-toggle)
      (message (format "%s comments..." %toggle-comments-state))))

  (defun %comment ()
    (interactive)
    (whole-line-or-region-comment-dwim nil))

  (defun %comment-par ()
    (interactive)
    (if (region-active-p)
        (%comment)
      (progn
        (%old-select "ip")
        (%left)
        (%comment))))

  (defun %vanish-eol ()
    (interactive)
    (%select-eol)
    (%vanish))

  (defun %dupe-eol ()
    (interactive)
    (%select-eol)
    (%dupe))

  (defun %indent-paragraph ()
    (interactive)
    (%old-select "ip")
    (%indent))

  (defun %wipe-eol ()
    (interactive)
    (if (not (eolp))
        (progn
          (%select-eol)
          (%wipe))))

  (defun %paste-raw ()
    (interactive)
    (if current-prefix-arg (insert " "))
    (yank))

  (defun %paste-fmt ()
    (interactive)
    (%paste-raw)
    (call-interactively 'indent-region))

  (defun %paste-eol ()
    (interactive)
    (%eol)
    (%paste-raw))

  (defun %graft (c)
    (interactive "cgraft: ")
    (%vanish)
    (insert c)
    (%left))

  (defun %cycle-spacing ()
    (interactive)
    (cycle-spacing 1 nil 'fast))

  (setq %repeat-cmd nil)
  (setq %repeat-arg nil)
  (defun %repeat-command ()
    (interactive)
    (if %repeat-cmd
        (if %repeat-arg
            (progn
              (message (concat "repeating: " %repeat-cmd (string %repeat-arg)))
              (call-interactively ((key-binding %repeat-cmd) %repeat-arg)))
          (progn
            (message (concat "repeating: " %repeat-cmd))
            (call-interactively (key-binding %repeat-cmd))))))

  (defun %on-blank-line () (= (line-beginning-position) (line-end-position)))

  (%defun-add %trade-eol %wipe-eol)
  (%defun-add %trade-bol %wipe-bol)
  (%defun-add-fn %trade-line
    (if (not (%on-blank-line))
        (progn
          (%old-select "il")
          (%wipe)))
    (%indent)
    (%add))

  (%defun-add %add-eol %eol)
  (%defun-add %add-bov %bov)

  (defun %newline-here ()
    (interactive)
    (newline-and-indent)
    (%add))

  (defun %newline-above ()
    (interactive)
    (%bol)
    (newline-and-indent)
    (%up)
    (%trade-line))

  (defun %newline-below ()
    (interactive)
    (%eol)
    (newline-and-indent)
    (%add))

  (defun %paste-line ()
    (interactive)
    (%bol)
    (%down)
    (%paste-fmt))

  ;; (defun %pend-all-specs ()
  ;;   (interactive)
  ;;   (%bob)
  ;;   (mapc (lambda (term)
  ;;           (while (search-forward (concat " " term "("))
  ;;             (replace-match (concat " x" term "("))))
  ;;         '("describe" "context" "test" "it")))

  (defun xah-toggle-previous-letter-case ()
    (interactive)
    (let ((case-fold-search nil))
      (cond
       ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
       ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point)))))
      ))

  (defalias '%join-line-above 'delete-indentation)
  (defun %join-line-below ()
    (interactive)
    (%down)
    (%join-line-above))

  (defun %wip-commit ()
    (interactive)
    (shell-command "wip"))

  (defun %pbcopy ()
    (interactive)
    (%adjust-selection-for-edit)
    (shell-command-on-region (region-beginning) (region-end) "pbcopy")
    (deactivate-mark))

  (defun %pbcopy-str (s)
    (interactive)
    (let ((s (format "echo -n \"%s\" | pbcopy" s)))
      (shell-command s)
      (message "s: ")
      (message s)
      ))

  (defun %pbcopy-ruby ()
    (interactive)
    (let* ((s (buffer-substring (region-beginning) (region-end)))
           (fix-blocks (replace-regexp-in-string (regexp-quote "|
") "| " s))
           (fix-newlines (replace-regexp-in-string (regexp-quote "
") "; " fix-blocks))
           (fix-quotes (replace-regexp-in-string (regexp-quote "'") "\"" fix-newlines))
           (cmd (format "echo -n '%s' | pbcopy" fix-quotes))
           )
      (async-shell-command cmd)
      (message "copied to ruby line")
      (deactivate-mark)))

  (defun %copy-gh-line-number ()
    (interactive)
    (%pbcopy-str (%appserver-github-url (format-mode-line "%l"))))

  ;; https://github.com/BLC/admin-ui/blob/master/cypress/integration/investor-servicing/people/view-people.spec.js
  ;;                                             cypress/integration/investor-servicing/people/view-people.spec.js
  (defun nc/url-with-lines (dir-name repo-name &optional start-line end-line)
    (interactive)
    (let ((url (replace-regexp-in-string (concat "/Users/trevorb/code/" dir-name)
                                         (concat "https://github.com/BLC/" repo-name "/blob/master")
                                         (buffer-file-name)))
          (start-line-suffix (concat "#L" start-line))
          (end-line-suffix (concat "-L" end-line))
          )
      (message "replacing:")
      (message (concat "/Users/trevorb/code/" dir-name))
      (message "with:")
      (message (concat "https://github.com/BLC/" repo-name "/blob/master"))
      (message "in:")
      (message url)
      (if (null start-line)
          url
        (if (null end-line)
            (concat url start-line-suffix)
          (concat url start-line-suffix end-line-suffix)))))

  (defun nc/github-url (&optional start-line end-line)
    (interactive)
    (let* ((dir-name (file-name-nondirectory (buffer-file-name)))
           (repo-name (cond ((string= "appserver" dir-name) "rails")
                            ((string= "ui" dir-name) "nextcapital-ui")
                            (:else dir-name)
                            )))
      (nc/url-with-lines dir-name repo-name start-line end-line)))

  (defun nc/open-in-github ()
    (interactive)
    (message "got this url: ")
    (message (nc/github-url (format-mode-line "%l")))
    (browse-url (nc/github-url (format-mode-line "%l"))))

  (defun %show-filepaths ()
    (interactive)
    (let ((buffer-path-from-root
           (replace-regexp-in-string (regexp-quote (projectile-project-root)) "" (buffer-file-name) nil 'literal)))
      (message (concat "rel: " (%filepath-from-root) "\nabs: " (buffer-file-name)))))

  (defun %filepath-from-root ()
    (replace-regexp-in-string (regexp-quote (projectile-project-root)) "" (buffer-file-name) nil 'literal))

  (defun %cp-filepath ()
    (interactive)
    (%pbcopy-str (%filepath-from-root)))

  (defun %test-file ()
    (interactive)
    (%pbcopy-str (concat "s test " (%filepath-from-root))))

    ;;  (%appserver-github-url (format-mode-line "%l"))))
    ;; (let ((buffer-path-from-root
    ;;        ))
    ;;   (message (concat "rel: " buffer-path-from-root "\nabs: " (buffer-file-name)))))

  (defun increment-next-number (&optional arg)
    "Increment the number forward from point by 'arg'."
    (interactive "p*")
    (save-excursion
      (save-match-data
        (let (inc-by field-width answer)
          (setq inc-by (if arg arg 1))
          (skip-chars-backward "0123456789")
          (when (re-search-forward "[0-9]+" nil t)
            (setq field-width (- (match-end 0) (match-beginning 0)))
            (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
            (when (< answer 0)
              (setq answer (+ (expt 10 field-width) answer)))
            (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                   answer)))))))

  (defun decrement-next-number (&optional arg)
    (interactive "p*")
    (increment-next-number (if arg (- arg) -1)))

  ;; directional keys
  (defun %define-target-key (i-or-o input-key action target-key)
    (let* ((target (concat i-or-o target-key))
           (key-seq (concat input-key target)))
      (define-key modalka-mode-map key-seq (lambda () (interactive)
                                             (setq %repeat-cmd key-seq)
                                             (setq %repeat-arg nil)
                                             (save-excursion
                                               (%old-select target)
                                               (funcall action))))))

  (defun %define-target-keys (i-or-o key action targets)
    (mapcar (apply-partially '%define-target-key i-or-o key action) targets))

  (defun %select-inner-outer (i-or-o target-key selection-start-char)
    (cond
     ((string= target-key "w")
      (%old-select (concat i-or-o "w")))

     ((string= target-key "W")
      (%old-select (concat i-or-o "W")))

     ((string= target-key "p")
      (%old-select (concat i-or-o "p")))

     ((string= i-or-o "i")
      (search-backward selection-start-char)
      (er/expand-region 1)
      (%right)
      (%flip-selection)
      (%left)
      (%flip-selection))

     ((string= i-or-o "o")
      (search-backward selection-start-char)
      (er/expand-region 1))))

  (defun %define-delimited-key (input-key i-or-o selection-start-char action target-key)
    (let* ((key-seq (concat input-key i-or-o target-key))
           (run-cmd
            (lambda () (interactive)
              (%select-inner-outer i-or-o target-key selection-start-char)
              (funcall action)))
           (fn (lambda ()
                 (interactive)
                 (setq %repeat-cmd key-seq)
                 (setq %repeat-arg nil)
                 (cond
                  ((string= input-key "c") (save-excursion (funcall run-cmd)))
                  (t (funcall run-cmd))))))
      (define-key modalka-mode-map key-seq fn)))

  (defun %define-until-key (cmd-key key action search)
    (let ((key-seq (concat cmd-key key)))
      (define-key modalka-mode-map key-seq (lambda (target)
                                             (interactive "c")
                                             (setq %repeat-cmd key-seq)
                                             (setq %repeat-arg (string target))
                                             (save-excursion
                                               (set-mark-command nil)
                                               (funcall search target)
                                               (funcall action))))))

  (defun %define-until-keys (key action)
    (%define-until-key key "u" action '%until)
    (%define-until-key key "l" action '%bw-until)
    (%define-until-key key "y" action '%past)
    )

  (defun %chars (string)
    (split-string string "" 'f))

  (defun %define-delimited-targets (i-or-o key action pair)
    (mapcar (apply-partially '%define-delimited-key key i-or-o (cdr pair) action) (%chars (car pair))))

  (defun %define-prefix-action (key action)
    (let ((delimited-targets '(("b()" . "(") ("B{}" . "{") ("[]" . "[") ("S'" . "'") ("s\"" . "\"")))
          (targets (%chars "lpwW")))
      (%define-until-keys key action)
      (%define-target-keys "i" key action targets)
      (%define-target-keys "o" key action targets)
      (mapc (apply-partially '%define-delimited-targets "i" key action) delimited-targets)
      (mapc (apply-partially '%define-delimited-targets "o" key action) delimited-targets)))

  ;; remapped in iterm
  ;; ("actual key pressed" . "what emacs sees")
  ;; ("C-i" . "M-!")
  ;; ("S-RET" . "M-@")
  ;; ("C-:" . "M-#")
  ;; ("<caps lock>" . "<f8>") ;; karabiner
  ;; ("<f8>" . "M-$") ;; iterm
  ;; ("C-'" . "M-%")
  ;; ("" . "M-^")
  ;; ("M-[" . "M-&")
  ;; ("C-," . "M-*")
  ;; ("C-TAB" . "M-+")
  ;; ("" . "M-_")
  ;; ("" . "M-=")
  ;; ("M-C-c" . "C-c")
  ;; ("C-M-TAB" . "M-<")
  ;; ("C-/" . "M-?")
  ;; ("M-<deletechar>" . "M-d")

  :bind
  (:map my-keys-minor-mode-map
        ("M-$" . '%normal) ("<f8>" . '%full-save)
        ;; ("M-SPC" . '%m-x-menu)
        ("M-+" . '%autocomplete)
        ("M-# M-#" . '%select)

        ;; ("C-M-E" . '%uppercase)
        ;; ("C-M-N" . '%downcase)
        ;; ("C-M-I" . '%capitalize)
        ;; ("C-M-O" . '%capitalize)

        ;; ("M-RET" . '%newline-above) fixme: work with org
        ("M-@" . '%newline-below)

        ("C-x z" . '%noop)

        ("M-(" . 'winner-undo)
        ("M-)" . 'winner-redo)

        ("M-&" . '%fold) ("M-]" . '%unfold)
        ("M-{" . '%search-project) ("M-}" . '%regex-search-project)
        ;; ("M-;" . '%noop)

        ("M-Q" . '%noop)
        ("M-q M-DEL RET" . 'save-buffers-kill-terminal)
        ("M-q '" . '%go-to-buffer)
        ("M-q o" . '%open-file)
        ("M-q DEL" . '%quit-window)
        ("M-q M-m" . '%maximize)
        ("M-q SPC" . '%dired)
        ;; ("qp" . '%quit-popups)

        ("M-q H" . '%split-left)
        ("M-q N" . '%split-down)
        ("M-q E" . '%split-up)
        ("M-q I" . '%split-right)

        ("M-q M-h" . '%switch-window-left)
        ("M-q M-n" . '%switch-window-down)
        ("M-q M-e" . '%switch-window-up)
        ("M-q M-i" . '%switch-window-right)

        ("M-w" . '%wipe-line) ("M-W" . '%noop) ("C-w" . '%wipe-eol)
        ("M-b" . '%become-line) ("M-B" . '%noop) ("C-b" . '%become-eol)
        ("M-p" . '%paste-fmt) ("M-P" . '%paste-raw) ("C-p" . '%paste-eol)
        ("M-f" . '%flip-flop) ("M-F" . '%noop) ("C-f" . '%flip-flop)

        ("M-a" . '%add) ("M-A" . '%add-bov) ("C-a" . '%add-eol)
        ("M-r" . '%repeat-command) ("M-R" . '%noop) ("C-r" . '%repeat-command)
        ("M-s" . '%select) ("M-S" . '%select-line) ("C-s" . '%select-eol)
        ("M-t" . '%trade-line) ("M-T" . '%noop) ("C-t" . '%trade-eol)
        ;; ("M-g" . '%graft) ("M-G" . '%noop)

        ("M-/" . '%help) ("M-?" . '%help)
        ("M-," . '%m-x-menu) ("M-*" . '%m-x-menu)
        ;; ("M-x" . '%exchange) ("M-X" . '%exchange-exact)
        ("M-c" . '%copy-line) ("M-C" . '%copy-line)
        ("M-d" . '%m-del) ("M-D" . '%dupe-line) ("C-d" . '%dupe-eol)
        ("M-v" . '%vanish) ("M-V" . '%noop) ("C-v" . '%vanish)

        ("M-;" .'%comment)
        ("M-|" . '%comment-par)
        ("M-\\" .'%toggle-comments)

        ("M-j" . '%join-line-below) ("M-J" . '%join-line-above)

        ("M-`" . 'pop-to-mark-command) ("M-~" . 'helm-all-mark-rings)

        ("M-z" . '%bol)
        ("M-l" . '%bw-bow) ("C-l" . '%recenter)
        ("M-u" . '%undo) ("M-U" . '%undo-tree) ("C-u" . '%undo)
        ("M-y" . '%bow) ("C-y" . '%noop)
        ("M-:" . '%redo)

        ("M-h" . '%left) ("C-h" . '%bw-drag)
        ("M-n" . '%down) ("C-n" . '%drag-line)
        ("M-e" . '%up) ("C-e" . '%bw-drag-line)
        ("M-i" . '%right) ("M-!" . '%drag)
        ("M-o" . '%eow) ("C-o" . 'universal-argument)
        ("M-'" . '%search) ("M-\"" . '%search)

        ("M-k" . '%bov) ("M-K" . '%noop)
        ("M-m" . '%bop) ("M-M" . '%bob)
        ("M-DEL" . '%m-bs)
        ("M-." . '%eop) ("M->" . '%eob)
        ("M--" . '%eol) ("C-_" . '%eol)

        ;; :map minibuffer-local-completion-map
        ;; ("C-o" . 'minibuffer-complete)

        :map isearch-mode-map
        ("M-e" . 'isearch-repeat-backward)
        ("M-n" . 'isearch-repeat-forward)
        ("M-y" . 'isearch-yank-word-or-char)
        ("M-o" . 'isearch-yank-word-or-char)
        ("M-s" . 'helm-swoop-from-isearch)
        ("C-r" . '%noop)
        ("C-s" . '%noop)

        :map modalka-mode-map
        ("RET" . '%newline-here)

        ("!" . '%noop) ("@" . '%noop) ("#" . '%noop) ("$" . '%noop) ("%" . '%noop) ("^" . '%noop) ("&" . '%to-line) ("*" . '%noop)
        ("1" . '%noop) ("2" . '%noop) ("3" . '%noop) ("4" . '%noop) ("5" . '%noop) ("6" . '%noop) ("7" . '%noop) ("8" . '%noop) ("9" . '%noop) ("0" . '%to-line)

        ("[" . '%fold) ("]" . '%unfold)
        ("{" . '%search-project) ("}" . '%regex-search-project)
        (";" . '%noop)

        ("q M-DEL RET" . 'save-buffers-kill-terminal)

        ("q'" . '%go-to-buffer)
        ("qo" . '%open-file)
        ("q DEL" . '%quit-window)
        ("qm" . '%maximize)
        ("q)" . 'dumb-jump-go)
        ("q(" . 'dumb-jump-back)
        ("q M-)" . 'dumb-jump-go-other-window)
        ;; ("q M-(" . 'dumb-jump-go-prompt)
        ;; ("qp" . '%quit-popups)

        ("qH" . '%split-left)
        ("qN" . '%split-down)
        ("qE" . '%split-up)
        ("qI" . '%split-right)

        ("qh" . '%switch-window-left)
        ("qn" . '%switch-window-down)
        ("qe" . '%switch-window-up)
        ("qi" . '%switch-window-right)

        ("q SPC d" . '%dired)
        ("q SPC s" . '%show-filepaths)
        ("q SPC c" . '%cp-filepath)
        ("q SPC t" . '%test-file)

        ("W" . '%noop)
        ("B" . '%noop)
        ("p" . '%paste-fmt) ("P" . '%paste-raw)
        ("f" . '%flip-flop) ("F" . '%noop)

        ("a" . '%add) ("A" . '%add-bov)
        ("r" . '%repeat-command) ("R" . '%noop)
        ("s" . '%select) ("S" . '%select-line)
        ("T" . '%noop)
        ("g" . '%graft) ("G" . 'xah-toggle-previous-letter-case)

        ("?" . '%help)

        ;; help
        ("/ SPC" . 'help-for-help)
        ("/m" . '%help-messages)
        ("/k" . '%help-key)
        ("//k" . '%help-all-keys)
        ("/'" . '%help-search)

        ;; modifier
        ("SPC u" . '%undo-tree)
        ("SPC p" . '%paste-from-history)
        ("SPC c" . '%pbcopy)
        ("SPC r" . '%pbcopy-ruby)
        ("SPC s" . '%reselect)
        ("SPC x" . '%exchange-regex)
        ("SPC X" . '%query-exchange-regex)

        ;; command
        ("SPC b" . '%quick-bookmark)
        ("SPC f" . '%quick-find-bookmark)
        ("SPC B" . '%bookmark)
        ("SPC F" . '%find-bookmark)
        ("SPC g" . '%git)
        ("SPC G" . 'nc/open-in-github)
        ("SPC n" . '%note)
        ("SPC m" . '%start-macro)
        ("SPC M" . '%end-macro)
        ("SPC RET" . '%run-macro)
        ("SPC (" . '%eval-expr) ("SPC )" . '%eval-inline)

        ("," . '%m-x-menu) ("<" . '%noop)
        ("x" . '%exchange) ("X" . '%query-exchange)
        ("C" . '%copy-line)
        ("D" . '%dupe-line)
        ("v" . '%vanish) ("V" . '%noop)

        (";" .'%comment)
        ("|" . '%comment-par)
        ("\\" .'%toggle-comments)

        ("=a" . '%rect-add)
        ("=t" . '%rect-trade)
        ("=w" . '%rect-wipe)
        ("=p" . '%rect-paste)
        ("j" . '%join-line-below) ("J" . '%join-line-above)

        ("`" . 'pop-global-mark) ("~" . 'helm-all-mark-rings)
        ("(" . 'pop-to-mark-command) (")" . '%cycle-spacing)

        ("z" . '%bol) ("Z" . '%noop)
        ("l" . '%bw-bow) ("L" . '%bw-boW)
        ("u" . '%undo) ("U" . '%undo-tree)
        ("y" . '%bow) ("Y" . '%boW)
        (":" . '%redo)

        ("<deletechar>" . '%del)

        ("h" . '%left) ("H" . '%noop)
        ("n" . '%down) ("N" . '%noop)
        ("e" . '%up) ("E" . '%noop)
        ("i" . '%right) ("I" . '%noop)
        ("o" . '%eow) ("O" . '%eoW)
        ("'" . '%search) ("\"" . '%search)

        ("k" . '%bov) ("K" . '%noop)
        ("m" . '%bop) ("M" . '%bob)
        ("DEL" . '%bs)
        ("." . '%eop) (">" . '%eob)
        ("-" . '%eol) ("_" . '%eol)

        )

  :config
  (add-hook 'text-mode-hook #'modalka-mode)
  (add-hook 'prog-mode-hook #'modalka-mode)

  (defun %is-special-buffer ()
    (let* ((first-char-idx (string-match non-whitespace (buffer-name)))
           (first-char (substring (buffer-name) first-char-idx (+ 1 first-char-idx))))
      (string= "*" first-char)
      ))

  (defun %set-mode-colors ()
    ;; (rainbow-mode)
    (let* ((colors (cond (modalka-mode         '("#bce" "#89a"))
                         ;; ((%is-special-buffer) '("#000" "#032" "#044"))
                         (t                    '("#c88" "#a89"))))
           (mode-line-active-bg (car colors))
           (linum-fg (cadr colors))
           )
      (set-face-background 'mode-line mode-line-active-bg)
      (if (featurep 'linum) (set-face-foreground 'linum linum-fg))
      ))

  (define-key global-map (kbd "C-M-x") ctl-x-map)

  (add-hook 'post-command-hook '%adjust-selection-hook)
  (add-hook 'modalka-mode-hook '%set-mode-colors)
  ;; (add-hook 'minibuffer-setup-hook '%set-mode-colors)
  ;; (add-hook 'minibuffer-exit-hook '%set-mode-colors)
  ;; (add-hook 'minibuffer-inactive-mode-hook '%set-mode-colors)
  (selected-minor-mode)
  )
