;; -*- lexical-binding: t; -*-

(use-package selected
  :ensure t
  :commands selected-minor-mode

  :bind
  (:map selected-keymap
        ("w" . bt/wipe-selection)
        ("b" . bt/become-selection)
        ("t" . bt/trade-selection)
        ("c" . bt/copy-selection)
        ("d" . bt/dupe-selection)
        ("v" . bt/vanish-selection)
        )

  :config
  (bt/define-prefix-action "w" 'bt/wipe)
  (bt/define-prefix-action "b" 'bt/become)
  (bt/define-prefix-action "t" 'bt/trade)
  (bt/define-prefix-action "c" 'bt/copy)
  (bt/define-prefix-action "d" 'bt/dupe)
  ;; (bt/define-prefix-action " " 'bt/noop) ;; selection stays activated so this is like "select"
  )

(use-package modalka
  :init
  (defconst whitespace "[[:space:]]\\|\n")
  (defconst non-whitespace "[[:graph:]]")
  (defalias 'bt/left 'backward-char)
  (defalias 'bt/down 'next-line)
  (defalias 'bt/up 'previous-line)
  (defalias 'bt/right 'forward-char)

  (defun bt/repeat ()
    (interactive)
    (if bt/repeat-macro (call-interactively 'bt/repeat-macro))
    (bt/normal))

  (defalias 'bt/m-x-menu 'helm-M-x)

  ;; emacs uses "up"/"down" to refer to the text in the buffer moving,
  ;; bt/ uses direction that the cursor moves
  (defalias 'bt/scroll-up 'scroll-down-line)
  (defalias 'bt/scroll-down 'scroll-up-line)
  (defalias 'bt/scroll-up-page 'scroll-down)
  (defalias 'bt/scroll-down-page 'scroll-up)

  (defalias 'bt/switch-window-left 'windmove-left)
  (defalias 'bt/switch-window-down 'windmove-down)
  (defalias 'bt/switch-window-up 'windmove-up)
  (defalias 'bt/switch-window-right 'windmove-right)

  (defalias 'bt/split-left 'split-window-right)
  (defalias 'bt/split-up 'split-window-below)

  (defun bt/split-right ()
    (interactive)
    (bt/split-left)
    (bt/switch-window-right))

  (defun bt/split-down ()
    (interactive)
    (bt/split-up)
    (bt/switch-window-down))

  (defalias 'bt/open-file 'helm-projectile-find-file)
  (defalias 'bt/buffer-list 'ibuffer)
  (defalias 'bt/go-to-buffer 'switch-to-buffer)
  (defalias 'bt/dired 'dired-jump)
  ;; (defalias 'bt/locate-file 'find-file)

  (defalias 'bt/maximize 'delete-other-windows)
  (defalias 'bt/quit-window 'delete-window)
  (defalias 'bt/window-undo 'winner-undo)
  (defalias 'bt/window-redo 'winner-redo)

  (defun bt/quit-popups ()
    (interactive)
    (dolist (buf (buffer-list))
      (when (and (get-buffer-window buf 'visible)
                 (string-match "^\\*" (buffer-name buf)))
        (kill-buffer buf))))

  (defun bt/checkout-buffer ()
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

  (defalias 'bt/start-macro 'kmacro-start-macro)
  (defalias 'bt/end-macro 'kmacro-end-macro)
  (defalias 'bt/run-macro 'kmacro-end-and-call-macro)

  (defun bt/wipe-bol () (interactive) (kill-line 0))
  (defalias 'bt/bw-wipe-word 'backward-kill-word)
  (defalias 'bt/wipe-word 'kill-word)
  ;; (defalias 'bt/wipe-eol 'kill-line)
  (defalias 'bt/indent 'indent-for-tab-command)
  (defalias 'bt/to-line 'goto-line)

  (defalias 'bt/bw-bow 'backward-word)
  (defalias 'bt/bow 'forward-to-word)
  (defalias 'bt/eow 'forward-word)
  (defalias 'bt/bob 'beginning-of-buffer)
  (defalias 'bt/eob 'end-of-buffer)
  (defalias 'bt/bop 'backward-paragraph)
  (defalias 'bt/eop 'forward-paragraph)
  (defalias 'bt/bov 'back-to-indentation) ;; visible
  (defalias 'bt/eov 'forward-paragraph)

  (defun bt/eol ()
    (interactive)
    (if visual-line-mode
        (end-of-visual-line)
      (end-of-line)))

  (defun bt/bol ()
    (interactive)
    (if visual-line-mode
        (beginning-of-visual-line)
      (beginning-of-line)))

  (defun bt/bw-boW ()
    (interactive)
    (if (string-match whitespace (string (preceding-char)))
        (re-search-backward non-whitespace))
    (re-search-backward whitespace)
    (bt/right))

  (defun bt/boW ()
    (interactive)
    (re-search-forward whitespace)
    (if (string-match whitespace (string (following-char)))
        (progn (re-search-forward non-whitespace)
               (bt/left))))

  (defun bt/eoW ()
    (interactive)
    (if (string-match whitespace (string (following-char)))
        (re-search-forward non-whitespace))
    (re-search-forward whitespace)
    (bt/left))

  ;; (defun bt/deadgrep-app ()
  ;;   (interactive)
  ;;   (let ((default-directory (concat (projectile-project-root) "/app")))
  ;;     (call-interactively 'bt/search-project)))

  ;; (defun bt/deadgrep-lib ()
  ;;   (interactive)
  ;;   (let (default-directory "lib")
  ;;     (call-interactively 'bt/search-project)))

  (defalias 'bt/search 'isearch-forward-regexp)
  ;; (defalias 'bt/search-project 'deadgrep)
  (defun bt/search-project ()
    (interactive)
    (if (region-active-p)
        (bt/adjust-selection-for-edit))
    (setq deadgrep--search-type 'string)
    (call-interactively 'deadgrep))
  ;; (defalias 'bt/search-project-app 'bt/deadgrep-app)
  ;; (defalias 'bt/search-project-lib 'bt/deadgrep-lib)
  ;; (defalias 'bt/search-project-spec 'bt/deadgrep-spec)

  (defun bt/regex-search-project ()
    (interactive)
    (setq deadgrep--search-type 'regexp)
    (call-interactively 'deadgrep))

  (defun bt/search-swoop ()
    (interactive)
    ;; (bt/mark-set ?M)
    (helm-swoop :$query ""))

  ;; (defun bt/to-search-start ()
  ;;   (bt/mark-jump ?M))

  (defalias 'bt/exchange-regex 'replace-regexp)
  (defalias 'bt/exchange 'replace-string)

  (defun bt/query-exchange-regex ()
    (interactive)
    (let ((scroll-conservatively 0))
      (call-interactively 'query-replace-regexp)))

  (defun bt/query-exchange ()
    (interactive)
    (let ((scroll-conservatively 0))
      (call-interactively 'query-replace)))

  ;; (defun bt/mark-set (c)
  ;;   (interactive "cset mark: ")
  ;;   (point-to-register (capitalize c)))

  (defalias 'bt/recenter 'recenter-top-bottom)

  (defalias 'bt/bookmark 'point-to-register)
  (defalias 'bt/find-bookmark 'jump-to-register)

  (defun bt/quick-bookmark ()
    (interactive)
    (bt/bookmark ?q))

  (defun bt/quick-find-bookmark ()
    (interactive)
    (bt/find-bookmark ?q))

  (defalias 'bt/rect-wipe 'kill-rectangle)
  (defalias 'bt/rect-trade 'string-rectangle)
  (defalias 'bt/rect-paste 'yank-rectangle)

  (defun bt/rect-add ()
    (interactive)
    (let ((pcol (current-column))
          (mcol (save-excursion
                  (bt/flip-selection)
                  (current-column))))
      (if (= pcol mcol)
          (call-interactively 'bt/rect-trade)
        (message "`=a` should only add text, use `=t` if you want to trade"))))

  (defalias 'bt/downcase 'downcase-word)
  (defalias 'bt/uppercase 'upcase-word)
  (defalias 'bt/capitalize 'capitalize-word)

  (defun bt/note (s)
    (interactive "snote: ")
    (let* ((org-file "~/org/inbox.org")
           (padded-output (shell-command-to-string (concat "cat " org-file " | wc -l")))
           (output (projectile-trim-string padded-output)))
      (if (string= s "count")
          (message (concat (number-to-string (- (string-to-number output) 1)) " items total"))
        (progn
          (shell-command (concat "echo '** <" (format-time-string "%Y-%m-%d %a") "> '" s " >> " org-file))
          (message (concat "added to inbox.org (" output " items total)"))))))

  (defun bt/editable ()
    (and (not buffer-read-only)
         (buffer-file-name)))

  (defun bt/save ()
    (interactive)
    (if (bt/editable) (save-buffer) (message "not saving...")))

  (defun bt/full-save ()
    (interactive)
    (bt/whitespace)
    (bt/normal))

  (defun bt/whitespace ()
    (interactive)
    (delete-trailing-whitespace)
    (bt/save))

  (defun bt/select ()
    (interactive)
    (setq bt/selection-line nil)
    (set-marker bt/mark-anchor (point))
    (set-mark-command nil))

  (defun bt/select-line ()
    (interactive)
    (bt/bol)
    (bt/select)
    (setq bt/selection-line (line-number-at-pos))
    (bt/eol)
    (set-marker bt/mark-anchor-eol (+ 1 (point))))

  (defalias 'bt/deselect 'deactivate-mark)
  (defalias 'bt/reselect 'exchange-point-and-mark)
  (defalias 'bt/flip-selection 'exchange-point-and-mark)
  (defalias 'bt/autocomplete 'hippie-expand)
  (defalias 'bt/help 'help-command)
  (defalias 'bt/help-search 'describe-symbol)
  (defalias 'bt/help-key 'describe-key)
  (defalias 'bt/help-messages 'view-echo-area-messages)
  (defalias 'bt/help-all-keys 'describe-bindings)
  (defalias 'bt/paste-from-history 'helm-show-kill-ring)

  (defalias 'bt/eval-expr 'eval-expression)
  (defalias 'bt/eval-inline 'eval-last-sexp)

  (defalias 'bt/undo 'undo-tree-undo)
  (defalias 'bt/redo 'undo-tree-redo)
  (defalias 'bt/undo-tree 'undo-tree-visualize)

  (setq bt/mark-anchor (make-marker))
  (setq bt/mark-anchor-eol (make-marker))

  (defun bt/adjust-selection (anchor)
    (let* ((mark- (or (mark) 0))
           (diff (- mark- anchor)))
      (if (or (= 0 diff) (= 1 diff))
          (let ((new-mark (if (<= (point) anchor)
                              (1+ anchor)
                            anchor)))
            (set-mark new-mark)))))

  (defun bt/adjust-line-selection (anchor anchor-eol)
    (if (< (line-number-at-pos) bt/selection-line) ;; above selection-line
        (progn
          (set-mark anchor-eol)
          (bt/bol))
      (progn
        (set-mark anchor)
        (bt/eol))))

  (defun bt/adjust-selection-hook ()
    (if (region-active-p)
        (let ((anchor (marker-position bt/mark-anchor))
              (anchor-eol (marker-position bt/mark-anchor-eol)))
          (if anchor
              (if bt/selection-line
                  (bt/adjust-line-selection anchor anchor-eol)
                (bt/adjust-selection anchor))))))

  (defun bt/adjust-selection-for-edit ()
    (if (and (> (point) (mark))
             (not (eobp)))
        (bt/right)))

  (defmacro bt/defun-add (fn original)
    `(defun ,fn ()
       (interactive)
       (,original)
       (bt/add)))

  (defmacro bt/defun-add-fn (fn &rest body)
    `(defun ,fn ()
       (interactive)
       ,@body
       (bt/add)))

  (put 'bt/defun-add 'lisp-indent-function 'defun)
  (put 'bt/defun-add-fn 'lisp-indent-function 'defun)
  (put 'bt/defun-selection-fn 'lisp-indent-function 'defun)

  (defun bt/strip-until-command (s)
    (let ((idx (string-match "[twcdaA]" s)))
      (interactive)
      (if idx (substring s idx nil) s)))

  (defun bt/noop () (interactive) )
  (defun bt/add () (interactive) (modalka-mode -1))
  (defun bt/normal () (interactive) (modalka-mode) (bt/deselect) (bt/save))
  (defun bt/normal-without-save () (interactive) (modalka-mode))
  (defun bt/escape ()
    (interactive)
    ;; (if (not modalka-mode)
    ;;     (if defining-kbd-macro
    ;;         (progn
    ;;           (message "resetting macro")
    ;;           (call-interactively 'kmacro-end-macro)
    ;;           (setq last-kbd-macro (bt/strip-until-command last-kbd-macro))
    ;;           (setq bt/repeat-macro nil)
    ;;           (kmacro-name-last-macro 'bt/repeat-macro)
    ;;           )))
    ;; (bt/start-macro nil)
    (bt/normal))

  (defun bt/copy () (interactive) (whole-line-or-region-kill-ring-save 1))
  (defun bt/wipe () (interactive) (whole-line-or-region-kill-region 1))
  (defun bt/vanish ()
    (interactive)
    (if (region-active-p)
        (whole-line-or-region-delete 1)
      (delete-char 1)))

  (bt/defun-add bt/trade bt/wipe)

  (defun bt/become ()
    (interactive)
    (bt/vanish)
    (bt/paste-fmt))

  (defun bt/become-eol ()
    (interactive)
    (bt/select-eol)
    (bt/become))

  (defun bt/dupe ()
    (interactive)
    (bt/copy)
    (bt/flip-selection)
    (bt/paste-fmt)
    (bt/flip-selection))

  (defmacro bt/defun-selection-fn (name fn)
    `(defun ,name ()
       (interactive)
       (bt/adjust-selection-for-edit)
       (,fn)))

  (bt/defun-selection-fn bt/vanish-selection bt/vanish)
  (bt/defun-selection-fn bt/dupe-selection bt/dupe)
  (bt/defun-selection-fn bt/trade-selection bt/trade)
  (bt/defun-selection-fn bt/wipe-selection bt/wipe)
  (bt/defun-selection-fn bt/become-selection bt/become)

  (defmacro bt/defun-line-fn (name fn)
    `(defun ,name ()
       (interactive)
       (bt/old-select "ol")
       (,fn)))

  (bt/defun-line-fn bt/dupe-line bt/dupe)
  (bt/defun-line-fn bt/wipe-line bt/wipe)
  (bt/defun-line-fn bt/become-line bt/become)
  ;; (bt/defun-line-fn bt/trade-line bt/trade)

  (defmacro bt/defun-copy (fn &rest body)
    `(defun ,fn ()
       (interactive)
       (save-excursion
         ,@body
         (bt/copy))))
  (put 'bt/defun-copy 'lisp-indent-function 'defun)

  (bt/defun-copy bt/copy-line
    (bt/old-select "ol"))

  (bt/defun-copy bt/copy-selection
    (bt/adjust-selection-for-edit))

  (bt/defun-copy bt/copy-eol
    (bt/select-eol))

 (defun bt/bs ()
    (interactive)
    (delete-char -1)
    (bt/add))

  (defun bt/m-bs ()
    (interactive)
    (call-interactively 'bt/bw-wipe-word)
    (bt/add))

  (defun bt/del ()
    (interactive)
    (bt/vanish)
    (bt/add))

  (defun bt/m-del ()
    (interactive)
    (call-interactively 'bt/wipe-word)
    (bt/add))

  (defun bt/drag ()
    (interactive)
    (bt/right)
    (transpose-chars 1)
    (bt/left))

  (defun bt/bw-drag ()
    (interactive)
    (transpose-chars 1)
    (bt/left 2))

  (defun bt/drag-word ()
    (interactive)
    (transpose-words 1))

  (defun bt/bw-drag-line ()
    ;; neither save-excursion nor setting a marker was working here, probably because point moves
    (interactive)
    (let ((col (current-column)))
      (transpose-lines 1)
      (bt/up 2)
      (bt/right col)))

  (defun bt/drag-line ()
    (interactive)
    (let ((col (current-column)))
      (bt/down)
      (transpose-lines 1)
      (bt/up)
      (bt/right col)
      ))

  (defun bt/bw-drag-word ()
    (interactive)
    (transpose-words -1)
    )

  (defun bt/past (target)
    (interactive "cpast: ")
    (search-forward (string target)))

  (defun bt/bw-past (target)
    (interactive "cbw past: ")
    (search-backward (string target)))

  (defun bt/until (target)
    (interactive "cuntil: ")
    (bt/past target)
    (bt/left))

  (defun bt/bw-until (target)
    (interactive "cback until: ")
    (bt/bw-past target)
    (bt/right))

  (defun bt/fold ()
    (interactive)
    (bt/bov)
    (set-selective-display (1+ (current-column))))

  (defun bt/fold-max ()
    (interactive)
    (bt/bol)
    (set-selective-display (1+ (current-column))))

  (defun bt/unfold ()
    (interactive)
    (set-selective-display 0))

  (defun bt/old-select (target)
    (interactive)
    (cond
     ((string= "iw" target)
      (bt/right)
      (bt/bw-bow)
      (bt/select)
      (bt/eow))

     ((string= "ow" target)
      (bt/old-select "iw")
      (bt/right))

     ((string= "oW" target)
      (re-search-backward whitespace)
      (bt/right)
      (set-mark-command nil)
      (re-search-forward whitespace))

     ((string= "iW" target)
      (bt/old-select "oW")
      (bt/left))

     ((string= "ip" target)
      (mark-paragraph)
      (bt/right)
      (bt/flip-selection))

     ((string= "op" target)
      (bt/old-select "ip")
      (bt/right))

     ((string= "il" target)
      (bt/bol)
      (bt/select)
      (bt/eol))

     ((string= "ol" target)
      (bt/old-select "il")
      (bt/right))))

  (defun bt/old-select-line ()
    (interactive)
    (bt/old-select "il"))

  (defun bt/select-eol ()
    (interactive)
    (bt/select)
    (bt/eol)
    (bt/flip-selection))

  (setq bt/toggle-comments-state "showing")

  (defun bt/toggle-comments ()
    (interactive)
    (let ((new-state (if (string= bt/toggle-comments-state "showing") "hiding" "showing")))
      (setq bt/toggle-comments-state new-state)
      (call-interactively 'hide/show-comments-toggle)
      (message (format "%s comments..." bt/toggle-comments-state))))

  (defun bt/comment ()
    (interactive)
    (whole-line-or-region-comment-dwim nil))

  (defun bt/comment-par ()
    (interactive)
    (if (region-active-p)
        (bt/comment)
      (progn
        (bt/old-select "ip")
        (bt/left)
        (bt/comment))))

  (defun bt/vanish-eol ()
    (interactive)
    (bt/select-eol)
    (bt/vanish))

  (defun bt/dupe-eol ()
    (interactive)
    (bt/select-eol)
    (bt/dupe))

  (defun bt/indent-paragraph ()
    (interactive)
    (bt/old-select "ip")
    (bt/indent))

  (defun bt/wipe-eol ()
    (interactive)
    (if (not (eolp))
        (progn
          (bt/select-eol)
          (bt/wipe))))

  (defun bt/paste-raw ()
    (interactive)
    (if current-prefix-arg (insert " "))
    (yank))

  (defun bt/paste-fmt ()
    (interactive)
    (bt/paste-raw)
    (call-interactively 'indent-region))

  (defun bt/paste-eol ()
    (interactive)
    (bt/eol)
    (bt/paste-raw))

  (defun bt/graft (c)
    (interactive "cgraft: ")
    (bt/vanish)
    (insert c)
    (bt/left))

  (defun bt/cycle-spacing ()
    (interactive)
    (cycle-spacing 1 nil 'fast))

  (setq bt/repeat-cmd nil)
  (setq bt/repeat-arg nil)
  (defun bt/repeat-command ()
    (interactive)
    (if bt/repeat-cmd
        (if bt/repeat-arg
            (progn
              (message (concat "repeating: " bt/repeat-cmd (string bt/repeat-arg)))
              (call-interactively ((key-binding bt/repeat-cmd) bt/repeat-arg)))
          (progn
            (message (concat "repeating: " bt/repeat-cmd))
            (call-interactively (key-binding bt/repeat-cmd))))))

  (defun bt/on-blank-line () (= (line-beginning-position) (line-end-position)))

  (bt/defun-add bt/trade-eol bt/wipe-eol)
  (bt/defun-add bt/trade-bol bt/wipe-bol)
  (bt/defun-add-fn bt/trade-line
    (if (not (bt/on-blank-line))
        (progn
          (bt/old-select "il")
          (bt/wipe)))
    (bt/indent)
    (bt/add))

  (bt/defun-add bt/add-eol bt/eol)
  (bt/defun-add bt/add-bov bt/bov)

  (defun bt/newline-here ()
    (interactive)
    (newline-and-indent)
    (bt/add))

  (defun bt/newline-above ()
    (interactive)
    (bt/bol)
    (newline-and-indent)
    (bt/up)
    (bt/trade-line))

  (defun bt/newline-below ()
    (interactive)
    (bt/eol)
    (newline-and-indent)
    (bt/add))

  (defun bt/paste-line ()
    (interactive)
    (bt/bol)
    (bt/down)
    (bt/paste-fmt))

  ;; (defun bt/pend-all-specs ()
  ;;   (interactive)
  ;;   (bt/bob)
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

  (defalias 'bt/join-line-above 'delete-indentation)
  (defun bt/join-line-below ()
    (interactive)
    (bt/down)
    (bt/join-line-above))

  (defun bt/wip-commit ()
    (interactive)
    (shell-command "wip"))

  (defun bt/pbcopy ()
    (interactive)
    (bt/adjust-selection-for-edit)
    (shell-command-on-region (region-beginning) (region-end) "pbcopy")
    (deactivate-mark))

  (defun bt/pbcopy-str (s)
    (interactive)
    (let ((s (format "echo -n \"%s\" | pbcopy" s)))
      (shell-command s)
      (message "s: ")
      (message s)
      ))

  (defun bt/pbcopy-ruby ()
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

  (defun bt/copy-gh-line-number ()
    (interactive)
    (bt/pbcopy-str (bt/appserver-github-url (format-mode-line "%l"))))

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

  (defun bt/show-filepaths ()
    (interactive)
    (let ((buffer-path-from-root
           (replace-regexp-in-string (regexp-quote (projectile-project-root)) "" (buffer-file-name) nil 'literal)))
      (message (concat "rel: " (bt/filepath-from-root) "\nabs: " (buffer-file-name)))))

  (defun bt/filepath-from-root ()
    (replace-regexp-in-string (regexp-quote (projectile-project-root)) "" (buffer-file-name) nil 'literal))

  (defun bt/cp-filepath ()
    (interactive)
    (bt/pbcopy-str (bt/filepath-from-root)))

  (defun bt/test-file ()
    (interactive)
    (bt/pbcopy-str (concat "s test " (bt/filepath-from-root))))

    ;;  (bt/appserver-github-url (format-mode-line "%l"))))
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
  (defun bt/define-target-key (i-or-o input-key action target-key)
    (let* ((target (concat i-or-o target-key))
           (key-seq (concat input-key target)))
      (define-key modalka-mode-map key-seq (lambda () (interactive)
                                             (setq bt/repeat-cmd key-seq)
                                             (setq bt/repeat-arg nil)
                                             (save-excursion
                                               (bt/old-select target)
                                               (funcall action))))))

  (defun bt/define-target-keys (i-or-o key action targets)
    (mapcar (apply-partially 'bt/define-target-key i-or-o key action) targets))

  (defun bt/select-inner-outer (i-or-o target-key selection-start-char)
    (cond
     ((string= target-key "w")
      (bt/old-select (concat i-or-o "w")))

     ((string= target-key "W")
      (bt/old-select (concat i-or-o "W")))

     ((string= target-key "p")
      (bt/old-select (concat i-or-o "p")))

     ((string= i-or-o "i")
      (search-backward selection-start-char)
      (er/expand-region 1)
      (bt/right)
      (bt/flip-selection)
      (bt/left)
      (bt/flip-selection))

     ((string= i-or-o "o")
      (search-backward selection-start-char)
      (er/expand-region 1))))

  (defun bt/define-delimited-key (input-key i-or-o selection-start-char action target-key)
    (let* ((key-seq (concat input-key i-or-o target-key))
           (run-cmd
            (lambda () (interactive)
              (bt/select-inner-outer i-or-o target-key selection-start-char)
              (funcall action)))
           (fn (lambda ()
                 (interactive)
                 (setq bt/repeat-cmd key-seq)
                 (setq bt/repeat-arg nil)
                 (cond
                  ((string= input-key "c") (save-excursion (funcall run-cmd)))
                  (t (funcall run-cmd))))))
      (define-key modalka-mode-map key-seq fn)))

  (defun bt/define-until-key (cmd-key key action search)
    (let ((key-seq (concat cmd-key key)))
      (define-key modalka-mode-map key-seq (lambda (target)
                                             (interactive "c")
                                             (setq bt/repeat-cmd key-seq)
                                             (setq bt/repeat-arg (string target))
                                             (save-excursion
                                               (set-mark-command nil)
                                               (funcall search target)
                                               (funcall action))))))

  (defun bt/define-until-keys (key action)
    (bt/define-until-key key "u" action 'bt/until)
    (bt/define-until-key key "l" action 'bt/bw-until)
    (bt/define-until-key key "y" action 'bt/past)
    )

  (defun bt/chars (string)
    (split-string string "" 'f))

  (defun bt/define-delimited-targets (i-or-o key action pair)
    (mapcar (apply-partially 'bt/define-delimited-key key i-or-o (cdr pair) action) (bt/chars (car pair))))

  (defun bt/define-prefix-action (key action)
    (let ((delimited-targets '(("b()" . "(") ("B{}" . "{") ("[]" . "[") ("S'" . "'") ("s\"" . "\"")))
          (targets (bt/chars "lpwW")))
      (bt/define-until-keys key action)
      (bt/define-target-keys "i" key action targets)
      (bt/define-target-keys "o" key action targets)
      (mapc (apply-partially 'bt/define-delimited-targets "i" key action) delimited-targets)
      (mapc (apply-partially 'bt/define-delimited-targets "o" key action) delimited-targets)))

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
        ("M-$" . 'bt/normal) ("<f8>" . 'bt/full-save)
        ;; ("M-SPC" . 'bt/m-x-menu)
        ("M-+" . 'bt/autocomplete)
        ("M-# M-#" . 'bt/select)

        ;; ("C-M-E" . 'bt/uppercase)
        ;; ("C-M-N" . 'bt/downcase)
        ;; ("C-M-I" . 'bt/capitalize)
        ;; ("C-M-O" . 'bt/capitalize)

        ;; ("M-RET" . 'bt/newline-above) fixme: work with org
        ("M-@" . 'bt/newline-below)

        ("C-x z" . 'bt/noop)

        ("M-(" . 'winner-undo)
        ("M-)" . 'winner-redo)

        ("M-&" . 'bt/fold) ("M-]" . 'bt/unfold)
        ("M-{" . 'bt/search-project) ("M-}" . 'bt/regex-search-project)
        ;; ("M-;" . 'bt/noop)

        ("M-Q" . 'bt/noop)
        ("M-q M-DEL RET" . 'save-buffers-kill-terminal)
        ("M-q '" . 'bt/go-to-buffer)
        ("M-q o" . 'bt/open-file)
        ("M-q DEL" . 'bt/quit-window)
        ("M-q M-m" . 'bt/maximize)
        ("M-q SPC" . 'bt/dired)
        ;; ("qp" . 'bt/quit-popups)

        ("M-q H" . 'bt/split-left)
        ("M-q N" . 'bt/split-down)
        ("M-q E" . 'bt/split-up)
        ("M-q I" . 'bt/split-right)

        ("M-q M-h" . 'bt/switch-window-left)
        ("M-q M-n" . 'bt/switch-window-down)
        ("M-q M-e" . 'bt/switch-window-up)
        ("M-q M-i" . 'bt/switch-window-right)

        ("M-g o" . dumb-jump-go-other-window)
        ("M-g j" . dumb-jump-go)
        ("M-g b" . dumb-jump-back)
        ("M-g i" . dumb-jump-go-prompt)
        ;; ("M-g x" . dumb-jump-go-prefer-external)
        ;; ("M-g z" . dumb-jump-go-prefer-external-other-window)


        ("M-w" . 'bt/wipe-line) ("M-W" . 'bt/noop) ("C-w" . 'bt/wipe-eol)
        ("M-b" . 'bt/become-line) ("M-B" . 'bt/noop) ("C-b" . 'bt/become-eol)
        ("M-p" . 'bt/paste-fmt) ("M-P" . 'bt/paste-raw) ("C-p" . 'bt/paste-eol)
        ("M-f" . 'bt/flip-flop) ("M-F" . 'bt/noop) ("C-f" . 'bt/flip-flop)

        ("M-a" . 'bt/add) ("M-A" . 'bt/add-bov) ("C-a" . 'bt/add-eol)
        ("M-r" . 'bt/repeat-command) ("M-R" . 'bt/noop) ("C-r" . 'bt/repeat-command)
        ("M-s" . 'bt/select) ("M-S" . 'bt/select-line) ("C-s" . 'bt/select-eol)
        ("M-t" . 'bt/trade-line) ("M-T" . 'bt/noop) ("C-t" . 'bt/trade-eol)
        ;; ("M-g" . 'bt/graft) ("M-G" . 'bt/noop)

        ("M-/" . 'bt/help) ("M-?" . 'bt/help)
        ("M-," . 'bt/m-x-menu) ("M-*" . 'bt/m-x-menu)
        ;; ("M-x" . 'bt/exchange) ("M-X" . 'bt/exchange-exact)
        ("M-c" . 'bt/copy-line) ("M-C" . 'bt/copy-line)
        ("M-d" . 'bt/m-del) ("M-D" . 'bt/dupe-line) ("C-d" . 'bt/dupe-eol)
        ("M-v" . 'bt/vanish) ("M-V" . 'bt/noop) ("C-v" . 'bt/vanish)

        ("M-;" .'bt/comment)
        ("M-|" . 'bt/comment-par)
        ("M-\\" .'bt/toggle-comments)

        ("M-j" . 'bt/join-line-below) ("M-J" . 'bt/join-line-above)

        ("M-`" . 'pop-to-mark-command) ("M-~" . 'helm-all-mark-rings)

        ("M-z" . 'bt/bol)
        ("M-l" . 'bt/bw-bow) ("C-l" . 'bt/recenter)
        ("M-u" . 'bt/undo) ("M-U" . 'bt/undo-tree) ("C-u" . 'bt/undo)
        ("M-y" . 'bt/bow) ("C-y" . 'bt/noop)
        ("M-:" . 'bt/redo)

        ("M-h" . 'bt/left) ("C-h" . 'bt/bw-drag)
        ("M-n" . 'bt/down) ("C-n" . 'bt/drag-line)
        ("M-e" . 'bt/up) ("C-e" . 'bt/bw-drag-line)
        ("M-i" . 'bt/right) ("M-!" . 'bt/drag)
        ("M-o" . 'bt/eow) ("C-o" . 'universal-argument)
        ("M-'" . 'bt/search) ("M-\"" . 'bt/search)

        ("M-k" . 'bt/bov) ("M-K" . 'bt/noop)
        ("M-m" . 'bt/bop) ("M-M" . 'bt/bob)
        ("M-DEL" . 'bt/m-bs)
        ("M-." . 'bt/eop) ("M->" . 'bt/eob)
        ("M--" . 'bt/eol) ("C-_" . 'bt/eol)

        ;; :map minibuffer-local-completion-map
        ;; ("C-o" . 'minibuffer-complete)

        :map isearch-mode-map
        ("M-e" . 'isearch-repeat-backward)
        ("M-n" . 'isearch-repeat-forward)
        ("M-y" . 'isearch-yank-word-or-char)
        ("M-o" . 'isearch-yank-word-or-char)
        ("M-s" . 'helm-swoop-from-isearch)
        ("C-r" . 'bt/noop)
        ("C-s" . 'bt/noop)

        :map modalka-mode-map
        ("RET" . 'bt/newline-here)

        ("!" . 'bt/noop) ("@" . 'bt/noop) ("#" . 'bt/noop) ("$" . 'bt/noop) ("%" . 'bt/noop) ("^" . 'bt/noop) ("&" . 'bt/to-line) ("*" . 'bt/noop)
        ("1" . 'bt/noop) ("2" . 'bt/noop) ("3" . 'bt/noop) ("4" . 'bt/noop) ("5" . 'bt/noop) ("6" . 'bt/noop) ("7" . 'bt/noop) ("8" . 'bt/noop) ("9" . 'bt/noop) ("0" . 'bt/to-line)

        ("[" . 'bt/fold) ("]" . 'bt/unfold)
        ("{" . 'bt/search-project) ("}" . 'bt/regex-search-project)
        (";" . 'bt/noop)

        ("q M-DEL RET" . 'save-buffers-kill-terminal)

        ("q'" . 'bt/go-to-buffer)
        ("qo" . 'bt/open-file)
        ("q DEL" . 'bt/quit-window)
        ("qm" . 'bt/maximize)
        ("q)" . 'dumb-jump-go)
        ("q(" . 'dumb-jump-back)
        ("q M-)" . 'dumb-jump-go-other-window)
        ;; ("q M-(" . 'dumb-jump-go-prompt)
        ;; ("qp" . 'bt/quit-popups)

        ("qH" . 'bt/split-left)
        ("qN" . 'bt/split-down)
        ("qE" . 'bt/split-up)
        ("qI" . 'bt/split-right)

        ("qh" . 'bt/switch-window-left)
        ("qn" . 'bt/switch-window-down)
        ("qe" . 'bt/switch-window-up)
        ("qi" . 'bt/switch-window-right)

        ("q SPC d" . 'bt/dired)
        ("q SPC s" . 'bt/show-filepaths)
        ("q SPC c" . 'bt/cp-filepath)
        ("q SPC t" . 'bt/test-file)

        ("W" . 'bt/noop)
        ("B" . 'bt/noop)
        ("p" . 'bt/paste-fmt) ("P" . 'bt/paste-raw)
        ("f" . 'bt/flip-flop) ("F" . 'bt/noop)

        ("a" . 'bt/add) ("A" . 'bt/add-bov)
        ("r" . 'bt/repeat-command) ("R" . 'bt/noop)
        ("s" . 'bt/select) ("S" . 'bt/select-line)
        ("T" . 'bt/noop)
        ("g" . 'bt/graft) ("G" . 'xah-toggle-previous-letter-case)

        ("?" . 'bt/help)

        ;; help
        ("/ SPC" . 'help-for-help)
        ("/m" . 'bt/help-messages)
        ("/k" . 'bt/help-key)
        ("//k" . 'bt/help-all-keys)
        ("/'" . 'bt/help-search)

        ;; modifier
        ("SPC u" . 'bt/undo-tree)
        ("SPC p" . 'bt/paste-from-history)
        ("SPC c" . 'bt/pbcopy)
        ("SPC r" . 'bt/pbcopy-ruby)
        ("SPC s" . 'bt/reselect)
        ("SPC x" . 'bt/exchange-regex)
        ("SPC X" . 'bt/query-exchange-regex)

        ;; command
        ("SPC b" . 'bt/quick-bookmark)
        ("SPC f" . 'bt/quick-find-bookmark)
        ("SPC B" . 'bt/bookmark)
        ("SPC F" . 'bt/find-bookmark)
        ("SPC g" . 'bt/git)
        ("SPC G" . 'nc/open-in-github)
        ("SPC n" . 'bt/note)
        ("SPC m" . 'bt/start-macro)
        ("SPC M" . 'bt/end-macro)
        ("SPC RET" . 'bt/run-macro)
        ("SPC (" . 'bt/eval-expr) ("SPC )" . 'bt/eval-inline)

        ("," . 'bt/m-x-menu) ("<" . 'bt/noop)
        ("x" . 'bt/exchange) ("X" . 'bt/query-exchange)
        ("C" . 'bt/copy-line)
        ("D" . 'bt/dupe-line)
        ("v" . 'bt/vanish) ("V" . 'bt/noop)

        (";" .'bt/comment)
        ("|" . 'bt/comment-par)
        ("\\" .'bt/toggle-comments)

        ("=a" . 'bt/rect-add)
        ("=t" . 'bt/rect-trade)
        ("=w" . 'bt/rect-wipe)
        ("=p" . 'bt/rect-paste)
        ("j" . 'bt/join-line-below) ("J" . 'bt/join-line-above)

        ("`" . 'pop-global-mark) ("~" . 'helm-all-mark-rings)
        ("(" . 'pop-to-mark-command) (")" . 'bt/cycle-spacing)

        ("z" . 'bt/bol) ("Z" . 'bt/noop)
        ("l" . 'bt/bw-bow) ("L" . 'bt/bw-boW)
        ("u" . 'bt/undo) ("U" . 'bt/undo-tree)
        ("y" . 'bt/bow) ("Y" . 'bt/boW)
        (":" . 'bt/redo)

        ("<deletechar>" . 'bt/del)

        ("h" . 'bt/left) ("H" . 'bt/noop)
        ("n" . 'bt/down) ("N" . 'bt/noop)
        ("e" . 'bt/up) ("E" . 'bt/noop)
        ("i" . 'bt/right) ("I" . 'bt/noop)
        ("o" . 'bt/eow) ("O" . 'bt/eoW)
        ("'" . 'bt/search) ("\"" . 'bt/search)

        ("k" . 'bt/bov) ("K" . 'bt/noop)
        ("m" . 'bt/bop) ("M" . 'bt/bob)
        ("DEL" . 'bt/bs)
        ("." . 'bt/eop) (">" . 'bt/eob)
        ("-" . 'bt/eol) ("_" . 'bt/eol)

        )

  :config
  (add-hook 'text-mode-hook #'modalka-mode)
  (add-hook 'prog-mode-hook #'modalka-mode)

  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond (modalka-mode '("brightblack" . "color-245"))
                                 (t '("brightblack" . "red")))))
                (set-face-foreground 'mode-line (car color))
                (if (featurep 'linum) (set-face-foreground 'linum (cdr color)))
                (set-face-background 'mode-line (cdr color)))))

  ;; (define-key global-map (kbd "C-M-x") ctl-x-map)

  (add-hook 'post-command-hook 'bt/adjust-selection-hook)
  )
