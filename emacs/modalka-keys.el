;; -*- lexical-binding: t; -*-

(use-package selected
  :ensure t
  :commands selected-minor-mode

  :bind
  (:map selected-keymap
        ("c" . bt/copy-selection)
        ("v" . bt/vanish-selection)
        ("d" . bt/dupe-selection)
        ("t" . bt/trade-selection)
        ("w" . bt/wipe-selection)
        )

  :config
  (bt/define-prefix-action "c" 'bt/copy)
  (bt/define-prefix-action "d" 'bt/dupe)
  (bt/define-prefix-action "t" 'bt/trade)
  (bt/define-prefix-action "w" 'bt/wipe)
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

  (defalias 'bt/menu 'helm-M-x)

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

  (defun bt/access-file-left ()
    (interactive)
    (bt/split-left)
    (bt/access-file))

  (defun bt/access-file-right ()
    (interactive)
    (bt/split-right)
    (bt/access-file))

  (defun bt/access-file-up ()
    (interactive)
    (bt/split-up)
    (bt/access-file))

  (defun bt/access-file-down ()
    (interactive)
    (bt/split-down)
    (bt/access-file))

  (defalias 'bt/access-file 'helm-projectile-find-file)
  (defalias 'bt/access-file-this-window 'helm-projectile-find-file)
  (defalias 'bt/buffer-list 'ibuffer)
  (defalias 'bt/go-to-buffer 'switch-to-buffer)
  (defalias 'bt/dired 'dired-jump)
  ;; (defalias 'bt/locate-file 'find-file)

  (defalias 'bt/whole-window 'delete-other-windows)
  (defalias 'bt/close-window 'delete-window)
  (defalias 'bt/window-revert 'winner-undo)

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
  (defalias 'bt/go-line 'goto-line)

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

  (defun bt/match ()
    (interactive)
    (bt/mark-set ?M)
    (helm-swoop :$query ""))

  (defun bt/to-match-start ()
    (bt/mark-jump ?M))

  ;; use M-m M-p instead
  ;; (defun bt/match-again ()
  ;;   (interactive)
  ;;   (helm-swoop :$query helm-swoop-pattern))

  ;; use M-m M-o instead
  ;; (defalias 'bt/match-symbol 'helm-swoop)
  ;; (defalias 'bt/match-multi-all 'helm-multi-swoop-all)
  ;; (defalias 'bt/match-multi-some 'helm-multi-swoop)
  ;; (defalias 'bt/match 'isearch-forward-regexp)
  ;; (defalias 'bt/match-exact 'isearch-forward)

  (defalias 'bt/exchange 'replace-string)
  (defalias 'bt/exchange-reg 'replace-regexp)
  (defalias 'bt/query 'query-replace-regexp)
  (defalias 'bt/query-exact 'query-replace)

  (defun bt/query ()
    (interactive)
    (let ((scroll-conservatively 0))
      (call-interactively 'query-replace-regexp)))

  (defun bt/cursor-to-middle ()
    (interactive)
    (recenter-top-bottom))

  (defun bt/cursor-to-top ()
    (interactive)
    (recenter-top-bottom 0))

  (defun bt/cursor-to-bottom ()
    (interactive)
    (recenter-top-bottom -1))

  (defun bt/mark-set (c)
    (interactive "cset mark: ")
    (point-to-register (capitalize c)))

  ;; (defalias 'bt/mark-set 'point-to-register)
  (defalias 'bt/mark-jump 'jump-to-register)

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

  (defun bt/capture (s)
    (interactive "scapture: ")
    (let* ((org-file "/home/vagrant/org/inbox.org")
           (padded-output (shell-command-to-string (concat "cat " org-file " | wc -l")))
           (output (projectile-trim-string padded-output)))
      (if (string= s "count")
          (message (concat (number-to-string (- (string-to-number output) 1)) " items total"))
        (progn
          (shell-command (concat "echo '** '" s " >> " org-file))
          (message (concat "added to inbox.org (" output " items total)"))))))

  (defun bt/editable ()
    (and (not buffer-read-only)
         (buffer-file-name)))

  (defun bt/save-fn (msg)
    (interactive)
    (message (concat "saving: " msg))
    (if (bt/editable) (save-buffer) (message "not saving...")))

  (defun bt/save ()
    (interactive)
    (if (eq this-command last-command)
        (call-interactively 'bt/full-save)
      (bt/save-fn "save")))

  (defun bt/full-save ()
    (interactive)
    (bt/whitespace)
    (message "fs attempt...")
    (bt/save-fn "full-save")
    )

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
    (set-marker bt/mark-anchor-eol (+ 1 (point)))
    )

  (defalias 'bt/deselect 'deactivate-mark)
  (defalias 'bt/reselect 'exchange-point-and-mark)
  (defalias 'bt/flip-selection 'exchange-point-and-mark)
  (defalias 'bt/autocomplete 'hippie-expand)
  (defalias 'bt/help 'help-command)
  (defalias 'bt/paste-ring 'helm-show-kill-ring)

  (defalias 'bt/revert 'undo-tree-undo)
  (defalias 'bt/revert-tree 'undo-tree-visualize)

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

  (defun bt/noop () (interactive) )
  (defun bt/add () (interactive) (modalka-mode -1))
  (defun bt/normal () (interactive) (modalka-mode) (bt/deselect) (bt/save))
  (defun bt/normal-without-save () (interactive) (modalka-mode))

  (defun bt/copy () (interactive) (whole-line-or-region-kill-ring-save 1))
  (defun bt/wipe () (interactive) (whole-line-or-region-kill-region 1))
  (defun bt/vanish ()
    (interactive)
    (if (region-active-p)
        (whole-line-or-region-delete 1)
      (delete-char 1)))

  (bt/defun-add bt/trade bt/wipe)

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

  (defmacro bt/defun-selection-fn-ex (name fn)
    `(defun ,name ()
       (interactive)
       (save-excursion
         (bt/adjust-selection-for-edit)
         (,fn))))

  (bt/defun-selection-fn-ex bt/copy-selection bt/copy)
  (bt/defun-selection-fn-ex bt/vanish-selection bt/vanish)
  (bt/defun-selection-fn bt/dupe-selection bt/dupe)
  (bt/defun-selection-fn bt/trade-selection bt/trade)
  (bt/defun-selection-fn bt/wipe-selection bt/wipe)

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

  ;; this does not select the newline, which is probably what I want
  (defun bt/select-eol ()
    (interactive)
    (bt/select)
    (bt/eol)
    (bt/flip-selection))

  (defun bt/old-select-line ()
    (interactive)
    (bt/old-select "il"))

  (defun bt/comment ()
    (interactive)
    (whole-line-or-region-comment-dwim nil))

  (defun bt/comment-par ()
    (interactive)
    (if (region-active-p)
        (bt/comment)
      (progn
        (bt/old-select "ip")
        (bt/comment))))

  (defun bt/vanish-eol ()
    (interactive)
    (bt/select-eol)
    (bt/vanish))

  (defun bt/copy-eol ()
    (interactive)
    (save-excursion
      (bt/select-eol)
      (bt/copy)))

  (defun bt/dupe-eol ()
    (interactive)
    (bt/copy-eol)
    (bt/paste-eol))

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

  (defun bt/wipe-line ()
    (interactive)
    (bt/old-select-line)
    (bt/wipe))

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

  (defun bt/one-space ()
    (interactive)
    (just-one-space))

  (defun bt/zero-spaces ()
    (interactive)
    (just-one-space 0))

  ;; (defun bt/repeat-command ()
  ;;   (interactive)
  ;;   (if bt/directional-key-repeat-seq
  ;;       (if bt/directional-key-repeat-arg
  ;;           (call-interactively ((key-binding bt/directional-key-repeat-seq) bt/directional-key-repeat-arg))
  ;;         (call-interactively (key-binding bt/directional-key-repeat-seq)))
  ;;     (message "no bt/directional-key-repeat-seq")))

  (defun bt/on-blank-line () (= (line-beginning-position) (line-end-position)))

  (bt/defun-add bt/trade-eol bt/wipe-eol)
  (bt/defun-add bt/trade-bol bt/wipe-bol)
  (bt/defun-add-fn bt/trade-line
    (if (not (bt/on-blank-line))
        (progn
          (bt/old-select-line)
          (bt/wipe)))
    (bt/indent))

  (bt/defun-add bt/add-eol bt/eol)
  (bt/defun-add bt/add-bov bt/bov)

  (defun bt/blank-below ()
    (interactive)
    (save-excursion
      (bt/eol)
      (newline-and-indent))
    )

  (defun bt/blank-above ()
    (interactive)
    (save-excursion
      (bt/bol)
      (insert "\n"))
    )

  (defun bt/add-below ()
    (interactive)
    (bt/blank-below)
    (bt/down)
    (bt/indent)
    (bt/add))

  (defun bt/add-above ()
    (interactive)
    (bt/blank-above)
    (bt/up)
    (bt/indent)
    (bt/add))

  (defun bt/paste-line ()
    (interactive)
    (bt/bol)
    (bt/down)
    (bt/paste-fmt))

  (defalias 'bt/join-line-above 'delete-indentation)
  (defun bt/join-line-below ()
    (interactive)
    (bt/down)
    (bt/join-line-above))


  ;; directional keys
  (defun bt/define-target-key (i-or-o input-key action target-key)
    (let* ((target (concat i-or-o target-key))
           (key-seq (concat input-key target)))
      (define-key modalka-mode-map key-seq (lambda () (interactive)
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
                 (cond
                  ((string= input-key "c") (save-excursion (funcall run-cmd)))
                  (t (funcall run-cmd))))))
      (define-key modalka-mode-map key-seq fn)))

  (defun bt/define-until-key (cmd-key key action search)
    (let ((key-seq (concat cmd-key key)))
      (define-key modalka-mode-map key-seq (lambda (target) (interactive "c")
                                             (save-excursion
                                               (set-mark-command nil)
                                               (funcall search target)
                                               (funcall action))))))

  (defun bt/define-until-keys (key action)
    (bt/define-until-key key "u" action 'bt/until)
    (bt/define-until-key key "b" action 'bt/bw-until)
    (bt/define-until-key key "U" action 'bt/past)
    (bt/define-until-key key "B" action 'bt/bw-past))

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
  ;; ("<caps lock>" . "<f9>") ;; karabiner
  ;; ("<f9>" . "M-$") ;; iterm
  ;; ("C-'" . "M-%")
  ;; ("S-TAB" . "M-^")
  ;; ("M-[" . "M-&")
  ;; ("S-M-RET" . "M-*")
  ;; ("M--" . "M-(")
  ;; ("" . "M-)")
  ;; ("" . "M-_")
  ;; ("" . "M-=")
  ;; ("M-<deletechar>" . "M->")

  :bind
  (:map my-keys-minor-mode-map
        ("M-$" . 'bt/normal) ("S-<f8>" . 'bt/full-save) ("C-<f8>" . 'bt/full-save)
        ("M-SPC" . 'bt/menu)
        ("M-:" . 'bt/capture)
        ;; ("M-G" . 'bt/git)

        ("M-RET" . 'bt/blank-below) ("M-*" . 'bt/blank-above) ("M-@" . 'bt/add-above)

        ("C-x z" . 'bt/noop)
        ("C-M-r" . 'bt/window-revert)
        ("C-M-q" . 'bt/close-window)
        ("C-M-x" . 'bt/close-window)
        ("C-M-c" . 'bt/checkout-buffer)

        ("C-M-w" . 'bt/whole-window)
        ("C-M-l" . 'bt/buffer-list)
        ("C-M-b" . 'bt/go-to-buffer)
        ("C-M-d" . 'bt/dired)

        ("C-M-a C-M-t" . 'bt/access-file-this-window)
        ("C-M-a C-M-h" . 'bt/access-file-left)
        ("C-M-a C-M-n" . 'bt/access-file-down)
        ("C-M-a C-M-e" . 'bt/access-file-up)
        ("C-M-a C-M-i" . 'bt/access-file-right)

        ("C-M-s C-M-h" . 'bt/split-left)
        ("C-M-s C-M-n" . 'bt/split-down)
        ("C-M-s C-M-e" . 'bt/split-up)
        ("C-M-s C-M-i" . 'bt/split-right)

        ("C-M-h" . 'bt/switch-window-left)
        ("C-M-n" . 'bt/switch-window-down)
        ("C-M-e" . 'bt/switch-window-up)
        ("C-M-i" . 'bt/switch-window-right)

        ("M-&" . 'bt/fold) ("M-{" . 'bt/fold-max)
        ("M-]" . 'bt/unfold) ("M-}" . 'bt/unfold) ("C-]" . 'bt/noop)

        ("M-q" . 'bt/query)
        ("M-w" . 'bt/wipe) ("C-w" . 'bt/wipe-eol)
        ("M-b" . 'bt/bw-wipe-word) ("C-b" . 'bt/bw-wipe-word)
        ("M-p" . 'bt/paste-fmt) ("C-p" . 'bt/paste-eol)
        ("M-f" . 'bt/wipe-word) ("C-f" . 'bt/wipe-word)

        ("M-Q" . 'bt/query-exact)
        ("M-W" . 'bt/noop)
        ("M-B" . 'bt/noop)
        ("M-P" . 'bt/paste-ring)
        ("M-F" . 'bt/noop)

        ("M-," . 'bt/noop) ("M-<" . 'bt/noop)
        ("M-a" . 'bt/add)   ("C-a" . 'bt/add-eol)
        ("M-r" . 'bt/revert-tree) ("C-r" . 'bt/revert)
        ("M-s" . 'bt/select) ("C-s" . 'bt/select-eol)
        ("M-t" . 'bt/trade-line) ("C-t" . 'bt/trade-eol)
        ;; ("M-g" . 'bt/noop) ("C-g" . 'bt/noop)

        ("M-A" . 'bt/noop)
        ("M-R" . 'bt/noop)
        ("M-S" . 'bt/noop)
        ("M-T" . 'bt/noop)
        ("M-G" . 'bt/noop)

        ;; ("M-x" . 'bt/noop) ("C-x" . 'bt/noop)
        ("M-c" . 'bt/copy-line) ("C-c" . 'bt/copy-eol)
        ("M-d" . 'bt/m-del) ("C-d" . 'bt/dupe-eol)
        ("M-v" . 'bt/vanish) ("C-v" . 'bt/vanish)

        ("M-/" . 'bt/help)

        ("M-z" . 'bt/bol) ("C-z" . 'bt/noop)
        ("M-l" . 'bt/bw-bow) ("C-l" . 'recenter-top-bottom)
        ("M-u" . 'bt/noop) ("C-u" . 'bt/noop)
        ("M-y" . 'bt/bow) ("C-y" . 'bt/noop)
        ("M-:" . 'bt/match)
        ("M->" . 'bt/m-del)

        ("M-Z" . 'bt/noop)
        ("M-D" . 'bt/downcase)
        ("M-U" . 'bt/uppercase)
        ("M-C" . 'bt/capitalize)

        ("M-h" . 'bt/left) ("C-h" . 'bt/noop)
        ("M-n" . 'bt/down) ("C-n" . 'bt/noop)
        ("M-e" . 'bt/up) ("C-e" . 'bt/noop)
        ("M-i" . 'bt/right) ("M-!" . 'bt/noop)
        ("M-o" . 'bt/eow) ("C-o" . 'universal-argument)
        ("M-'" . 'bt/noop) ("M-\"" . 'bt/noop) ("M-%" . 'bt/noop)

        ("M-H" . 'bt/bw-drag)
        ("M-I" . 'bt/drag)
        ("M-E" . 'bt/bw-drag-line)
        ("M-N" . 'bt/drag-line)
        ("M-L" . 'bt/bw-drag-word)
        ("M-Y" . 'bt/drag-word)

        ("M-k" . 'bt/bov) ("M-K" . 'bt/noop) ("C-k" . 'bt/bov)
        ("M-m" . 'bt/bop) ("M-M" . 'bt/noop)
        ("M-DEL" . 'bt/m-bs)
        ("M-." . 'bt/eop) ;; ("M->" . 'bt/noop)
        ("M--" . 'bt/eol) ("C-_" . 'bt/eol) ("M-_" . 'bt/noop)

        ("M-j" . 'bt/join-line-below) ("M-J" . 'bt/join-line-above)

        ;; :map minibuffer-local-completion-map
        ;; ("C-o" . 'minibuffer-complete)

        :map isearch-mode-map
        ("M-e" . 'isearch-repeat-backward)
        ("M-n" . 'isearch-repeat-forward)
        ("M-y" . 'isearch-yank-word-or-char)
        ("M-o" . 'isearch-yank-word-or-char)
        ("C-r" . 'bt/noop)
        ("C-s" . 'bt/noop)

        :map modalka-mode-map
        ("RET" . 'bt/add-below)
        ("SPC" . 'bt/noop)

        ("!" . 'bt/noop) ("@" . 'bt/noop) ("#" . 'bt/noop) ("$" . 'bt/noop) ("%" . 'bt/noop)
        ("1" . 'bt/one-space) ("2" . 'bt/noop) ("3" . 'bt/noop) ("4" . 'bt/noop) ("5" . 'bt/noop)

        ("^" . 'bt/noop) ("&" . 'bt/noop) ("*" . 'bt/noop)
        ("6" . 'bt/noop) ("7" . 'bt/noop) ("8" . 'bt/noop) ("9" . 'bt/noop) ("0" . 'bt/zero-spaces)

        ("~" . 'bt/noop) ("`" . 'bt/noop)

        ("q" . 'bt/query) ("Q" . 'bt/query-exact)
        ("W" . 'bt/wipe-line)
        ("b" . 'bt/bw-wipe-word) ("B" . 'bt/noop)
        ("p" . 'bt/paste-fmt) ("P" . 'bt/paste-raw)
        ("f" . 'bt/wipe-word) ("F" . 'bt/noop)

        ("a" . 'bt/add) ("A" . 'bt/add-bov)
        ("r" . 'bt/revert-tree) ("R" . 'bt/noop)
        ("s" . 'bt/select) ("S" . 'bt/select-line)
        ("T" . 'bt/trade-line)
        ("g" . 'bt/graft) ("G" . 'bt/noop)

        ("," . 'bt/noop) ("<" . 'bt/noop)
        ("x" . 'bt/exchange) ("X" . 'bt/exchange-exact)
        ("C" . 'bt/copy-line)
        ("D" . 'bt/dupe-line)
        ("v" . 'bt/vanish) ("V" . 'bt/vanish-line)

        ("{" . 'bt/fold) ("}" . 'bt/unfold)

        ("[" . 'bt/start-macro) ("]" . 'bt/end-macro)
        ("|" . 'bt/run-macro)

        ("/" . 'bt/comment-par) ("\\" .'bt/comment)

        ("=a" . 'bt/rect-add)
        ("=t" . 'bt/rect-trade)
        ("=w" . 'bt/rect-wipe)
        ("=p" . 'bt/rect-paste)

        ("(" . 'bt/bob) (")" . 'bt/eob)
        (";" . 'deadgrep)

        ("z" . 'bt/bol) ("Z" . 'bt/noop)
        ("l" . 'bt/bw-bow) ("L" . 'bt/bw-boW)
        ("u" . 'bt/noop) ("U" . 'bt/noop)
        ("y" . 'bt/bow) ("Y" . 'bt/boW)
        (":" . 'bt/match)
        ("<deletechar>" . 'bt/del)

        ("h" . 'bt/left) ("H" . 'bt/noop)
        ("n" . 'bt/down) ("N" . 'bt/noop)
        ("e" . 'bt/up) ("E" . 'bt/noop)
        ("i" . 'bt/right) ("I" . 'bt/noop)
        ("o" . 'bt/eow) ("O" . 'bt/eoW)
        ("'" . 'isearch-forward-regexp) ("\"" . 'isearch-backward-regexp)

        ("j" . 'bt/join-line-below) ("J" . 'bt/join-line-above)
        ("k" . 'bt/bov) ("K" . 'bt/noop)
        ("m" . 'bt/bop) ("M" . 'bt/noop)
        ("DEL" . 'bt/bs)
        ("." . 'bt/eop) (">" . 'bt/noop)
        ("-" . 'bt/eol) ("_" . 'bt/eol)

        ("`" . 'bt/help) ("~" . 'bt/help)

  ;;       ;; :map global-map

        )

  :config
  (add-hook 'text-mode-hook #'modalka-mode)
  (add-hook 'prog-mode-hook #'modalka-mode)

  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond (modalka-mode '("brightblack" . "green"))
                                 (t '("brightblack" . "red")))))
                (set-face-foreground 'mode-line (car color))
                (if (featurep 'linum) (set-face-foreground 'linum (cdr color)))
                (set-face-background 'mode-line (cdr color)))))

  (define-key global-map (kbd "M-x") ctl-x-map)

  (add-hook 'post-command-hook 'bt/adjust-selection-hook)
  (selected-minor-mode)
  )
