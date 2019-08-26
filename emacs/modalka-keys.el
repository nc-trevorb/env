;; -*- lexical-binding: t; -*-

(use-package selected
  :ensure t
  :commands selected-minor-mode

  :bind
  (:map selected-keymap
        ("c" . bt/copy-selection)
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

  ;; emacs uses "up"/"down" to refer to the text in the buffer moving,
  ;; bt/ uses direction that the cursor moves
  (defalias 'bt/scroll-up 'scroll-down-line)
  (defalias 'bt/scroll-down 'scroll-up-line)
  (defalias 'bt/scroll-up-page 'scroll-down)
  (defalias 'bt/scroll-down-page 'scroll-up)

  (defalias 'bt/split-side 'split-window-right)
  (defalias 'bt/split-below 'split-window-below)
  (defalias 'bt/fullscreen 'delete-other-windows)
  (defalias 'bt/close-window 'delete-window)
  (defalias 'bt/window-revert 'winner-undo)
  (defalias 'bt/open-file 'helm-projectile-find-file)
  (defalias 'bt/locate-file 'find-file)
  (defalias 'bt/dired 'dired-jump)

  (defalias 'bt/start-macro 'kmacro-start-macro)
  (defalias 'bt/end-macro 'kmacro-end-macro)
  (defalias 'bt/run-macro 'kmacro-end-and-call-macro)

  (defalias 'bt/bw-bow 'backward-word)
  (defalias 'bt/bow 'forward-to-word)
  (defalias 'bt/eow 'forward-word)
  (defalias 'bt/bol 'beginning-of-visual-line)
  (defalias 'bt/eol 'end-of-visual-line)
  (defalias 'bt/bob 'beginning-of-buffer)
  (defalias 'bt/eob 'end-of-buffer)
  (defalias 'bt/bop 'backward-paragraph)
  (defalias 'bt/eop 'forward-paragraph)
  (defalias 'bt/bov 'back-to-indentation) ;; visible
  (defalias 'bt/eov 'forward-paragraph)

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

  (defun bt/match-again ()
    (interactive)
    (helm-swoop :$query helm-swoop-pattern))

  (defalias 'bt/match-symbol 'helm-swoop)
  (defalias 'bt/match-multi-all 'helm-multi-swoop-all)
  ;; (defalias 'bt/match-multi-some 'helm-multi-swoop)
  ;; (defalias 'bt/match 'isearch-forward-regexp)
  ;; (defalias 'bt/match-exact 'isearch-forward)
  (defalias 'bt/exchange 'replace-string)
  (defalias 'bt/exchange-reg 'replace-regexp)
  (defalias 'bt/query 'query-replace-regexp)
  (defalias 'bt/query-exact 'query-replace)

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
  ;; (defun bt/rect-add ()
  ;;   (interactive)
  ;;   (let ((pcol (current-column))
  ;;         (mcol (save-excursion
  ;;                 (bt/flip-selection)
  ;;                 (current-column)))
  ;;         )
  ;;     (if (= pcol mcol)
  ;;         (call-interactively 'bt/rect-trade)
  ;;       (message "`ri` should only insert, use `rt` if you want to trade"))))

  (defun bt/editable ()
    (and (not buffer-read-only)
         (buffer-file-name)))

  (defun bt/save ()
    (interactive)
    (if (bt/editable) (save-buffer) (message "not saving...")))

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
  (defalias 'bt/revert 'undo)
  (defalias 'bt/wipe-ring 'helm-show-kill-ring)

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

  (defalias 'bt/wipe-ring 'helm-show-kill-ring)

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

  (bt/defun-selection-fn bt/copy-selection bt/copy)
  (bt/defun-selection-fn bt/vanish-selection bt/vanish)
  (bt/defun-selection-fn bt/dupe-selection bt/dupe)
  (bt/defun-selection-fn bt/trade-selection bt/trade)
  (bt/defun-selection-fn bt/wipe-selection bt/wipe)

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

  (defun bt/unfold ()
    (interactive)
    (set-selective-display 0))

  (defun bt/old-select (target)
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

  (defun bt/select-eol ()
    (interactive)
    (bt/select)
    (bt/eol))

  (defun bt/old-select-line ()
    (interactive)
    (bt/old-select "il"))

  (defun bt/comment ()
    (interactive)
    (whole-line-or-region-comment-dwim nil))

  (defun bt/comment-par () (interactive) (bt/old-select "ip") (bt/comment))

  (defun bt/vanish-eol ()
    (interactive)
    (bt/select-eol)
    (bt/vanish))

  (defun bt/copy-eol ()
    (interactive)
    (bt/select-eol)
    (bt/copy))

  (defun bt/dupe-eol ()
    (interactive)
    (bt/copy-eol)
    (bt/paste-eol))


  (defun bt/wipe-bol () (interactive) (kill-line 0))
  (defalias 'bt/bw-wipe-word 'backward-kill-word)
  (defalias 'bt/wipe-word 'kill-word)
  (defalias 'bt/wipe-eol 'kill-line)
  (defalias 'bt/indent 'indent-for-tab-command)
  (defalias 'bt/go-line 'goto-line)

  (defun bt/wipe-eol ()
    (interactive)
    (bt/select-eol)
    (bt/wipe))

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

  ;; (bt/defun-add-fn bt/add-below
  ;;   (let ((run (lambda ()
  ;;                (bt/eol)
  ;;                (newline-and-indent))))
  ;;     (if current-prefix-arg

  ;;         (save-excursion (funcall run))
  ;;       (funcall run))))

  ;; (defun bt/add-above ()
  ;;   (interactive)
  ;;   (bt/up)
  ;;   (bt/add-below))

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
  (defun bt/select-target (target-key)
    (cond
     ((or (string= target-key " ") (string= target-key "r"))
      (bt/adjust-selection-for-edit))

     ((string= target-key "ol")
      (bt/old-select "ol"))

     ((string= target-key "il")
      (bt/old-select-line))))

  (defun bt/define-target-key (input-key target-key action)
    (let ((key-seq (concat input-key target-key)))
      (define-key modalka-mode-map key-seq (lambda () (interactive)
                                             (save-excursion
                                               (bt/select-target target-key)
                                               (funcall action))))))

  (defun bt/define-target-keys (key action)
    (bt/define-target-key key " " action)
    (bt/define-target-key key "r" action)
    (bt/define-target-key key "ol" action)
    (bt/define-target-key key "il" action))

  (defun bt/select-in-around (i-or-o target-key selection-start-char)
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

  (defun bt/define-in-around-key (input-key i-or-o selection-start-char action target-key)
    (let* ((key-seq (concat input-key i-or-o target-key))
           (run-cmd
            (lambda () (interactive)
               (bt/select-in-around i-or-o target-key selection-start-char)
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
    (bt/define-until-key key "z" action 'bt/bw-until)
    (bt/define-until-key key "U" action 'bt/past)
    (bt/define-until-key key "Z" action 'bt/bw-past))

  (defun bt/define-in-around-targets (i-or-o key action pair)
    (mapcar (apply-partially 'bt/define-in-around-key key i-or-o (cdr pair) action) (split-string (car pair) "" 'f)))

  (defun bt/define-prefix-action (key action)
    (let ((selection-targets '(("b()" . "(") ("B{}" . "{") ("[]" . "[") ("p" . "p") ("w" . "w") ("W" . "W") ("S'" . "'") ("s\"" . "\""))))
      (bt/define-target-keys key action)
      (mapc (apply-partially 'bt/define-in-around-targets "i" key action) selection-targets)
      (mapc (apply-partially 'bt/define-in-around-targets "o" key action) selection-targets)))

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
  ;; ("" . "M-(")
  ;; ("" . "M-)")
  ;; ("" . "M-_")
  ;; ("" . "M-=")

  :bind
  (:map my-keys-minor-mode-map
   ("M-$" . 'bt/normal) ("S-<f9>" . 'bt/normal-without-save)
   ("<f7>" . 'bt/toggle-keys)
   ("M-SPC" . 'bt/select) ("C-@" . 'helm-M-x)
   ("M-g M-g" . 'bt/git)

   ("M-RET" . 'bt/blank-below) ("M-*" . 'bt/blank-above) ("M-@" . 'bt/add-above)
   ("M-7" . 'bt/match-again) ("M-8" . 'bt/match-symbol)

   ("C-x z" . 'bt/noop) ("C-q" . 'quit-window)
   ("C-M-o" . 'bt/open-file)
   ("C-M-l" . 'bt/locate-file)
   ("C-M-q" . 'bt/close-window)
   ("C-M-f" . 'bt/fullscreen)
   ("C-M-r" . 'bt/window-revert)
   ("C-M-b" . 'bt/split-below)
   ("C-M-s" . 'bt/split-side)
   ;; others:
   ;;; switch to any open buffer
   ;;; switch to any open buffer of current mode
   ;;; switch to any project file of current mode
   ;;; ibuffer


   ("C-M-h" . 'windmove-left)
   ("C-M-n" . 'windmove-down)
   ("C-M-e" . 'windmove-up)
   ("C-M-i" . 'windmove-right)

   ("M-q" . 'bt/query) ("M-Q" . 'bt/query-exact)
   ("M-w" . 'bt/wipe) ("C-w" . 'bt/wipe-eol)
   ("M-b" . 'bt/bw-wipe-word) ("C-b" . 'bt/bw-wipe-word)
   ("M-p" . 'bt/paste-fmt) ("C-p" . 'bt/paste-eol) ("M-P" . 'bt/paste-raw) ("C-M-p" . 'bt/wipe-ring)
   ("M-f" . 'bt/wipe-word) ("C-f" . 'bt/wipe-word)

   ("M-a" . 'bt/add)   ("C-a" . 'bt/add-eol)
   ("M-r" . 'bt/revert) ("C-r" . 'bt/revert)
   ("M-s" . 'bt/select) ("C-s" . 'bt/select-eol)
   ("M-t" . 'bt/trade-line) ("C-t" . 'bt/trade-eol)
   ;; ("M-g" . 'bt/noop) ("C-g" . 'bt/noop)

   ;; ("M-x" . 'bt/noop) ("C-x" . 'bt/noop)
   ("M-c" . 'bt/copy-line) ("C-c" . 'bt/copy-eol)
   ("M-d" . 'bt/dupe-line) ("C-d" . 'bt/dupe-eol)
   ("M-v" . 'bt/vanish) ("C-v" . 'bt/vanish-eol)

   ("M-z" . 'bt/bw-until) ("C-z" . 'bt/noop)
   ("M-l" . 'bt/bw-bow) ("C-l" . 'bt/noop)
   ("M-u" . 'bt/until) ("C-u" . 'bt/noop)
   ("M-y" . 'bt/bow) ("C-y" . 'bt/noop)
   ("M-;" . 'bt/mark-set) ("M-:" . 'bt/mark-jump)
   ("M-&" . 'bt/bol)
   ("M-]" . 'bt/eol) ("C-]" . 'bt/eol)

   ("M-h" . 'bt/left) ("C-h" . 'bt/noop)
   ("M-n" . 'bt/down) ("C-n" . 'bt/noop)
   ("M-e" . 'bt/up) ("C-e" . 'bt/noop)
   ("M-i" . 'bt/right) ("M-!" . 'bt/noop)
   ("M-o" . 'bt/eow) ("C-o" . 'universal-argument)
   ("M-'" . 'bt/comment) ("M-\"" . 'bt/comment-par) ("M-%" . 'bt/noop)

   ("M-k" . 'bt/bov) ("C-k" . 'bt/bov)
   ("M-m" . 'bt/match) ("M-M" . 'bt/match-multi-all)
   ("M-," . 'bt/bop) ("M-<" . 'bt/bob)
   ("M-." . 'bt/eop) ("M->" . 'bt/eob)
   ("M-/" . 'bt/help) ("C-_" . 'bt/revert)

   ("M-j" . 'bt/join-line-below) ("M-J" . 'bt/join-line-above)

   ;; :map minibuffer-local-completion-map
   ;; ("C-o" . 'minibuffer-complete)

   :map isearch-mode-map
   ("M-n" . 'isearch-repeat-forward)
   ("M-e" . 'isearch-repeat-backward)
   ("M-m" . 'isearch-repeat-forward)
   ("M-y" . 'isearch-yank-word-or-char)
   ("M-o" . 'isearch-yank-word-or-char)
   ("C-r" . 'bt/noop)
   ("C-s" . 'bt/noop)

   :map modalka-mode-map
   ("DEL" . 'bt/noop)
   ("RET" . 'bt/add-below)
   ("SPC" . 'bt/noop)

   ("!" . 'bt/run-macro) ("@" . 'bt/noop) ("#" . 'bt/noop) ("$" . 'bt/noop) ("%" . 'bt/noop) ("^" . 'bt/noop) ("&" . 'bt/match-again) ("*" . 'bt/match-symbol) ("(" . 'bt/start-macro) (")" . 'bt/end-macro)
   ("1" . 'bt/noop) ("2" . 'bt/noop) ("3" . 'bt/noop) ("4" . 'bt/noop) ("5" . 'bt/noop) ("6" . 'bt/noop) ("7" . 'bt/noop) ("8" . 'bt/noop) ("9" . 'bt/noop) ("0" . 'bt/noop)

   ("~" . 'bt/fold) ("`" . 'bt/unfold)
   ("_" . 'bt/noop) ("-" . 'bt/noop)
   ("=" . 'bt/noop) ("+" . 'bt/noop)

   ("q" . 'bt/query) ("Q" . 'bt/query-exact)
                     ("W" . 'bt/wipe-line)
   ("b" . 'bt/bw-wipe-word) ("B" . 'bt/noop)
   ("p" . 'bt/paste-fmt) ("P" . 'bt/paste-raw)
   ("f" . 'bt/wipe-word) ("F" . 'bt/noop)

   ("a" . 'bt/add) ("A" . 'bt/add-bov)
   ("r" . 'bt/revert) ("R" . 'bt/noop)
   ("s" . 'bt/select) ("S" . 'bt/select-line)
                     ("T" . 'bt/trade-line)
   ("g" . 'bt/graft) ("G" . 'bt/noop)

   ("x" . 'bt/exchange) ("X" . 'bt/exchange-exact)
                        ("C" . 'bt/copy-line)
                        ("D" . 'bt/dupe-line)
   ("v" . 'bt/vanish) ("V" . 'bt/vanish-line)

   ("j" . 'bt/join-line-below) ("J" . 'bt/join-line-above)

   ("z" . 'bt/bw-until) ("Z" . 'bt/bw-past)

   ("l" . 'bt/bw-bow) ("L" . 'bt/bw-boW)
   ("u" . 'bt/until) ("U" . 'bt/past)
   ("y" . 'bt/bow) ("Y" . 'bt/boW)
   (":" . 'bt/mark-set) (";" . 'bt/mark-jump)
   ("{" . 'bt/bol) ("[" . 'bt/cursor-to-bottom)
   ("}" . 'bt/eol) ("]" . 'bt/cursor-to-top)
                   ("\\" .'bt/cursor-to-middle)
   ("|t" . 'bt/rect-trade)
   ("|w" . 'bt/rect-wipe)
   ("|p" . 'bt/rect-paste)

   ("h" . 'bt/left)
   ("n" . 'bt/down)
   ("e" . 'bt/up)
   ("i" . 'bt/right)
   ("o" . 'bt/eow) ("O" . 'bt/eoW)
   ("'" . 'bt/comment) ("\"" . 'bt/comment-par)

   ("H" . 'bt/bw-drag)
   ("I" . 'bt/drag)
   ("E" . 'bt/bw-drag-line)
   ("N" . 'bt/drag-line)
   ;; ("L" . 'bt/bw-drag-word)
   ;; ("Y" . 'bt/drag-word)

   ("k" . 'bt/bov) ("K" . 'bt/bov)
   ("m" . 'bt/match) ("M" . 'bt/match-multi-all)
   ("," . 'bt/bop) ("<" . 'bt/bob)
   ("." . 'bt/eop) (">" . 'bt/eob)
   ("/" . 'bt/help) ("?" . 'bt/help)

   ;; :map global-map

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
