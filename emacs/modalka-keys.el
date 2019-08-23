;; -*- lexical-binding: t; -*-

(use-package modalka
  :init
  (defconst whitespace "[[:space:]]\\|\n")
  (defconst non-whitespace "[[:graph:]]")
  (defalias 'bt/left 'backward-char)
  (defalias 'bt/down 'next-line)
  (defalias 'bt/up 'previous-line)
  (defalias 'bt/right 'forward-char)

  (defalias 'bt/swap 'replace-string)
  (defalias 'bt/swap-reg 'replace-regexp)
  (defalias 'bt/qswap 'query-replace)
  (defalias 'bt/qswap-reg 'query-replace-regexp)

  ;; emacs uses "up"/"down" to refer to the text in the buffer moving,
  ;; bt/ uses direction that the cursor moves
  (defalias 'bt/scroll-up 'scroll-down-line)
  (defalias 'bt/scroll-down 'scroll-up-line)
  (defalias 'bt/scroll-up-page 'scroll-down)
  (defalias 'bt/scroll-down-page 'scroll-up)

  (defalias 'bt/vsplit 'split-window-right)
  (defalias 'bt/hsplit 'split-window-below)
  (defalias 'bt/fullscreen 'delete-other-windows)
  (defalias 'bt/window-undo 'winner-undo)

  (defalias 'bt/start-macro 'kmacro-start-macro)
  (defalias 'bt/end-macro 'kmacro-end-macro)
  (defalias 'bt/run-macro 'kmacro-end-and-call-macro)

  (defalias 'bt/bw-bow 'backward-word)
  (defalias 'bt/bow 'forward-to-word)
  (defalias 'bt/eow 'forward-word)
  (defalias 'bt/bol 'beginning-of-line)
  (defalias 'bt/eol 'end-of-line)
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

  (defalias 'bt/bw-kill-word 'backward-kill-word)
  (defalias 'bt/bw-find 'isearch-backward)
  (defalias 'bt/find 'isearch-forward)
  (defalias 'bt/bw-find-reg 'isearch-backward-regexp)
  (defalias 'bt/find-reg 'isearch-forward-regexp)

  (defalias 'bt/mark-set 'point-to-register)
  (defalias 'bt/mark-jump 'jump-to-register)

  (defalias 'bt/rect-kill 'kill-rectangle)
  (defalias 'bt/rect-update 'string-rectangle)
  (defalias 'bt/rect-paste 'yank-rectangle)
  (defun bt/rect-insert ()
    (interactive)
    (let ((pcol (current-column))
          (mcol (save-excursion
                  (bt/flip-region)
                  (current-column)))
          )
      (if (= pcol mcol)
          (call-interactively 'bt/rect-update)
        (message "`ri` should only insert, use `ru` if you want to update"))))

  (defun bt/editable ()
    (and (not buffer-read-only)
         (buffer-file-name)))

  (defun bt/save ()
    (interactive)
    (if (bt/editable) (save-buffer)))

  (defalias 'bt/cancel-region 'deactivate-mark)
  (defalias 'bt/reselect 'exchange-point-and-mark)
  (defalias 'bt/flip-region 'exchange-point-and-mark)
  (defalias 'bt/autocomplete 'hippie-expand)
  (defalias 'bt/help 'help-command)
  (defalias 'bt/undo 'undo)
  (defalias 'bt/kill-ring 'helm-show-kill-ring)

  (setq bt/mark-anchor (make-marker))
  (setq bt/mark-anchor-eol (make-marker))

  (defun bt/start-region ()
    (interactive)
    (setq bt/region-line nil)
    (set-marker bt/mark-anchor (point))
    (set-mark-command nil))

  (defun bt/start-region-line ()
    (interactive)
    (bt/bol)
    (bt/start-region)
    (setq bt/region-line (line-number-at-pos))
    (bt/eol)
    (set-marker bt/mark-anchor-eol (+ 1 (point)))
    )

  (defun bt/adjust-region (anchor)
    (let* ((mark- (or (mark) 0))
           (diff (- mark- anchor)))
      (if (or (= 0 diff) (= 1 diff))
          (let ((new-mark (if (<= (point) anchor)
                              (1+ anchor)
                            anchor)))
            (set-mark new-mark)))))

  (defun bt/adjust-line-region (anchor anchor-eol)
    (if (< (line-number-at-pos) bt/region-line) ;; above region-line
        (progn
          (set-mark anchor-eol)
          (bt/bol))
      (progn
        (set-mark anchor)
        (bt/eol))))


  (defun bt/adjust-region-hook ()
    (if (region-active-p)
        (let ((anchor (marker-position bt/mark-anchor))
              (anchor-eol (marker-position bt/mark-anchor-eol)))
          (if anchor
              (if bt/region-line
                  (bt/adjust-line-region anchor anchor-eol)
                (bt/adjust-region anchor))))))


  (defmacro bt/defun-insert (fn original)
    `(defun ,fn ()
       (interactive)
       (,original)
       (bt/insert)))

  (defmacro bt/defun-insert-fn (fn &rest body)
    `(defun ,fn ()
       (interactive)
       ,@body
       (bt/insert)))

  (put 'bt/defun-insert 'lisp-indent-function 'defun)
  (put 'bt/defun-insert-fn 'lisp-indent-function 'defun)

  (defun bt/noop () (interactive) )
  (defun bt/insert () (interactive) (modalka-mode -1))
  (defun bt/normal () (interactive) (modalka-mode) (bt/cancel-region) (bt/save))
  (defun bt/normal-without-save () (interactive) (modalka-mode))

  (defun bt/exchange ()
    (interactive)
    (bt/right)
    (transpose-chars 1)
    (bt/left))

  (defun bt/bw-exchange ()
    (interactive)
    (transpose-chars 1)
    (bt/left 2))

  (defun bt/exchange-word ()
    (interactive)
    (transpose-words 1))

  (defun bt/bw-exchange-line ()
    ;; neither save-excursion nor setting a marker was working here, probably because point moves
    (interactive)
    (let ((col (current-column)))
      (transpose-lines 1)
      (bt/up 2)
      (bt/right col)))

  (defun bt/exchange-line ()
    (interactive)
    (let ((col (current-column)))
      (bt/down)
      (transpose-lines 1)
      (bt/up)
      (bt/right col)
      ))

  (defun bt/bw-exchange-word ()
    (interactive)
    (transpose-words -1)
    )

  (defun bt/past (target)
    (interactive "cpast: ")
    (search-forward (string target)))

  (defun bt/bw-past (target)
    (interactive "cbw past: ")
    (search-backward (string target)))

  (defun bt/to (target)
    (interactive "cto: ")
    (bt/past target)
    (bt/left))

  (defun bt/bw-to (target)
    (interactive "cback to: ")
    (bt/bw-past target)
    (bt/right))

  (defun bt/fold ()
    (interactive)
    (bt/bov)
    (set-selective-display (1+ (current-column))))

  (defun bt/unfold ()
    (interactive)
    (set-selective-display 0))

  (defun bt/select (target)
    (cond
     ((string= "iw" target)
      (bt/bw-bow)
      (bt/start-region)
      (bt/eow))

     ((string= "aw" target)
      (bt/select "iw")
      (bt/right))

     ((string= "aW" target)
      (re-search-backward whitespace)
      (bt/right)
      (set-mark-command nil)
      (re-search-forward whitespace))

     ((string= "iW" target)
      (bt/select "aW")
      (bt/left))

     ((string= "ip" target)
      (mark-paragraph)
      (bt/right)
      (bt/flip-region))

     ((string= "ap" target)
      (bt/select "ip")
      (bt/right))

     ((string= "il" target)
      (bt/bol)
      (bt/start-region)
      (bt/eol))

     ((string= "al" target)
      (bt/select "il")
      (bt/right))))

  (defun bt/select-eol ()
    (interactive)
    (bt/start-region)
    (bt/eol))

  (defun bt/select-line ()
    (interactive)
    (bt/select "il"))

  (defun bt/comment ()
    (interactive)
    (whole-line-or-region-comment-dwim nil))

  (defun bt/comment-par () (interactive) (bt/select "ip") (bt/comment))

  (defun bt/copy () (interactive) (whole-line-or-region-kill-ring-save 1))
  (defun bt/copy-eol ()
    (interactive)
    (bt/select-eol)
    (bt/copy))

  (defun bt/dupe ()
    (bt/copy)
    (bt/flip-region)
    (bt/paste-fmt)
    (bt/flip-region))

  (defun bt/dupe-eol ()
    (interactive)
    (bt/copy-eol)
    (bt/paste-eol))

  (defun bt/kill () (interactive) (whole-line-or-region-kill-region 1))
  (defun bt/kill-bol () (interactive) (kill-line 0))
  (defalias 'bt/kill-eol 'kill-line)
  (defalias 'bt/indent 'indent-for-tab-command)
  (defalias 'bt/go-line 'goto-line)

  (setq bt/kill-map (make-sparse-keymap))
  (define-key bt/kill-map (kbd "ip") 'bt/kill-ip)

  (defun bt/kill-ip ()
    (interactive)
    (bt/select "ip")
    (bt/kill))

  (defun bt/new-kill ()
    (interactive)
    (if (region-active-p)
        (bt/kill)
      (set-temporary-overlay-map bt/kill-map)))

  (defun bt/delete ()
    (interactive)
    (if (region-active-p)
        (progn (bt/kill) (pop kill-ring))
      (delete-char 1)))

  (defun bt/delete-eol ()
    (interactive)
    (bt/select-eol)
    (bt/delete))

  (defun bt/delete-line ()
    (interactive)
    (bt/select-line)
    (bt/delete))

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

  (defun bt/trade-char (c)
    (interactive "ctrade char: ")
    (bt/delete)
    (insert c)
    (bt/left))

  (defun bt/trade-line ()
    (interactive)
    (bt/delete-line)
    (bt/paste-fmt))

  (defun bt/repeat-command ()
    (interactive)
    (if bt/directional-key-repeat-seq
        (if bt/directional-key-repeat-arg
            (call-interactively ((key-binding bt/directional-key-repeat-seq) bt/directional-key-repeat-arg))
          (call-interactively (key-binding bt/directional-key-repeat-seq)))
      (message "no bt/directional-key-repeat-seq")))

  (defun bt/on-blank-line () (= (line-beginning-position) (line-end-position)))

  (bt/defun-insert bt/update bt/kill)
  (bt/defun-insert bt/update-eol bt/kill-eol)
  (bt/defun-insert bt/update-bol bt/kill-bol)
  (bt/defun-insert-fn bt/update-line
    (if (not (bt/on-blank-line))
        (progn
          (bt/select-line)
          (bt/kill)))
    (bt/indent))

  (bt/defun-insert bt/insert-eol bt/eol)
  (bt/defun-insert bt/insert-bov bt/bov)


  (bt/defun-insert-fn bt/insert-below (end-of-line) (newline-and-indent))
  (defun bt/insert-above ()
    (interactive)
    (bt/up)
    (bt/insert-below))

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
  (defun bt/select-target-region (target-key)
    (cond
     ((or (string= target-key " ") (string= target-key "r"))
      (if (> (point) (mark))
          (bt/right))) ;; region is already selected

     ((string= target-key "al")
      (bt/select "al"))

     ((string= target-key "il")
      (bt/select-line))))

  (defun bt/define-target-key (input-key target-key action)
    (let ((key-seq (concat input-key target-key)))
      (define-key modalka-mode-map key-seq (lambda () (interactive)
                                             (save-excursion
                                               (bt/select-target-region target-key)
                                               (funcall action))))))

  (defun bt/define-target-keys (key action)
    (bt/define-target-key key " " action)
    (bt/define-target-key key "r" action)
    (bt/define-target-key key "al" action)
    (bt/define-target-key key "il" action))

  (defun bt/select-in-around-region (i-or-a target-key region-start-char)
    (cond
     ((string= target-key "w")
      (bt/select (concat i-or-a "w")))

     ((string= target-key "W")
      (bt/select (concat i-or-a "W")))

     ((string= target-key "p")
      (bt/select (concat i-or-a "p")))

     ((string= i-or-a "i")
      (search-backward region-start-char)
      (er/expand-region 1)
      (bt/right)
      (bt/flip-region)
      (bt/left)
      (bt/flip-region))

     ((string= i-or-a "a")
      (search-backward region-start-char)
      (er/expand-region 1))))

  (defun bt/define-in-around-key (input-key i-or-a region-start-char action target-key)
    (let* ((key-seq (concat input-key i-or-a target-key))
           (run-cmd
            (lambda () (interactive)
               (bt/select-in-around-region i-or-a target-key region-start-char)
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
    (bt/define-until-key key "f" action 'bt/to)
    (bt/define-until-key key "b" action 'bt/bw-to)
    (bt/define-until-key key "F" action 'bt/past)
    (bt/define-until-key key "B" action 'bt/bw-past))

  (defun bt/define-in-around-targets (i-or-a key action pair)
    (mapcar (apply-partially 'bt/define-in-around-key key i-or-a (cdr pair) action) (split-string (car pair) "" 'f)))

  (defun bt/define-prefix-action (key action)
    (let ((region-targets '(("b()" . "(") ("B{}" . "{") ("[]" . "[") ("p" . "p") ("w" . "w") ("W" . "W") ("S'" . "'") ("s\"" . "\""))))
      (bt/define-until-keys key action)
      (bt/define-target-keys key action)
      (mapc (apply-partially 'bt/define-in-around-targets "i" key action) region-targets)
      (mapc (apply-partially 'bt/define-in-around-targets "a" key action) region-targets)))

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
  ;; ("" . "M-*")
  ;; ("" . "M-(")
  ;; ("" . "M-)")
  ;; ("" . "M-_")
  ;; ("" . "M-=")

  :bind
  (:map my-keys-minor-mode-map
   ("M-$" . 'bt/normal) ("S-<f9>" . 'bt/normal-without-save)
   ("<f7>" . 'bt/toggle-keys)
   ("M-SPC" . 'bt/start-region) ("C-@" . 'helm-M-x)
   ("M-g M-g" . 'bt/git)

   ("M-RET" . 'bt/insert-below) ("M-@" . 'bt/insert-above)

   ("C-x z" . 'bt/noop)
   ("C-M-z" . 'bt/fullscreen)
   ;; ("C-x \\" . 'split-window-right) ("C-x |" . 'split-window-right)
   ;; ("C-x -" . 'split-window-below) ("C-x _" . 'split-window-below)
   ;; ("C-x k" . 'delete-window)
   ;; ("C-x C-k" . 'delete-window)

   ("M-q" . 'bt/qswap-reg) ("C-q" . 'quit-window)
   ("M-w" . 'bt/bw-kill-word) ("C-w" . 'bt/noop)
   ("M-b" . 'bt/bw-to)       ("C-b" . 'bt/bw-find-reg) ("M-B" . 'bt/bw-past)
   ("M-p" . 'bt/paste-fmt) ("C-p" . 'bt/paste-eol) ("M-P" . 'bt/paste-raw) ("M-C-p" . 'bt/paste-line)
   ("M-f" . 'bt/to)        ("C-f" . 'bt/find-reg) ("M-F" . 'bt/past) ("M-C-f" . 'deadgrep)

   ("M-a" . 'bt/insert)   ("C-a" . 'bt/insert-eol)
   ("M-r" . 'bt/kill-ring) ("C-r" . 'bt/noop) ("M-C-r" . 'bt/noop)
   ("M-s" . 'bt/swap-reg) ("C-s" . 'bt/swap)
   ("M-t" . 'bt/trade-char) ("C-t" . 'bt/noop) ("M-C-t" . 'bt/trade-line)
   ;; ("M-g" . 'bt/noop) ("C-g" . 'bt/noop)

   ;; ("M-x" . 'bt/noop) ("C-x" . 'bt/noop)
   ("M-c" . 'bt/copy) ;("C-c" . 'bt/copy-eol) ("M-C-c" . 'bt/copy)
   ("M-d" . 'bt/dupe) ("C-d" . 'bt/dupe-eol) ("M-C-d" . 'bt/dupe)
   ("M-v" . 'bt/select) ("C-v" . 'bt/select-eol) ("M-C-v" . 'bt/select)

   ("M-z" . 'bt/delete) ("C-z" . 'bt/delete-eol) ("M-C-z" . 'bt/delete-line)
   ("M-l" . 'bt/bw-bow) ("C-l" . 'recenter-top-bottom)
   ("M-u" . 'bt/update-line) ("C-u" . 'bt/update-eol)
   ("M-y" . 'bt/bow) ("C-y" . 'bt/noop)
   ("M-:" . 'bt/noop) ("M-;" . 'bt/autocomplete)
   ("M-&" . 'bt/bol)
   ("M-]" . 'bt/eol) ("C-]" . 'bt/eol)

   ("M-h" . 'bt/left) ("C-h" . 'bt/noop) ("C-M-h" . 'windmove-left)
   ("M-n" . 'bt/down) ("C-n" . 'bt/noop) ("C-M-n" . 'windmove-down)
   ("M-e" . 'bt/up) ("C-e" . 'bt/noop) ("C-M-e" . 'windmove-up)
   ("M-i" . 'bt/right) ("M-!" . 'bt/noop) ("C-M-i" . 'windmove-right)
   ("M-o" . 'bt/eow) ("C-o" . 'universal-argument)
   ("M-'" . 'bt/comment) ("M-\"" . 'bt/comment-par) ("M-%" . 'bt/noop)

   ("M-k" . 'bt/kill) ("C-k" . 'bt/kill-eol)
   ("M-m" . 'bt/mark-set) ("M-M" . 'bt/mark-jump)
   ("M-," . 'bt/bop) ("M-{" . 'bt/noop)
   ("M-." . 'bt/eop) ("M-}" . 'bt/noop)
   ("M-/" . 'bt/help) ("C-_" . 'bt/undo)

   ("M-j" . 'bt/join-line-below) ("M-J" . 'bt/join-line-above)

   ;; :map minibuffer-local-completion-map
   ;; ("C-o" . 'minibuffer-complete)

   :map isearch-mode-map
   ("C-f" . 'isearch-repeat-forward)
   ("C-b" . 'isearch-repeat-backward)
   ("C-y" . 'isearch-yank-word-or-char)
   ("C-o" . 'isearch-yank-word-or-char)
   ("C-r" . 'bt/noop)
   ("C-s" . 'bt/noop)

   :map modalka-mode-map
   ("DEL" . 'bt/noop)
   ("RET" . 'bt/insert-below)

   ("!" . 'bt/run-macro) ("@" . 'bt/noop) ("#" . 'bt/noop) ("$" . 'bt/noop) ("%" . 'bt/noop) ("^" . 'bt/noop) ("&" . 'bt/noop) ("*" . 'bt/noop) ("(" . 'bt/start-macro) (")" . 'bt/end-macro)
   ("1" . 'bt/noop) ("2" . 'bt/noop) ("3" . 'bt/noop) ("4" . 'bt/noop) ("5" . 'bt/noop) ("6" . 'bt/noop) ("7" . 'bt/noop) ("8" . 'bt/noop) ("9" . 'bt/noop) ("0" . 'bt/noop)

   ("~" . 'bt/fullscreen) ("`" . 'bt/window-undo)
   ("_" . 'bt/noop) ("-" . 'bt/hsplit)
   ("=" . 'bt/fold) ("+" . 'bt/unfold)

   ("q" . 'bt/qswap-reg) ("Q" . 'bt/qswap)
   ("w" . 'bt/bw-kill-word) ("W" . 'bt/noop)
   ("b" . 'bt/bw-to) ("B" . 'bt/bw-past)
   ("p" . 'bt/paste-fmt) ("P" . 'bt/paste-raw)
   ("f" . 'bt/to) ("F" . 'bt/past)

   ("a" . 'bt/insert) ("A" . 'bt/insert-bov)
   ("ru" . 'bt/rect-update) ("R" . 'bt/noop)
   ("ri" . 'bt/rect-insert)
   ("rk" . 'bt/rect-kill)
   ("rp" . 'bt/rect-paste)
   ("s" . 'bt/swap-reg) ("S" . 'bt/swap)
   ("t" . 'bt/trade-char) ("T" . 'bt/noop)

   ("G" . 'bt/noop)

   ("gl" . 'bt/go-line)
   ("ga" . 'bt/bov)
   ("gs" . 'bt/reselect)
   ("gm" . 'bt/mark-jump)

   ("xh" . 'bt/bw-exchange)
   ("xi" . 'bt/exchange)
   ("xe" . 'bt/bw-exchange-line)
   ("xn" . 'bt/exchange-line)
   ("xl" . 'bt/bw-exchange-word)
   ("xy" . 'bt/exchange-word)
                          ("X" . 'bt/noop)
                          ("C" . 'bt/noop)
                          ("D" . 'bt/noop)
   ("v" . 'bt/start-region) ("V" . 'bt/start-region-line)

   ("z" . 'bt/delete) ("Z" . 'bt/noop)
   ("l" . 'bt/bw-bow) ("L" . 'bt/bw-boW)
                   ("U" . 'bt/noop)
   ("y" . 'bt/bow) ("Y" . 'bt/boW)
   (":" . 'bt/noop) (";" . 'bt/bov)
   ("{" . 'bt/bol) ("[" . 'bt/unfold)
   ("}" . 'bt/eol) ("]" . 'bt/fold)
   ("|" . 'bt/noop) ("\\" . 'bt/vsplit)

   ("h" . 'bt/left) ("H" . 'bt/scroll-up-page)
   ("n" . 'bt/down) ("N" . 'bt/scroll-down)
   ("e" . 'bt/up) ("E" . 'bt/scroll-up)
   ("i" . 'bt/right) ("I" . 'bt/scroll-down-page)
   ("o" . 'bt/eow) ("O" . 'bt/eoW)
   ("'" . 'bt/comment) ("\"" . 'bt/comment-par)

   ("k" . 'bt/new-kill) ("K" . 'bt/noop)
   ("m" . 'bt/mark-set) ("M" . 'bt/mark-jump)
   ("," . 'bt/bop) ("<" . 'bt/bob)
   ("." . 'bt/eop) (">" . 'bt/eob)
   ("/" . 'bt/undo) ("?" . 'bt/help)

   ("j" . 'bt/join-line-below) ("J" . 'bt/join-line-above)

   ;; :map global-map
   ;; ("M-C-x" . 'ctl-x-map)

   )

  :config
  (add-hook 'text-mode-hook #'modalka-mode)
  (add-hook 'prog-mode-hook #'modalka-mode)

  (setq bt/directional-key-repeat-seq nil)
  (setq bt/directional-key-repeat-arg nil)
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond (modalka-mode '("brightblack" . "green"))
                                 (t '("brightblack" . "red")))))
                (set-face-foreground 'mode-line (car color))
                (if (featurep 'linum) (set-face-foreground 'linum (cdr color)))
                (set-face-background 'mode-line (cdr color)))))

  (define-key global-map (kbd "M-x") ctl-x-map)

  (bt/define-prefix-action "c" 'bt/copy)
  (bt/define-prefix-action "d" 'bt/dupe)

  ;; (bt/define-prefix-action "k" 'bt/kill)
  (bt/define-prefix-action "u" 'bt/update)
  (bt/define-prefix-action " " 'bt/noop) ;; region stays activated so this is like "select"

  (add-hook 'post-command-hook 'bt/adjust-region-hook)
  )
