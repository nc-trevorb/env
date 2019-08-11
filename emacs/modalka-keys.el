;; -*- lexical-binding: t; -*-

(use-package modalka
  :init
  (defconst whitespace "[[:space:]]\\|\n")
  (defalias 'bt/left 'backward-char)
  (defalias 'bt/down 'next-line)
  (defalias 'bt/up 'previous-line)
  (defalias 'bt/right 'forward-char)

  (defalias 'bt/swap 'replace-string)
  (defalias 'bt/swap-reg 'replace-regexp)
  (defalias 'bt/qswap 'query-replace)
  (defalias 'bt/qswap-reg 'query-replace-regexp)

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

  (defalias 'bt/save 'save-buffer)
  (defalias 'bt/cancel-region 'deactivate-mark)
  (defalias 'bt/reselect 'exchange-point-and-mark)
  (defalias 'bt/flip-region 'exchange-point-and-mark)
  (defun bt/start-region () (interactive) (set-mark-command nil))
  (defalias 'bt/autocomplete 'hippie-expand)
  (defalias 'bt/help 'help-command)
  (defalias 'bt/undo 'undo)

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

  (defun bt/select (target)
    (cond
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

  (defun bt/comment ()
    (interactive)
    (whole-line-or-region-comment-dwim nil))

  (defun bt/comment-par () (interactive) (bt/select "ip") (bt/comment))

  (defun bt/copy () (interactive) (whole-line-or-region-kill-ring-save 1))
  (defun bt/copy-eol ()
    (interactive)
    (bt/select-eol)
    (bt/copy))

  (defun bt/kill () (interactive) (whole-line-or-region-kill-region 1))
  (defun bt/kill-bol () (interactive) (kill-line 0))
  (defalias 'bt/kill-eol 'kill-line)
  (defalias 'bt/indent 'indent-for-tab-command)
  (defalias 'bt/go-line 'goto-line)

  (defalias 'bt/paste-raw 'yank)
  (defun bt/paste-fmt ()
    (interactive)
    (bt/paste-raw)
    (call-interactively 'indent-region))

  (defun bt/paste-eol ()
    (interactive)
    (bt/eol)
    (if current-prefix-arg (insert " "))
    (bt/paste-raw))

  (defun bt/duplicate ()
    (bt/copy)
    (bt/paste-fmt))

  (defun bt/trade-char (c)
    (interactive "ctrade char: ")
    (bt/delete)
    (insert c)
    (bt/left))

  (defun bt/repeat-command ()
    (interactive)
    (if bt/directional-key-repeat-seq
        (if bt/directional-key-repeat-arg
            (call-interactively ((key-binding bt/directional-key-repeat-seq) bt/directional-key-repeat-arg))
          (call-interactively (key-binding bt/directional-key-repeat-seq)))
      (message "no bt/directional-key-repeat-seq")))

  (defun bt/delete ()
    (interactive)
    (if (region-active-p)
        (progn (bt/kill) (pop kill-ring))
      (delete-char 1)))

  (defun bt/on-blank-line () (= (line-beginning-position) (line-end-position)))

  (bt/defun-insert bt/update bt/kill)
  (bt/defun-insert bt/update-eol bt/kill-eol)
  (bt/defun-insert bt/update-bol bt/kill-bol)
  (bt/defun-insert-fn bt/update-line
    (if (not (bt/on-blank-line))
        (progn
          (bt/select "il")
          (bt/kill)))
    (bt/indent))

  (bt/defun-insert bt/insert-eol bt/eol)
  (bt/defun-insert bt/insert-bot bt/go-bot)

  (bt/defun-insert-fn bt/insert-below (end-of-line) (newline-and-indent))
  (defun bt/insert-above ()
    (interactive)
    (bt/up)
    (bt/insert-below))

  (defalias 'bt/join-line-above 'delete-indentation)
  (defun bt/join-line-below ()
    (interactive)
    (bt/down)
    (bt/join-line-above))


  ;; directional keys
  (defun bt/select-target-region (target-key)
    (cond
     ((string= target-key " ") nil) ;; region is already selected

     ((string= target-key "al")
      (bt/select "al"))

     ((string= target-key "il")
      (bt/select "il"))))

  (defun bt/define-target-key (input-key target-key action)
    (let ((key-seq (concat input-key target-key)))
      (define-key modalka-mode-map key-seq (lambda () (interactive)
                                             (bt/select-target-region (if (string= target-key "l") "al" target-key))
                                             (funcall action)
                                             (exchange-point-and-mark)))))

  (defun bt/define-barget-key (input-key target-key action)
    (let ((key-seq (concat input-key target-key)))
      (define-key modalka-mode-map key-seq (lambda () (interactive)
                                             (bt/select-target-region (if (string= target-key "l") "al" target-key))
                                             (funcall action)
                                             ))))

  (defun bt/define-target-keys (key action)
    (bt/define-target-key key " " action)
    (bt/define-target-key key "l" action)
    (bt/define-barget-key key "al" action)
    (bt/define-target-key key "il" action))

  ;; adapted from https://gist.github.com/Kungsgeten/fbb6f07b49e27dcd72b8539996891ae6
  (defun kakmacs-mark-inner (char)
    "Takes a char, like ( or \" and marks the innards of the first
  ancestor semantic unit starting with that char."
    (interactive)
    (let* ((expand-region-fast-keys-enabled nil)
           (q-char (regexp-quote char))
           (starting-point (point)))
      (er--expand-region-1)
      (er--expand-region-1)
      (while (and (not (= (point) (point-min)))
                  (not (looking-at q-char)))
        (er--expand-region-1))
      (if (not (looking-at q-char))
          (progn
            (goto-char starting-point)
            (setq mark-active nil)
            (error (concat "No match for " char)))
        (er/contract-region 1))))

  (defun kakmacs-mark-outer (char)
    "Takes a char, like ( or \" and marks the first ancestor
semantic unit starting with that char."
    (interactive)
    (let* ((expand-region-fast-keys-enabled nil)
           (q-char (regexp-quote char))
           (starting-point (point)))
      (when (looking-at q-char)
        (er/expand-region 1))
      (while (and (not (= (point) (point-min)))
                  (not (looking-at q-char)))
        (er/expand-region 1))
      (unless (looking-at q-char)
        (goto-char starting-point)
        (setq mark-active nil)
        (error (concat "No match for " char)))))

  (defun bt/select-in-around-region (i-or-a target-key region-start-char)
    (cond
     ((string= target-key "w")
      (er/expand-region 1)
      (if (string= i-or-a "a")
          (progn
            (exchange-point-and-mark)
            (bt/right))))

     ((string= target-key "W")
      (re-search-backward whitespace)
      (bt/right)
      (set-mark-command nil)
      (re-search-forward whitespace)
      (if (string= i-or-a "i") (bt/left)))

     ((string= target-key "p")
      (bt/select (concat i-or-a "p")))

     ((string= i-or-a "i")
      (kakmacs-mark-inner region-start-char))

     ((string= i-or-a "a")
      (kakmacs-mark-outer region-start-char))))

  (defun bt/define-in-around-key (input-key i-or-a region-start-char action target-key)
    (let ((key-seq (concat input-key i-or-a target-key)))
      (define-key modalka-mode-map key-seq (lambda () (interactive)
                                             (bt/select-in-around-region i-or-a target-key region-start-char)
                                             (funcall action)
                                             (exchange-point-and-mark)))))

  (defun bt/define-until-key (cmd-key key action search)
    (let ((key-seq (concat cmd-key key)))
      (define-key modalka-mode-map key-seq (lambda (target) (interactive "c")
                                             (set-mark-command nil)
                                             (funcall search target)
                                             (funcall action)
                                             (exchange-point-and-mark)))))

  (defun bt/define-until-keys (key action)
    (bt/define-until-key key "f" action 'bt/to)
    (bt/define-until-key key "b" action 'bt/bw-to)
    (bt/define-until-key key "F" action 'bt/past)
    (bt/define-until-key key "B" action 'bt/bw-past))

  (defun bt/define-in-around-targets (i-or-a key action pair)
    (mapcar (apply-partially 'bt/define-in-around-key key i-or-a (cdr pair) action) (split-string (car pair) "" 'f)))

  (defun bt/define-prefix-action (key action)
    (let ((region-targets '(("b()" . "(") ("B{}" . "{") ("[]" . "[") ("p" . "p") ("w" . "w") ("W" . "W") ("s'" . "'") ("S\"" . "\""))))
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
  ;; ("" . "M-&")
  ;; ("" . "M-*")
  ;; ("" . "M-(")
  ;; ("" . "M-)")
  ;; ("" . "M-_")
  ;; ("" . "M-=")

  :bind
  (:map my-keys-minor-mode-map
   ("M-$" . 'bt/normal) ("S-<f9>" . 'bt/normal-without-save)
   ("M-SPC" . 'bt/start-region) ("C-@" . 'helm-M-x)
   ("M-g M-g" . 'bt/git)

   ("M-RET" . 'bt/insert-below) ("M-@" . 'bt/insert-above)
   ("M-^" . 'bt/autocomplete)

   ("C-x z" . 'delete-other-windows)
   ("C-x /" . 'winner-undo)
   ("C-x \\" . 'split-window-right) ("C-x |" . 'split-window-right)
   ("C-x -" . 'split-window-below) ("C-x _" . 'split-window-below)
   ("C-x k" . 'delete-window)
   ("C-x C-k" . 'delete-window)

   ("M-q" . 'bt/qswap-reg) ("C-q" . 'bt/qswap)
   ("M-w" . 'bt/bw-kill-word) ("C-w" . 'bt/noop)
   ("M-b" . 'bt/bw-to) ("C-b" . 'bt/bw-find-reg) ("M-B" . 'bt/bw-past)
   ("M-p" . 'bt/paste-fmt) ("C-p" . 'bt/paste-eol) ("M-P" . 'bt/paste-raw) ("M-C-p" . 'helm-show-kill-ring)
   ("M-f" . 'bt/to) ("C-f" . 'bt/find-reg) ("M-F" . 'bt/past)

   ("M-a" . 'bt/insert) ("C-a" . 'bt/insert-eol)
   ("M-r" . 'bt/noop) ("C-r" . 'bt/noop)
   ("M-s" . 'bt/swap-reg) ("C-s" . 'bt/swap)
   ("M-t" . 'bt/trade-char) ("C-t" . 'bt/noop)
   ;; ("M-g" . 'bt/noop) ("C-g" . 'bt/noop)

   ;; ("M-x" . 'bt/noop) ("C-x" . 'bt/noop)
   ("M-c" . 'bt/noop) ("C-c" . 'bt/copy-eol) ("M-C-c" . 'bt/copy-line)
   ("M-d" . 'bt/noop) ("C-d" . 'bt/noop) ("M-C-d" . 'bt/dupe-line)
   ("M-v" . 'bt/start-region) ("C-v" . 'bt/select-eol) ("M-C-v" . 'bt/select-line)

   ("M-z" . 'bt/delete) ("C-z" . 'bt/delete-eol)
   ("M-l" . 'bt/bw-bow) ("C-l" . 'recenter-top-bottom)
   ("M-u" . 'bt/update-line) ("C-u" . 'bt/update-eol)
   ("M-y" . 'bt/bow) ("C-y" . 'bt/noop)
   ("M-:" . 'bt/noop) ("M-;" . 'bt/noop)
   ("M-[" . 'bt/bol)
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

   ("!" . 'bt/noop) ("@" . 'bt/noop) ("#" . 'bt/noop) ("$" . 'bt/noop) ("%" . 'bt/noop) ("^" . 'bt/noop) ("&" . 'bt/noop) ("*" . 'bt/noop) ("(" . 'bt/noop) (")" . 'bt/noop)
   ("1" . 'bt/noop) ("2" . 'bt/noop) ("3" . 'bt/noop) ("4" . 'bt/noop) ("5" . 'bt/noop) ("6" . 'bt/noop) ("7" . 'bt/noop) ("8" . 'bt/noop) ("9" . 'bt/noop) ("0" . 'bt/noop)

   ("_" . 'bt/noop) ("-" . 'bt/noop)
   ("~" . 'bt/noop) ("`" . 'bt/noop)
   ("+" . 'bt/noop) ("=" . 'bt/noop)

   ("q" . 'bt/qswap-reg) ("Q" . 'bt/qswap)
   ("w" . 'bt/bw-kill-word) ("W" . 'bt/noop)
   ("b" . 'bt/bw-to) ("B" . 'bt/bw-past)
   ("p" . 'bt/paste-fmt) ("P" . 'bt/paste-raw)
   ("f" . 'bt/to) ("F" . 'bt/past)

   ("a" . 'bt/insert) ("A" . 'bt/insert-ind)
   ("ru" . 'bt/rect-update) ("R" . 'bt/noop)
   ("rk" . 'bt/rect-kill)
   ("rp" . 'bt/rect-paste)
   ("s" . 'bt/swap-reg) ("S" . 'bt/swap)
   ("t" . 'bt/trade-char) ("T" . 'bt/noop)

   ("G" . 'bt/noop)

   ("gl" . 'bt/go-line)
   ("ga" . 'bt/bot)
   ("gs" . 'bt/reselect)
   ("gm" . 'bt/mark-jump)

   ("x" . 'bt/noop) ("X" . 'bt/noop)
                          ("C" . 'bt/noop)
                          ("D" . 'bt/noop)
   ("v" . 'bt/start-region) ("V" . 'bt/noop)

   ("z" . 'bt/delete) ("Z" . 'bt/noop)
   ("l" . 'bt/bw-bow) ("L" . 'bt/bw-boW)
                   ("U" . 'bt/noop)
   ("y" . 'bt/bow) ("Y" . 'bt/boW)
   (":" . 'bt/noop) (";" . 'bt/noop)
   ("{" . 'bt/bol) ("[" . 'bt/noop)
   ("}" . 'bt/eol) ("]" . 'bt/noop)
   ("|" . 'bt/noop) ("\\" . 'bt/noop)

   ("h" . 'bt/left) ("H" . 'bt/noop)
   ("n" . 'bt/down) ("N" . 'bt/noop)
   ("e" . 'bt/up) ("E" . 'bt/noop)
   ("i" . 'bt/right) ("I" . 'bt/noop)
   ("o" . 'bt/eow) ("O" . 'bt/eoW)
   ("'" . 'bt/comment) ("\"" . 'bt/comment-par)

                        ("K" . 'bt/noop)
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

  ;; (bt/define-prefix-action "x" 'bt/delete)
  (bt/define-prefix-action "c" 'bt/copy)
  (bt/define-prefix-action "d" 'bt/duplicate)

  (bt/define-prefix-action "k" 'bt/kill)
  (bt/define-prefix-action "u" 'bt/update)
  (bt/define-prefix-action " " 'bt/noop) ;; region stays activated so this is like "select"
  )
