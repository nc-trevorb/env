;; -*- lexical-binding: t; -*-

;;; basic keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(use-package modalka
  :init
  (defun bt/noop () (interactive) )
  (defun bt/select () (interactive) )

  (defun bt/insert () (interactive) (modalka-mode -1))
  (defun bt/normal () (interactive) (modalka-mode) (deactivate-mark) (save-buffer))
  (defun bt/normal-without-save () (interactive) (modalka-mode))

  (defun bt/normal-keyboard-quit ()
    (interactive)
    (bt/normal-without-save)
    (keyboard-quit))

  (defmacro bt/defun-insert (fn original)
    `(defun ,fn ()
       (interactive)
       (,original)
       (bt/insert)))

  (defmacro bt/defun-insert-fn (fn body)
    `(defun ,fn ()
       (interactive)
       ,body
       (bt/insert)))

  (defun bt/copy () (whole-line-or-region-kill-ring-save 1))

  (defun bt/comment () (whole-line-or-region-comment-dwim nil))

  (defun bt/kill () (whole-line-or-region-kill-region 1))
  (defun bt/kill-to-bol () (kill-line 0))
  (defalias 'bt/kill-to-eol 'kill-line)
  (bt/defun-insert bt/update bt/kill)
  (bt/defun-insert bt/update-to-eol bt/kill-to-eol)
  (bt/defun-insert bt/update-to-bol bt/kill-to-bol)

  (bt/defun-insert-fn bt/insert-after (if (not (eolp)) (forward-char)))
  (bt/defun-insert bt/insert-at-eol end-of-line)

  (defalias 'bt/paste 'yank)

  (defun bt/paste-and-indent ()
    (interactive)
    (bt/paste)
    (call-interactively 'indent-region))

  (defun bt/duplicate ()
    (bt/copy)
    (bt/paste-and-indent))

  (defun bt/replace-with-char (c)
    (interactive "creplace with: ")
    (bt/delete)
    (insert c)
    (backward-char))

  (defun bt/replace-with-str (s)
    (interactive "sreplace with: ")
    (bt/delete)
    (insert s))

  (defun bt/comment-paragraph ()
    (interactive)
    (mark-paragraph)
    (bt/comment))

  (defun bt/delete ()
    (interactive)
    (if (region-active-p)
        (progn (bt/kill) (pop kill-ring))
      (delete-char 1)))

  (bt/defun-insert bt/substitute bt/delete)
  ;; (bt/defun-insert-fn bt/insert-on-next-line (end-of-line) (newline-and-indent))

  (defun bt/insert-on-previous-line ()
    (interactive)
    (previous-line)
    (bt/insert-on-next-line))

  (bt/defun-insert bt/insert-at-indentation back-to-indentation)

  ;; (bt/defun-insert-fn bt/replace-line
  ;;   (beginning-of-line)
  ;;   (if (not (eolp)) (kill-line))
  ;;   (indent-for-tab-command))

  (defun bt/select-line ()
    (interactive)
    (beginning-of-line)
    (set-mark-command nil)
    (next-line)
    (beginning-of-line))

  (defun bt/join-line-below ()
    (interactive)
    (next-line)
    (delete-indentation))

  ;; directional keys
  (defun bt/select-target-region (target-key)
    (cond
     ((string= target-key " ") nil) ;; region is already selected

     ((string= target-key "l")
      (move-beginning-of-line 1)
      (set-mark-command nil)
      (move-end-of-line 1)
      (forward-char))))


  (defun bt/define-target-key (input-key target-key action)
    (let ((key-seq (concat input-key target-key)))
      (define-key modalka-mode-map key-seq (lambda () (interactive)
                                             (bt/select-target-region target-key)
                                             (funcall action)
                                             (exchange-point-and-mark)))))

  (defun bt/select-in-around-region (i-or-a target-key region-start)
    (cond
     ((string= target-key "w") (er/expand-region 1))

     ((string= target-key "W")
      (re-search-backward "[[:space:]]\\|\n")
      (forward-char)
      (set-mark-command nil)
      (re-search-forward "[[:space:]]\\|\n")
      (if (string= i-or-a "i") (backward-char)))

     ((string= target-key "p")
      (mark-paragraph)
      (forward-char)
      (exchange-point-and-mark)
      (if (string= i-or-a "a") (forward-char)))

     ((string= i-or-a "i")
      (search-backward region-start)
      (er/expand-region 1)
      (forward-char)
      (exchange-point-and-mark)
      (backward-char))

     ((string= i-or-a "a")
      (search-backward region-start)
      (er/expand-region 1))))

  (defun bt/define-in-around-key (input-key i-or-a region-start-char action target-key)
    (let ((key-seq (concat input-key i-or-a target-key)))
      (define-key modalka-mode-map key-seq (lambda () (interactive)
                                             (bt/select-in-around-region i-or-a target-key region-start-char)
                                             (funcall action)
                                             (exchange-point-and-mark)))))

  (defun bt/define-until-key (cmd-key key action search)
    (define-key modalka-mode-map (concat cmd-key key) (lambda (target) (interactive "c")
                                                        (set-mark-command nil)
                                                        (funcall search target)
                                                        (funcall action)
                                                        (exchange-point-and-mark))))

  (defun bt/define-until-keys (key action)
    (bt/define-until-key key "t" action (lambda (target) (search-forward (string target)) (backward-char)))
    (bt/define-until-key key "T" action (lambda (target) (search-backward (string target)) (forward-char)))
    (bt/define-until-key key "f" action (lambda (target) (search-forward (string target))))
    (bt/define-until-key key "F" action (lambda (target) (search-backward (string target)))))

  (defun bt/define-in-around-targets (i-or-a key action pair)
    (mapcar (apply-partially 'bt/define-in-around-key key i-or-a (cdr pair) action) (split-string (car pair) "" 'f)))

  (defun bt/define-prefix-action (key action)
    (let ((region-targets '(("b()" . "(") ("B{}" . "{") ("[]" . "[") ("p" . "p") ("w" . "w") ("W" . "W") ("s'" . "'") ("S\"" . "\""))))
      (bt/define-until-keys key action)
      (bt/define-target-key key " " action)
      (bt/define-target-key key "l" action)
      (mapc (apply-partially 'bt/define-in-around-targets "i" key action) region-targets)
      (mapc (apply-partially 'bt/define-in-around-targets "a" key action) region-targets)))

  ;; remapped in iterm or macos
  ;; ("actual key pressed" . "what emacs sees")
  ;; ("C-;" . "M-#")
  ;; ("C-:" . "C-\\")
  ;; ("C-'" . "C-^")
  ;; ("C-/" . "C-_")
  ;; ("C-," . "M-{")
  ;; ("C-." . "M-}")
  ;; ("C-w" . "C-M-h")
  :bind
  (:map my-keys-minor-mode-map
   ("C-t" . 'universal-argument)
   ("M-g M-g" . 'bt/git) ;("C-g" . 'bt/normal-keyboard-quit)
   ("M-SPC" . 'bt/noop) ("C-@" . 'bt/normal)
   ("M-RET" . 'bt/insert-on-previous-line)

   ("M-q" . 'bt/noop) ("C-q" . 'bt/noop)
   ("M-w" . 'bt/noop) ("C-M-h" . 'backward-kill-word)
   ("M-b" . 'bt/noop) ("C-b" . 'backward-word)
   ("M-p" . 'helm-show-kill-ring) ("C-p" . 'bt/paste-and-indent) ("M-C-p" . 'bt/paste)
   ("M-f" . 'bt/noop) ("C-f" . 'forward-word) ("M-C-f" . 'bt/noop)

   ("M-a" . 'bt/noop) ("C-a" . 'back-to-indentation)
   ("M-r" . 'bt/noop) ("C-r" . 'isearch-backward-regexp)
   ("M-s" . 'bt/noop) ("C-s" . 'isearch-forward-regexp)
   ("M-t" . 'bt/noop) ("C-t" . 'bt/noop)
   ;; ("M-g" . 'bt/noop) ("C-g" . 'bt/noop)

   ("M-x" . 'helm-M-x) ;; ("C-x" . 'bt/noop)
   ("M-c" . 'bt/noop) ;; ("C-c" . 'bt/noop)
   ("M-d" . 'backward-delete-char-untabify) ("C-d" . 'delete-char)
   ("M-v" . 'bt/noop) ("C-v" . 'set-mark-command)

   ("M-l" . 'bt/noop) ("C-l" . 'recenter-top-bottom)
   ("M-u" . 'bt/update-to-bol) ("C-u" . 'bt/update-to-eol)
   ("M-y" . 'bt/noop) ("C-y" . 'forward-to-word)
   ;; ("M-:" . 'bt/noop) ("C-:" . 'bt/noop)

   ("M-h" . 'move-beginning-of-line) ("C-h" . 'backward-char)
   ("M-n" . 'bt/noop) ("C-n" . 'next-line)
   ("M-e" . 'bt/noop) ("C-e" . 'previous-line)
   ("M-i" . 'move-end-of-line) ("C-i" . 'forward-char)
   ("M-o" . 'indent-for-tab-command) ;; ("C-o" . 'bt/noop)
   ("M-'" . 'bt/noop) ("C-^" . 'whole-line-or-region-comment-dwim)

   ("M-k" . 'bt/kill-to-bol) ("C-k" . 'bt/kill-to-eol)
   ("M-m" . 'bt/noop)
   ("M-," . 'bt/noop) ("M-{" . 'backward-paragraph)
   ("M-." . 'bt/noop) ("M-}" . 'forward-paragraph)
   ("M-/" . 'bt/noop) ("C-_" . 'undo)

   :map minibuffer-local-completion-map
   ("C-o" . 'minibuffer-complete)

   :map global-map
   ("C-o" . 'hippie-expand)

   :map modalka-mode-map
   ("DEL" . 'bt/noop)
   ("SPC" . 'set-mark-command)
   ("RET" . 'bt/insert-on-next-line)
   ("?" . 'help-command)

   ("!" . 'bt/noop) ("@" . 'bt/noop) ("#" . 'bt/noop) ("$" . 'bt/noop) ("%" . 'bt/noop) ("^" . 'bt/noop) ("&" . 'bt/noop) ("*" . 'bt/noop) ("(" . 'bt/noop) (")" . 'bt/noop)
   ("1" . 'bt/noop) ("2" . 'bt/noop) ("3" . 'bt/noop) ("4" . 'bt/noop) ("5" . 'bt/noop) ("6" . 'bt/noop) ("7" . 'bt/noop) ("8" . 'bt/noop) ("9" . 'bt/noop) ("0" . 'bt/noop)

   ("_" . 'bt/noop) ("-" . 'bt/noop)
   ("~" . 'bt/noop) ("`" . 'bt/noop)
   ("+" . 'bt/noop) ("=" . 'bt/noop)

   ("q" . 'bt/noop) ("Q" . 'bt/noop)
   ("w" . 'backward-kill-word) ("W" . 'bt/noop)
   ("b" . 'backward-word) ("B" . 'bt/noop)
   ("p" . 'bt/paste-and-indent) ("P" . 'bt/paste)
   ("f" . 'forward-word) ("F" . 'bt/noop)

   ("a" . 'bt/insert) ("A" . 'bt/insert-at-indentation)
   ("r" . 'bt/replace-with-char) ("R" . 'bt/replace-with-str)
   ("s" . 'bt/substitute) ("S" . 'bt/replace-line)
   ("t" . 'bt/noop) ("T" . 'bt/noop)

   ("x" . 'bt/delete) ("X" . 'bt/delete)
   ;; ("c" . 'bt/directional-prefix)
   ("C" . 'bt/comment-paragraph)
   ;; ("d" . 'bt/directional-prefix)
   ("D" . 'bt/noop)
   ;; ("v" . 'bt/directional-prefix)
   ("V" . 'bt/select-line)

   ("gg" . 'goto-line) ("G" . 'bt/noop)

   ("ga" . 'back-to-indentation)
   ("gh" . 'move-beginning-of-line)
   ("gn" . 'end-of-buffer)
   ("ge" . 'beginning-of-buffer)
   ("gi" . 'move-end-of-line)
   ("gwh" . 'windmove-left)
   ("gwn" . 'windmove-down)
   ("gwe" . 'windmove-up)
   ("gwi" . 'windmove-right)
   ("gs" . 'exchange-point-and-mark)

   ("l" . 'bt/noop) ("L" . 'bt/noop)
   ;; ("u" . 'bt/directional-prefix)
   ("U" . 'bt/update-to-eol)
   ("y" . 'forward-to-word) ("Y" . 'bt/noop)
   (":" . 'bt/noop) (";" . 'bt/noop)
   ("{" . 'bt/noop) ("[" . 'bt/noop)
   ("}" . 'bt/noop) ("]" . 'bt/noop)
   ("|" . 'bt/noop) ("\\" . 'bt/noop)

   ("h" . 'backward-char) ("H" . 'bt/noop)
   ("n" . 'next-line) ("N" . 'bt/noop)
   ("e" . 'previous-line) ("E" . 'bt/noop)
   ("i" . 'forward-char) ("I" . 'bt/noop)
   ("o" . 'bt/insert-after) ("O" . 'bt/insert-at-eol)
   ("'" . 'bt/noop) ("\"" . 'bt/noop)

   ;; ("k" . 'bt/directional-prefix)
   ("K" . 'bt/noop)
   ("m" . 'bt/noop) ("M" . 'bt/noop)
   ("," . 'backward-paragraph) ("<" . 'bt/noop)
   ("." . 'forward-paragraph) (">" . 'bt/noop)
   ("/" . 'bt/noop)

   ("j" . 'bt/join-line-below) ("J" . 'bt/noop)
   ("z" . 'bt/noop) ("Z" . 'bt/noop))

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

  (bt/define-prefix-action "c" 'bt/copy)
  (bt/define-prefix-action "d" 'bt/duplicate)
  (bt/define-prefix-action "k" 'bt/kill)
  (bt/define-prefix-action "u" 'bt/update)
  (bt/define-prefix-action "v" 'bt/select)
  )
