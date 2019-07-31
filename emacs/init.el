(progn
;; initial setup
 ;;; packages
  (require 'package)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize)

 ;;; custom theme
  (add-to-list 'custom-theme-load-path "~/.emacs.d/bttheme/")
  (load-theme 'btcolor t)

 ;;; disable menu/toolbar/scrollbar
  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

 ;;; define minor mode for my keys
  (defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

  (define-minor-mode my-keys-minor-mode
    "minor mode with all my keys"
    t "" 'my-keys-minor-mode-map)
  (my-keys-minor-mode 1)
  (defadvice load (after give-my-keybindings-priority)
    "Try to ensure that my keybindings always have priority."
    (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
        (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
          (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
          (add-to-list 'minor-mode-map-alist mykeys))))
  (ad-activate 'load)

 ;;; base settings
  (setq-default auto-save-default nil)                    ;; no autosave
  (setq make-backup-files nil)                            ;; no autosave
  (setq backup-directory-alist `(("." . "~/.saves")))     ;; save backups in separate directory
  (setq-default indent-tabs-mode nil)                     ;; use spaces instead of tabs
  (show-paren-mode 1)                                     ;; highlight matching parens
  (fset 'yes-or-no-p 'y-or-n-p)                           ;; faster prompts
  (setq confirm-nonexistent-file-or-buffer nil)           ;; don't prompt to create a new file
  (define-key global-map (kbd "RET") 'newline-and-indent) ;; indent after pressing 'enter'
  (setq-default auto-compression-mode 0)                  ;; don't try to decompress files (like ~/.z)
  (setq-default fill-column 100)                          ;; text width for fill-paragraph
  (column-number-mode t)                                  ;; show column number
  (set-default 'truncate-lines t)                         ;; disable word wrap
  (winner-mode 1)                                         ;; undo/redo split layout changes
  (setq echo-keystrokes 0.001)                            ;; like vim's showcmd
  (setq require-final-newline nil)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (global-auto-revert-mode)
  (setq set-mark-command-repeat-pop t)

 ;;; startup settings
  (setq initial-major-mode 'text-mode)
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message (concat "# " (replace-regexp-in-string " (.*\n.*" "" (emacs-version)) "\n\n"))

;; packages
  (use-package projectile
    :config
    (projectile-global-mode)
    (setq projectile-globally-ignored-directories '("node_modules" "app/assets" "tmp" "vendor" "elpa" "zsh/zsh-syntax-highlighting"))
    (setq projectile-globally-ignored-file-suffixes '(".png" ".gif" ".pdf")))

  (use-package helm-config :config (helm-mode 1))

  (use-package helm-projectile
    :config
    (helm-projectile-on)

    ;; make helm always open in split window at the bottom
    (setq helm-split-window-in-side-p t)
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*helm" (* not-newline) "*" eos)
                   (display-buffer-in-side-window)
                   (inhibit-same-window . t)
                   (window-height . 0.4))))

  (use-package linum
    :config
    (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?║))
    (setq linum-format "%4d│")
    (add-hook 'prog-mode-hook #'linum-mode))

  (use-package hideshow
    :config
    (defun so/set-selective-display-dlw (&optional level)
      "Fold text indented same of more than the cursor.
     If level is set, set the indent level to LEVEL.
     If 'selective-display' is already set to LEVEL, clicking
     F5 again will unset 'selective-display' by setting it to 0."
      (interactive "P")
      (back-to-indentation)
      (if (eq selective-display (1+ (current-column)))
          (set-selective-display 0)
        (set-selective-display (or level (1+ (current-column))))))

    (add-to-list 'hs-special-modes-alist
                 '(nxml-mode
                   "<!--\\|<[^/>]*[^/]>"
                   "-->\\|</[^/>]*[^/]>"

                   "<!--"
                   sgml-skip-tag-forward
                   nil))

    (define-key my-keys-minor-mode-map (kbd "C-M-f") 'so/set-selective-display-dlw)
    (define-key my-keys-minor-mode-map (kbd "C-M-u") '(lambda () (interactive) (so/set-selective-display-dlw 0))))

  (use-package uniquify :config (setq uniquify-buffer-name-style 'forward))

  (use-package whole-line-or-region :config (whole-line-or-region-mode))

  (use-package expand-region :config (setq expand-region-fast-keys-enabled nil))

  ;; deferred packages
  (use-package cc-mode :defer t
    :config
    (setq-default c-basic-offset 8))

  (use-package css-mode :defer t
    :config
    (setq-default scss-compile-at-save nil))

  (use-package web-mode :defer t
    :config
    (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
    (setq web-mode-markup-indent-offset 2) ;; html indent
    (setq web-mode-code-indent-offset 2)) ;; js indent

  (use-package sh-script :defer t
    :config
    (add-to-list 'auto-mode-alist '("zshrc" . shell-script-mode)))

  (use-package python :defer t
    :config
    (add-to-list 'auto-mode-alist '("\\.j2$" . jinja2-mode))

    (defun bt/python-repl ()
      (interactive)
      (insert "import IPython")
      (newline-and-indent)
      (insert "IPython.embed()")
      (save-buffer))

    (define-key my-keys-minor-mode-map (kbd "C-c >") 'bt/python-repl))

  (use-package ruby-mode :defer t
    :config
    (setq ruby-insert-encoding-magic-comment nil)
    (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

    (mapc (lambda (ext) (add-to-list 'auto-mode-alist '(ext . ruby-mode)))
          '("\\.rb$" "Gemfile$" "pryrc$" "\\.rake$" "Rakefile$" "\\.gemspec$"
            "\\.ru$" "Guardfile$" "Vagrantfile$")))

  (use-package tuareg :defer t
    :config
    ;; merlin setup w/opam
    (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
      (when (and opam-share (file-directory-p opam-share))
        ;; Register Merlin
        (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
        (autoload 'merlin-mode "merlin" nil t nil)
        ;; Automatically start it in OCaml buffers
        (add-hook 'tuareg-mode-hook 'merlin-mode t)
        (add-hook 'caml-mode-hook 'merlin-mode t)
        ;; Use opam switch to lookup ocamlmerlin binary
        (setq merlin-command 'opam)))

    ;; menhir/ocamllex
    (add-to-list 'auto-mode-alist '("\\.mll\\'" . tuareg-mode))
    (add-to-list 'auto-mode-alist '("\\.mly\\'" . tuareg-mode))

    (define-key my-keys-minor-mode-map (kbd "C-M-e") 'merlin-error-prev)
    (define-key my-keys-minor-mode-map (kbd "M-e") 'merlin-error-next)

    (define-key my-keys-minor-mode-map (kbd "M-t") 'merlin-type-enclosing))

  (defun bt/git ()
    (interactive)
    (magit-status)
    (delete-other-windows)
    (magit-process-buffer)
    (other-window 1))

  (use-package magit :defer t
    :config
    (defalias 'blame 'magit-blame)

    (advice-add 'magit-status :before #'delete-other-windows)
    (setq magit-refs-sections-hook
          '(magit-insert-error-header
            magit-insert-branch-description
            magit-insert-local-branches
            magit-insert-tags)))

  (use-package org :defer t
    :init
    (message "org init")
    (defun bt/org-update-all-statistics ()
      (interactive)
      (let ((current-prefix-arg 4)) ;; emulate C-u
        (call-interactively 'org-update-statistics-cookies)))

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
      (call-interactively 'org-time-stamp))

    (defun bt/org-insert-date ()
      (interactive)
      (bt/org-insert-datetime)     ;; <2019-03-17 Sun 13:42>|
      (backward-char)              ;; <2019-03-17 Sun 13:42|>
      (org-delete-backward-char 6) ;; <2019-03-17 Sun|>
      (forward-char))               ;; <2019-03-17 Sun>|

    (defun bt/org-beginning-of-line ()
      (interactive)
      (beginning-of-line)
      (search-forward " "))

    (defun bt/org-insert-todo ()
      (interactive)
      (let ((current-prefix-arg 4)) ;; emulate C-u
        (call-interactively 'org-meta-return))
      (org-todo "{ }"))

    (defun bt/org-add-todo-counter ()
      (interactive)
      (bt/org-beginning-of-line)
      (org-todo "")
      (insert " [0/0]")
      (org-ctrl-c-ctrl-c)
      (org-todo "{ }"))

    (defun bt/org-journaling-on ()
      (interactive)
      (define-key my-keys-minor-mode-map (kbd "M-RET") 'bt/org-journal-entry))

    (defun insert-checkbox-item-on-next-line ()
      (interactive)
      (end-of-line)
      (insert "\n - [ ] "))

    (defun insert-checkbox-item ()
      (interactive)
      (beginning-of-line)
      (insert " - [ ] "))

    (defun current-line-empty-p ()
      (save-excursion
        (beginning-of-line)
        (looking-at "[[:space:]]*$")))

    (defun dwim-insert-checkbox-item ()
      (interactive)
      (if (current-line-empty-p)
          (insert-checkbox-item)
        (insert-checkbox-item-on-next-line)))

    ;; iterm remaps C-; to M-#
    :bind (:map org-mode-map
                ("M-# C-j" . 'bt/org-journal-entry)
                ("M-# D" . 'bt/org-insert-date)
                ("M-# T" . 'bt/org-insert-datetime)

                ("M-# C-n" . 'org-meta-return)
                ("M-# C-m" . 'bt/org-insert-todo)

                ("M-# C-c" . 'bt/org-add-todo-counter)
                ("M-# C-t" . (lambda () (interactive) (org-todo "{ }")))
                ("M-# C-d" . (lambda () (interactive) (org-todo "{X}")))
                ("M-# C-s" . (lambda () (interactive) (org-todo "{skip}")))
                ("M-# C-M-h" . (lambda () (interactive) (org-todo "{waiting}")))
                ("M-# C-f" . (lambda () (interactive) (org-todo "{followup}")))
                ("M-# C-SPC" . (lambda () (interactive) (org-todo "")))

                ("M-# b" . 'org-backward-heading-same-level)
                ("M-# f" . 'org-forward-heading-same-level)
                ("M-# u" . 'outline-up-heading)
                ("M-# p" . 'outline-previous-visible-heading)
                ("M-# n" . 'outline-next-visible-heading)

                ("M-# B" . 'org-promote-subtree)
                ("M-# F" . 'org-demote-subtree)
                ("M-# A" . 'org-archive-subtree)

                ("M-# C-k" . 'org-cut-subtree)
                ("M-# C-v" . 'org-cut-subtree)
                ("M-# M-v" . 'org-copy-subtree)
                ("M-# C-y" . 'org-paste-subtree)
                )


    :config
    (setq org-todo-keywords
          '((sequence "{ }" "{waiting}" "|" "{X}" "{skip}" "{followup}")))

    ;; (visual-line-mode)    ;; wrap long lines
    (setq org-startup-indented t)      ;; indent tasks and only show one star
    (setq org-catch-invisible-edits t) ;; don't allow edits to collapsed parts of a buffer
    ;; (setq org-startup-folded 'content) ;; show all headings at startup
    (setq org-enforce-todo-dependencies t) ;; can't finish a task with unfinished children
    (setq org-blank-before-new-entry nil) ;; no blank line between heading for org-metareturn
    ;; (`auto` setting wasn't working very well)

    (add-to-list 'org-structure-template-alist '("r" "#+BEGIN_SRC ruby\n?\n#+END_SRC"))
    (add-to-list 'org-structure-template-alist '("o" "#+BEGIN_SRC ocaml\n?\n#+END_SRC"))

    (add-hook 'before-save-hook 'org-align-all-tags)
    (add-hook 'before-save-hook 'bt/org-update-all-statistics)
    )

;; basic keys
 ;;; new
  ;; (global-unset-key (kbd "M-ESC ESC"))
  ;; (global-unset-key (kbd "C-x o"))
  ;; (global-unset-key (kbd "C-x C-o"))
  ;; (global-unset-key (kbd "C-x 1"))
  ;; (global-unset-key (kbd "C-x 1"))
  ;; (global-unset-key (kbd "C-x 2"))
  ;; (global-unset-key (kbd "C-x 3"))
  ;; (global-unset-key (kbd "C-x m"))
  ;; (global-unset-key (kbd "M-'"))

  (global-set-key (kbd "C-z") nil) ;; can still suspend with C-x C-z

  (define-key isearch-mode-map (kbd "C-f") 'isearch-yank-word-or-char)

  (global-unset-key (kbd "C-x h"))
  (global-unset-key (kbd "C-x n"))
  (global-unset-key (kbd "C-x e"))
  (global-unset-key (kbd "C-x i"))
  (global-unset-key (kbd "C-x C-h"))
  (global-unset-key (kbd "C-x C-n"))
  (global-unset-key (kbd "C-x C-e"))
  (global-unset-key (kbd "C-x C-i"))



  ;; need this so redo can work
  (define-key my-keys-minor-mode-map (kbd "C-/") 'undo-tree-undo)

  ;; (define-key my-keys-minor-mode-map (kbd "C-x C-f")   'find-file)
  (define-key my-keys-minor-mode-map (kbd "C-x z")     'delete-other-windows)
  (define-key my-keys-minor-mode-map (kbd "C-x /")     'winner-undo)
  (define-key my-keys-minor-mode-map (kbd "C-x \\")    'split-window-right)
  (define-key my-keys-minor-mode-map (kbd "C-x |")    'split-window-right)
  (define-key my-keys-minor-mode-map (kbd "C-x -")     'split-window-below)
  (define-key my-keys-minor-mode-map (kbd "C-x _")     'split-window-below)
  (define-key my-keys-minor-mode-map (kbd "C-x C-d")   'delete-window)
  (define-key my-keys-minor-mode-map (kbd "C-x k")   'delete-window)
  (define-key my-keys-minor-mode-map (kbd "C-x C-k")   'delete-window)
  ;; (define-key my-keys-minor-mode-map (kbd "C-x C-o")   'pop-global-mark)

  ;;(define-key my-keys-minor-mode-map (bt/kbd "C-'") 'whole-line-or-region-comment-dwim)

  (define-key my-keys-minor-mode-map (kbd "M-# q RET") 'delete-window)
  (define-key my-keys-minor-mode-map (kbd "M-# q a RET") 'save-buffers-kill-terminal)
  (define-key my-keys-minor-mode-map (kbd "M-# w")     'save-buffer)

  (define-key my-keys-minor-mode-map (kbd "M-# r i") 'string-rectangle)
  (define-key my-keys-minor-mode-map (kbd "M-# r r") 'string-rectangle)
  (define-key my-keys-minor-mode-map (kbd "M-# r k") 'kill-rectangle)
  (define-key my-keys-minor-mode-map (kbd "M-# r y") 'yank-rectangle)

  (define-key my-keys-minor-mode-map (kbd "M-# m s") 'kmacro-start-macro)
  (define-key my-keys-minor-mode-map (kbd "M-# m e") 'kmacro-end-macro)
  (define-key my-keys-minor-mode-map (kbd "M-# x") 'kmacro-end-and-call-macro)

  (define-key my-keys-minor-mode-map (kbd "M-# v") 'eval-expression)
  (define-key my-keys-minor-mode-map (kbd "M-# )") 'eval-last-sexp)

  (define-key my-keys-minor-mode-map (kbd "M-# k m") 'kmacro-to-register)
  ;; (define-key my-keys-minor-mode-map (kbd "M-# m") 'point-to-register)
  (define-key my-keys-minor-mode-map (kbd "M-# j") 'jump-to-register)

  ;; (define-key dired-mode-map (kbd "E") 'wdired-change-to-wdired-mode)

  ;; (with-eval-after-load "wdired"
  ;;   (define-key wdired-mode-map (kbd "M-# w") 'wdired-finish-edit))


  (define-key my-keys-minor-mode-map (kbd "M-# e s") 'switch-to-buffer)
  (define-key my-keys-minor-mode-map (kbd "M-# e i") 'ibuffer)
  (define-key my-keys-minor-mode-map (kbd "M-# e SPC") 'helm-projectile-find-file)
  (define-key my-keys-minor-mode-map (kbd "M-# e RET") (lambda () (interactive) (revert-buffer :ignore-auto :noconfirm)))
  (define-key my-keys-minor-mode-map (kbd "M-# e d") 'dired-jump)
  ;; (define-key my-keys-minor-mode-map (kbd "M-# e D") (lambda () (interactive) (dired (projectile-project-root))))
  ;; (define-key my-keys-minor-mode-map (kbd "M-# e f") 'find-file)

  (define-key my-keys-minor-mode-map (kbd "M-# s s")     'replace-string)
  (define-key my-keys-minor-mode-map (kbd "M-# s r")     'replace-regexp)
  (define-key my-keys-minor-mode-map (kbd "M-# s q s")   'query-replace)
  (define-key my-keys-minor-mode-map (kbd "M-# s q r")   'query-replace-regexp)

  (define-key my-keys-minor-mode-map (kbd "M-s")     'deadgrep)

  (defun bt/pbcopy ()
    (interactive)
    (shell-command-on-region (region-beginning) (region-end) "pbcopy")
    (deactivate-mark))

  (defun bt/tmux-copy ()
    (interactive)
    ;; FIXME still doesn't work for strings with "
    (shell-command
     (format-message "tmux set-buffer -b emacsclip \"%s\"" (buffer-substring (mark) (point))))
    (deactivate-mark))

;; functions
  (defun bt/show-filepaths ()
    (interactive)
    (let ((buffer-path-from-root
           (replace-regexp-in-string (regexp-quote (projectile-project-root)) "" (buffer-file-name) nil 'literal)))
      (message (concat "rel: " buffer-path-from-root "\nabs: " (buffer-file-name)))))

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

;; separate file for keys so it can use lexical-scoping
  (load "~/.emacs.d/modalka-keys.el")
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (deadgrep expand-region modalka yaml-mode whole-line-or-region web-mode use-package tuareg s rainbow-mode parent-mode org markdown-mode magit jinja2-mode helm-projectile))))
