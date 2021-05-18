(progn
;; initial setup
 ;;; packages
  (require 'package)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?║))

  (setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

  (setq smerge-command-prefix " t")

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
  (setq-default auto-compression-mode 0)                  ;; don't try to decompress files (like ~/.z)
  (setq-default fill-column 100)                          ;; text width for fill-paragraph
  (column-number-mode t)                                  ;; show column number
  (set-default 'truncate-lines t)                         ;; disable word wrap
  (winner-mode 1)                                         ;; undo/redo split layout changes
  (setq echo-keystrokes 0.001)                            ;; like vim's showcmd
  (setq vc-follow-symlinks nil)
  (setq require-final-newline nil)
  (setq-default show-trailing-whitespace t)
  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (global-auto-revert-mode)
  (setq set-mark-command-repeat-pop t)
  (setq register-preview-delay 0)
  (setq scroll-conservatively 101)
  (setq scroll-margin 2)

  (setq-default mode-line-format
                (list
                 "      "
                 '(:eval (when buffer-read-only
                           (propertize "RO "
                                       'face 'error)))
                 'mode-line-misc-info ;; for org timer

                 "%b:%l:%C " ;; buffer name : line number : column number

                 '(:eval (when (buffer-modified-p)
                           (propertize " [+]"
                                       'face 'font-lock-warning-face)))

                 " [%o] " ;; scroll percent

                 'mode-line-modes
                 ))


 ;;; startup settings

  (setq initial-major-mode 'text-mode)
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message (concat "# " (replace-regexp-in-string " (.*\n.*" "" (emacs-version)) "\n\n"))

;; packages
  (use-package projectile
    :diminish
    :config
    (projectile-global-mode)
    (setq projectile-globally-ignored-directories '("node_modules" "app/assets" "tmp" "vendor" "elpa" "zsh/zsh-syntax-highlighting"))
    (setq projectile-globally-ignored-file-suffixes '(".png" ".gif" ".pdf" ".opam.locked")))

  (use-package helm-config
    :diminish (helm-mode . "")
    :bind
    (:map helm-map
          ("M-e" . 'helm-previous-line)
          ("M-n" . 'helm-next-line)
          ("M-E" . 'previous-history-element)
          ("M-N" . 'next-history-element)
          ("M-g" . 'helm-keyboard-quit))

    :config (helm-mode 1))

  ;; message isn't showing up but at least it stops eshell from opening
  (defun eshell () (interactive) (message "no eshell!"))
  (use-package helm-projectile
    :bind
    (:map helm-projectile-find-file-map
          ("M-e" . 'helm-previous-line)
          )

    :config
    (helm-projectile-on)

    ;; make helm always open in split window at the bottom
    (setq helm-split-window-in-side-p t)
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*helm" (* not-newline) "*" eos)
                   (display-buffer-in-side-window)
                   (inhibit-same-window . t)
                   (window-height . 0.4)))

    )

  (use-package helm-swoop
    :ensure t ;; :pin melpa
    :bind
    (:map helm-swoop-map
     ("M-o" . 'helm-swoop-yank-thing-at-point)
     )

    :config
    (setq helm-swoop-split-direction 'split-window-horizontally)
    )

  (use-package whitespace
    :diminish (global-whitespace-mode . "")
    ;; http://ergoemacs.org/emacs/whitespace-mode.html
    :config
    (setq whitespace-style (quote (face trailing newline tab-mark newline-mark)))

    (setq whitespace-display-mappings
          '((newline-mark 10 [182 10]) ; LINE FEED,
            (tab-mark 9 [8677 9] [92 9]) ; tab
            ))
    (global-whitespace-mode)

    ;; not working with global-whitespace-mode, not sure why
    (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?║))
    )

  (use-package linum
    :config
    ;; (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?║))
    (setq linum-format "%4d│")
    (add-hook 'prog-mode-hook #'linum-mode))

  (use-package eldoc :diminish)
  (use-package hideshow
    :config
    (add-to-list 'hs-special-modes-alist
                 '(nxml-mode
                   "<!--\\|<[^/>]*[^/]>"
                   "-->\\|</[^/>]*[^/]>"

                   "<!--"
                   sgml-skip-tag-forward
                   nil)))

  (use-package uniquify :config (setq uniquify-buffer-name-style 'forward))
  (use-package whole-line-or-region :diminish :config (whole-line-or-region-mode))
  (use-package expand-region :config (setq expand-region-fast-keys-enabled nil))

  (use-package yasnippet
    :diminish (yas-minor-mode . "")
    :init
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode 1)
    )

  (use-package dumb-jump
    :config
    (setq dumb-jump-selector 'helm)
    (setq dumb-jump-force-searcher 'rg)
    (setq dumb-jump-rg-search-args "-S -M 500")
    (setq dumb-jump-max-find-time 4)
  ;; :ensure)
  )

  (use-package undo-tree
    :diminish
    :bind
    (:map undo-tree-visualizer-mode-map
          ("e" . 'undo-tree-visualize-undo)
          ("n" . 'undo-tree-visualize-redo)
          ("h" . 'undo-tree-visualize-switch-branch-left)
          ("i" . 'undo-tree-visualize-switch-branch-right)
          ("l" . 'undo-tree-visualize-undo-to-x)
          ("y" . 'undo-tree-visualize-redo-to-x)
          ("q" . 'undo-tree-visualizer-abort)
          ("C-g" . 'undo-tree-visualizer-abort)
          ("C-m" . 'undo-tree-visualizer-quit)
          )
    :config
    (global-undo-tree-mode)
    )

  (use-package yaml-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.yml\\.example$" . yaml-mode))
    )

  (use-package wdired)
  (use-package dired
    :bind
    (:map
     dired-mode-map
     ("e" . 'dired-previous-line)
     ("n" . 'dired-next-line)
     ("A" . 'wdired-change-to-wdired-mode)

     :map wdired-mode-map
     ("M-# C-r" . 'wdired-abort-changes)
     ("M-# M-$" . 'wdired-finish-edit))
    )


  ;; deferred packages
  (use-package cc-mode :defer t
    :config
    (setq-default c-basic-offset 8))

  (use-package css-mode :defer t
    :config
    (setq-default scss-compile-at-save nil))

  (use-package web-mode :defer t
    :init
    (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))

    :config
    (setq web-mode-markup-indent-offset 2) ;; html indent
    (setq web-mode-code-indent-offset 2) ;; js indent
    (setq js-indent-level 2) ;; js indent

    ;; single-line comments for js
    ;; (add-hook 'web-mode-hook (lambda ()
    ;;                            (setq comment-start "//")
    ;;                            (setq comment-padding " ")
    ;;                            (setq comment-end "")))
                               ;; (setq comment-style 'indent)
    )

  (use-package sh-script :defer t
    :config
    (add-to-list 'auto-mode-alist '("\\.zshrc" . shell-script-mode))
    (add-to-list 'auto-mode-alist '("^zshrc" . shell-script-mode))
    )

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
    (setq ruby-align-to-stmt-keywords t) ;; fix indentation for `if`
    ;; (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
    (add-to-list 'auto-mode-alist
                 '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
    (add-to-list 'auto-mode-alist
                 '("\\(?:Brewfile\\|Vagrantfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

    (defun bt/rspec-swoop ()
      (interactive)
      (helm-swoop :$query "describe\\ \\|context\\|\\ it\\ \\|_examples")
      )

    ;; (define-key my-keys-minor-mode-map (kbd "C-M-t") 'bt/rspec-swoop)

    )

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

    (defun bt/first-merlin-error ()
      (interactive)
      ;; FIXME figure out how to stay at excursion when pressing enter, cancel with C-g
      (save-excursion
        (bt/bob)
        (merlin-error-next)))

    (defun bt/dune-promote ()
      (interactive)
      (let ((exit-code (shell-command "dune promote")))
        (if (= 0 exit-code)
            (message "promoted"))))

    (defun bt/dune-runtest ()
      (interactive)
      (let ((exit-code (shell-command "dune runtest")))
        (if (= 0 exit-code)
            (message "%s" (propertize "tests passed" 'face 'success)))))
    ;; (message "%s" (propertize "tests passed" 'face '(:foreground "red"))))))

    (define-key my-keys-minor-mode-map (kbd "M-A") 'bt/first-merlin-error)
    (define-key my-keys-minor-mode-map (kbd "M-B") 'merlin-error-prev)
    (define-key my-keys-minor-mode-map (kbd "M-F") 'merlin-error-next)

    (define-key my-keys-minor-mode-map (kbd "M-T") 'merlin-type-enclosing)

    ;; menhir/ocamllex
    (add-to-list 'auto-mode-alist '("\\.mll\\'" . tuareg-mode))
    (add-to-list 'auto-mode-alist '("\\.mly\\'" . tuareg-mode)))

  (defun bt/git ()
    (interactive)
    (magit-status)
    (delete-other-windows)
    (magit-process-buffer)
    (other-window 1))

  ;; defer to save .5s at startup, calling bt/git will load magit
  (use-package magit :defer t
    :bind
    (:map my-keys-minor-mode-map
     ("M-# M-$" . 'with-editor-finish)
     ("M-# C-x" . 'with-editor-cancel)

     :map magit-mode-map
     ("e" . 'magit-section-backward)
     ("n" . 'magit-section-forward))

    :config
    (defalias 'blame 'magit-blame)
    (magit-auto-revert-mode)

    ;; from https://github.com/magit/magit/issues/3188#issuecomment-334135371
    (defun so/magit-submodule-update-recursive ()
      (interactive)
      (magit-run-git-async "submodule" "update" "--init" "--recursive"))

    (defun bt/magit-submodule-recursive-checkout ()
      (interactive)
      (magit-run-git-async "submodule" "foreach" "--recursive" "git" "checkout" "."))

    (defun bt/magit-update-latest-timestamp ()
      (interactive)
      (magit-run-git-async "commit" "--amend" "--no-edit" "--date" (format-time-string "%Y-%m-%d")))

    (defun bt/magit-checkout-last ()
      (interactive)
      (magit-run-git-async "checkout" "-"))

    (defun bt/magit-checkout-master ()
      (interactive)
      (magit-run-git-async "checkout" "master"))

    (defun bt/magit-rebase-master ()
      (interactive)
      (magit-run-git-async "rebase" "master"))

    (defun bt/magit-commit-no-verify ()
      (interactive)
      (magit-run-git-async "commit" "--no-verify"))
      

    (defun bt/magit-open-github-pr ()
      (interactive)
      (async-shell-command "gh pr view --web"))

    (defun bt/magit-open-github ()
      (interactive)
      (async-shell-command "gh repo view --web"))

    (defun bt/new-branch-at-HEAD ()
      (interactive)
      (magit-run-git-async "branch" "HEAD"))

    ;; (defun bt/magit-commit-without-hooks ()
    ;;   (interactive)
    ;;   (magit-run-git-async "commit" "--no-verify"))

    (advice-add 'magit-status :before #'delete-other-windows)
    (setq magit-refs-sections-hook
          '(magit-insert-error-header
            magit-insert-branch-description
            magit-insert-local-branches
            ;; magit-insert-remote-branches
            ;; magit-insert-tags
            ))

    (setq magit-revision-insert-related-refs nil)

    (transient-append-suffix 'magit-submodule "f"
      '("r" "recursive update" so/magit-submodule-update-recursive))

    (transient-append-suffix 'magit-submodule "r"
      '("c" "recursive checkout" bt/magit-submodule-recursive-checkout))

    (transient-append-suffix 'magit-branch "b"
      '("m" "master" bt/magit-checkout-master))

    (transient-append-suffix 'magit-branch "b"
      '("l" "last branch" bt/magit-checkout-last))

    (transient-append-suffix 'magit-rebase "e"
      '("m" "master" bt/magit-rebase-master))

    (transient-append-suffix 'magit-commit "S"
      '("t" "update timestamp" bt/magit-update-latest-timestamp))

    (transient-append-suffix 'magit-commit "c"
      '("n" "no hooks" bt/magit-commit-no-verify))

    ;; (transient-append-suffix 'magit-commit "c"
    ;;   '("n" "no hooks" bt/magit-commit-without-hooks))

    ;; open things in github, not submodule related ¯\_(ツ)_/¯
    (transient-append-suffix 'magit-submodule "c"
      '("g" "open github" bt/magit-open-github))

    (transient-append-suffix 'magit-submodule "g"
      '("p" "open PR in github" bt/magit-open-github-pr))

    )

  ;; (use-package forge
  ;;   :after magit
  ;;   :config
  ;;   (setq auth-sources '("~/.authinfo"))
  ;;   (setq forge-topic-list-limit '(8 . 4))
  ;;   ;; last two args "nil" "t" put section at the bottom
  ;;   (setq magit-status-sections-hook
  ;;         '(
  ;;           magit-insert-status-headers

  ;;           magit-insert-merge-log

  ;;           magit-insert-rebase-sequence
  ;;           magit-insert-am-sequence
  ;;           magit-insert-sequencer-sequence

  ;;           ;; magit-insert-bisect-output
  ;;           ;; magit-insert-bisect-rest
  ;;           ;; magit-insert-bisect-log

  ;;           magit-insert-untracked-files
  ;;           magit-insert-unstaged-changes
  ;;           magit-insert-staged-changes

  ;;           ;; magit-insert-stashes

  ;;           magit-insert-unpushed-to-pushremote
  ;;           ;; magit-insert-unpushed-to-upstream-or-recent
  ;;           ;; magit-insert-unpulled-from-pushremote
  ;;           ;; magit-insert-unpulled-from-upstream

  ;;           forge-insert-assigned-pullreqs
  ;;           forge-insert-requested-reviews
  ;;           forge-insert-authored-pullreqs
  ;;           ;; forge-insert-pullreqs
  ;;           ))

  ;;   ;; (remove-hook 'magit-status-sections-hook 'forge-insert-pullreqs)
  ;;   ;; (remove-hook 'magit-status-sections-hook 'forge-insert-issues)

  ;;   ;; (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-pullreqs nil t)
  ;;   ;; (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-requested-reviews nil t)
  ;;   ;; (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-authored-pullreqs nil t)

  ;;   )

  (use-package org :defer t
    :diminish ('org-indent-mode . "")
    :init
    (defun bt/org-update-all-statistics ()
      (interactive)
      (let ((current-prefix-arg 4)) ;; emulate C-u
        (call-interactively 'org-update-statistics-cookies)))

    (defun bt/org-todo ()
      (interactive)
      (let ((current-prefix-arg 4)) ;; emulate C-u
        (call-interactively 'org-todo)))

    (defun bt/org-journal-entry- (stars has-newline)
      (let ((header (concat
                     (if has-newline "\n" "")
                     (make-string stars ?*)
                     " ")))
        (end-of-buffer)
        (insert "\n\n")
        (delete-blank-lines)
        (insert header)
        (bt/org-insert-datetime)
        (insert " ")))

    (defun bt/org-journal-entry ()
      (interactive)
      (bt/org-journal-entry- 2 nil)
      (bt/add))

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
      (if (org-at-heading-p)
          (search-forward " ")))

    (defun bt/org-insert-heading ()
      (interactive)
      (if (bt/on-blank-line)
          (delete-char -1))
      (bt/eol)
      (org-insert-heading-after-current)
      (bt/add))

    (defun bt/org-insert-list-elt ()
      (interactive)
      (bt/eol)
      (org-meta-return)
      (bt/add))

    (defmacro bt/defun-todo-fn (name keyword)
      `(defun ,name ()
         (interactive)
         (org-todo ,keyword)))
    (put 'bt/defun-todo-fn 'lisp-indent-function 'defun)

    ;; not working: https://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Interactive.html
    ;; (defmacro bt/defun-todo-reason-fn (name keyword)
    ;;   `(defun ,name (arg)
    ;;      (interactive (concat "s" ,keyword " "))
    ;;      (org-todo ,keyword)
    ;;      (insert arg)
    ;;      (insert "]")))
    ;; (put 'bt/defun-todo-fn 'lisp-indent-function 'defun)

    (bt/defun-todo-fn bt/org-clear-todo "")
    (bt/defun-todo-fn bt/org-plan "[PLAN]")
    (bt/defun-todo-fn bt/org-todo "[TODO]")
    (bt/defun-todo-fn bt/org-wait "[WAIT]")
    (bt/defun-todo-fn bt/org-done "[DONE]")
    (bt/defun-todo-fn bt/org-skip "[SKIP]")

    (defun bt/org-reset-subtree ()
      (interactive)
      (save-excursion
        (org-mark-subtree)
        (bt/exchange-regex (regexp-quote "* [DONE]") "* [TODO]" nil (region-beginning) (region-end)))
      (save-excursion
        (org-mark-subtree)
        (bt/exchange-regex (regexp-quote "* [SKIP]") "* [TODO]" nil (region-beginning) (region-end))
        )
      (org-ctrl-c-ctrl-c)
      )

    (defun bt/org-finish ()
      (interactive)
      (save-excursion
        (bt/org-done)
        (org-archive-to-archive-sibling)))

    (defalias 'bt/org-archive-to-sibling 'org-archive-to-archive-sibling)

    (defun bt/org-create-archive ()
      (interactive)
      (bt/org-insert-child-heading)
      (insert "Archive :ARCHIVE:")
      (bt/org-insert-child-heading)
      (insert "notes")
      (bt/up)
      (so/JK-org-move-to-top)
      (bt/org-full-save)
      )

    (defun so/JK-org-move-to-top ()
      "Move current org subtree to the top of its parent so it is the first child."
      (interactive)
      (condition-case err
          (while t
            (funcall 'org-move-subtree-up))
        (user-error
         (let ((err-msg (cadr err)))
           (unless (string-match "Cannot move past superior level or buffer limit" err-msg)
             (signal 'user-error (list err-msg)))))))

    (defun bt/org-insert-todo ()
      (interactive)
      (bt/org-insert-heading)
      (bt/org-todo))

    (defun bt/org-insert-note ()
      (interactive)
      (bt/eob)
      (delete-blank-lines)
      (if (not (bt/on-blank-line))
          (progn
            (bt/eol)
            (insert "\n")))
      (bt/org-insert-datetime)
      (insert "\n")
      (bt/add))

    (defun bt/org-checklist ()
      (interactive)
      (org-todo "")
      (bt/org-beginning-of-line)
      (insert "[/] ")
      )

    (defun bt/org-full-save ()
      (interactive)
      (bt/whitespace)
      (bt/org-update-all-statistics)
      (bt/save)
      )

    (defun bt/org-insert-child-heading ()
      (interactive)
      (call-interactively 'bt/org-insert-heading)
      (org-demote-subtree))

    (defun bt/org-insert-timestamp-heading ()
      (interactive)
      (call-interactively 'bt/org-insert-heading)
      (call-interactively 'bt/org-insert-datetime))

    (defalias 'bt/org-deadline-insert 'org-deadline)
    (defalias 'bt/org-schedule-insert 'org-schedule)

    (defun bt/org-deadline-now ()
      (interactive)
      (org-back-to-heading)
      (bt/eol)
      (insert "\nDEADLINE: ")
      (call-interactively 'bt/org-insert-datetime)
      )

    (defun bt/org-schedule-now ()
      (interactive)
      (org-back-to-heading)
      (bt/eol)
      (insert "\nSCHEDULED: ")
      (call-interactively 'bt/org-insert-datetime)
      )

    (setq bt/org-hide-level 1)
    (defun bt/org-dec-hiding ()
      (interactive)
      (if (> bt/org-hide-level 1)
          (setq bt/org-hide-level (- bt/org-hide-level 1)))
      (outline-hide-sublevels bt/org-hide-level))

    (defun bt/org-inc-hiding ()
      (interactive)
      (if (< bt/org-hide-level 12)
          (setq bt/org-hide-level (+ bt/org-hide-level 1)))
      (outline-hide-sublevels bt/org-hide-level))

    (defun bt/org-reset-hiding ()
      (interactive)
      (setq bt/org-hide-level 1)
      (outline-hide-sublevels bt/org-hide-level))

    ;; (defalias 'bt/table-view 'org-columns)
    (defalias 'bt/org-agenda 'org-agenda-list)
    (defalias 'bt/clock-in 'org-clock-in)
    (defalias 'bt/clock-out 'org-clock-out)
    (defalias 'bt/clock-cancel 'org-clock-cancel)
    ;; (defalias 'bt/table-contents 'org-columns-content)
    (defun bt/refresh-table ()
      (interactive)
      (call-interactively 'org-columns-redo)
      (call-interactively 'org-columns-content)
      )

    (defun bt/org-morning ()
      (interactive)
      (org-clone-subtree-with-time-shift 1 "+1d")
      (org-forward-heading-same-level 1)
      (org-metaup)
      )

    (defun bt/org-unfinish ()
      (interactive)
      (org-clone-subtree-with-time-shift 1 "+1d")
      )

    (defun bt/table-begin ()
      (interactive)
      (bt/clock-in)
      (setq current-prefix-arg '(4))      ;; C-u
      (org-columns-todo))

    (defun bt/table-finish ()
      (interactive)
      (bt/clock-out)
      (setq current-prefix-arg '(4))      ;; C-u
      (org-columns-todo))

    (defun bt/set-todo ()
      (interactive)
      (setq current-prefix-arg '(4))      ;; C-u
      (org-columns-todo))

    (defun bt/table-view ()
      (interactive)
      (save-excursion
        (bt/bob) ;; applies to entire file, using default COLUMNS
        (call-interactively 'org-columns)
        (call-interactively 'org-columns-content))

      (bind-key "n" 'bt/down org-columns-map)
      (bind-key "e" 'bt/up org-columns-map)

      (bind-key "[" 'org-columns-previous-allowed-value org-columns-map)
      (bind-key "]" 'org-columns-next-allowed-value org-columns-map)

      (bind-key "H" 'org-columns-previous-allowed-value org-columns-map)
      (bind-key "N" 'org-columns-next-allowed-value org-columns-map)
      (bind-key "E" 'bt/up org-columns-map)
      (bind-key "I" 'bt/up org-columns-map)

      (bind-key "t" 'bt/set-todo org-columns-map)
      (bind-key "b" 'bt/table-begin org-columns-map)
      (bind-key "f" 'bt/table-finish org-columns-map)

      (bind-key "c" nil org-columns-map)
      (bind-key "cc" 'org-clock-jump-to-current-clock org-columns-map)
      (bind-key "ci" 'bt/clock-in org-columns-map)
      (bind-key "co" 'bt/clock-out org-columns-map)

      (bind-key "r" 'bt/refresh-table org-columns-map)
      (bind-key "G" 'bt/refresh-table org-columns-map)
      )

    ;; iterm remaps C-; to M-#
    :bind
    (
     :map org-mode-map
     ("C-M-h" . 'org-backward-heading-same-level)
     ("C-M-n" . 'outline-next-visible-heading)
     ("C-M-e" . 'outline-previous-visible-heading)
     ("C-M-i" . 'org-forward-heading-same-level)
     ("C-M-u" . 'outline-up-heading)

     ("M-# <f8>" . 'bt/org-full-save)

     ;; ("M-&" . 'bt/org-inc-hiding)
     ;; ("M-]" . 'bt/org-dec-hiding)

     ("M-<" . 'org-force-cycle-archived)
     ;; ("C-M-a" . 'bt/org-archive-to-sibling) ;; C-M-:
     ("C-M-l" . 'org-promote-subtree)
     ("C-M-y" . 'org-demote-subtree)

     ("C-M-o" . 'bt/org-insert-heading)
     ("C-M-_" . 'bt/org-insert-child-heading)
     ("M-^" . 'bt/org-insert-timestamp-heading) ;; C-M-return

     ;; ("C-M-f" . 'bt/org-finish)
     ;; ("C-M-c" . 'bt/org-complete)
     ;; ("C-M-p" . 'bt/org-plan)
     ("C-M-x" . 'bt/org-clear-todo)
     ("C-M-r" . 'bt/org-reset-subtree)
     ("C-M-t" . 'bt/org-todo)
     ("C-M-d" . 'bt/org-done)
     ("C-M-w" . 'bt/org-wait)
     ("C-M-s" . 'bt/org-skip)

     ("C-M-a" . 'bt/org-agenda)
     ("C-M-c" . 'bt/org-checklist)

     ("M-# M-l" . 'bt/org-journal-entry)
     ("M-# M-i M-l" . 'bt/org-journal-entry)
     ("M-# M-i M-d" . 'bt/org-insert-date)
     ("M-# M-i M-t" . 'bt/org-insert-datetime)
     ("M-# M-i M-a" . 'bt/org-create-archive)

     ;; ("M-# M-d M-i" . 'bt/org-deadline-insert)
     ;; ("M-# M-d M-n" . 'bt/org-deadline-now)
     ;; ("M-# M-s M-i" . 'bt/org-schedule-insert)
     ;; ("M-# M-s M-n" . 'bt/org-schedule-now)

     ;; ("M-# d" . 'org-deadline)
     ;; ("M-# s" . 'org-schedule)

     ;; ("M-# C-a" . 'org-agenda-list)

     ;; ("M-RET" . 'bt/org-insert-note)
     ("M-# M-n" . 'bt/org-insert-list-elt)
     ;; ("M-# M-h" . 'bt/org-insert-heading)
     ;; ("M-# M-m" . 'bt/org-insert-todo)

     ;; ("M-# M-t" . 'bt/org-todo)
     ;; ("M-# M-d" . 'bt/org-done)
     ;; ("M-# M-c" . 'bt/org-complete)
     ;; ("M-# M-p" . 'bt/org-plan)
     ;; ("M-# M-w" . 'bt/org-wait)

     ("M-# M-b" . 'bt/org-begin)
     ("M-# M-f" . 'bt/org-finish)

     ("M-# T" . 'bt/table-view)
     ("M-# c i" . 'bt/clock-in)
     ("M-# c o" . 'bt/clock-out)
     ("M-# c DEL" . 'bt/clock-cancel)

     ("M-# M-w" . 'org-cut-subtree)
     ("M-# M-c" . 'org-copy-subtree)
     ("M-# M-p" . 'org-paste-subtree)

     )

    :hook
    (org-mode . visual-line-mode)
    (org-mode . (lambda () (progn
                             ;; stopped working in :bind
                             (bind-key "k" 'bt/org-beginning-of-line modalka-mode-map)
                             (bind-key ";" 'bt/reset-org-hiding modalka-mode-map)
                             (bind-key "[" 'bt/noop modalka-mode-map)
                             (bind-key "]" 'bt/noop modalka-mode-map)
                             (bind-key "m" 'outline-previous-visible-heading modalka-mode-map)
                             (bind-key "." 'outline-next-visible-heading modalka-mode-map)
                  )))

    :config
    (setq org-todo-keywords '((sequence
                               "[PLAN]"
                               "[TODO]"
                               "[WAIT]"
                               "|"
                               "[DONE]"
                               "[SKIP]"
                               )))

    (setq org-startup-indented t)      ;; indent tasks and only show one star
    ;; (setq org-log-done 'time)          ;; timestamp when finishing a task
    (setq org-catch-invisible-edits t) ;; don't allow edits to collapsed parts of a buffer
    ;; (setq org-startup-folded 'content) ;; show all headings at startup
    (setq org-enforce-todo-dependencies t) ;; can't finish a task with unfinished children
    (setq org-blank-before-new-entry nil) ;; no blank line between heading for org-metareturn
    (setq org-hierarchical-todo-statistics nil) ;; count all children for statistics cookies

    ;; FIXME only for table mode/files
    ;; (setq org-blank-before-new-entry '((heading . 't)
    ;;                                    (plain-list-item . nil)))
    (setq org-ellipsis " [...]")
    (setq org-archive-location "~/org/archives/archive.org::* From %s")
    (setq org-agenda-files '("~/org/calendar.org"))
    (setq org-cycle-level-after-item/entry-creation nil)
    ;; (setq org-agenda-use-time-grid nil)
    (setq org-agenda-time-grid
          '((daily today require-timed remove-match)
            (
             0800
             0900
             1000
             1100
             1200
             1300
             1400
             1500
             1600
             1700
             1800
             )
            "      " "──────────────────────────"))

    ;; (setq org-agenda-files (append '("~/org/t.org")
    ;;                                (directory-files-recursively "~/org/keep" "\\.org$")
    ;;                                ))

    (setq org-highest-priority ?A) ;; A: do immediately
                                   ;; B: do today
                                   ;; C: do today if possible
    (setq org-default-priority ?D) ;; D: do next
                                   ;; E: do later
                                   ;; F: maybe someday
    (setq org-lowest-priority ?G)  ;; G: probably won't happen

    (setq org-clock-mode-line-total 'current)
    (setq org-clock-string-limit 40)
    (setq org-columns-default-format "%30ITEM(Task) %7TODO(state) %CLOCKSUM(spent) %Effort(est.){:}")
    (add-to-list 'org-global-properties
                 '("Effort_ALL" . "0:05 0:10 0:15 0:20 0:25 0:30 0:40 0:45 1:00 1:15 1:30 2:00"))

    (add-to-list 'org-structure-template-alist '("r" "#+BEGIN_SRC ruby\n?\n#+END_SRC"))
    (add-to-list 'org-structure-template-alist '("sh" "#+BEGIN_SRC sh\n?\n#+END_SRC"))
    (add-to-list 'org-structure-template-alist '("o" "#+BEGIN_SRC ocaml\n?\n#+END_SRC"))
    ;; (add-to-list 'org-structure-template-alist '("d" "DEADLINE: ?"))
    ;; (add-to-list 'org-structure-template-alist '("s" "SCHEDULED: ?"))

    ;; (add-hook 'before-save-hook 'org-align-all-tags)
    ;; using this as a hook screws up scrolling while undoing
    ;; (add-hook 'before-save-hook 'bt/org-update-all-statistics)

    ;; https://gist.githubusercontent.com/ironchicken/6b5424bc2024b3d0a58a8a130f73c2ee/raw/e397e9bc5d31633efb0f04c6c861693f2c30a7cb/clocktable-by-tag.el
    ;; (defun gist/clocktable-by-tag/shift-cell (n)
    ;;   (let ((str ""))
    ;;     (dotimes (i n)
    ;;       (setq str (concat str "| ")))
    ;;     str))

    ;; (defun gist/clocktable-by-tag/insert-tag (params)
    ;;   (let ((tag (plist-get params :tags)))
    ;;     (insert "|--\n")
    ;;     (insert (format "| %s | *Tag time* |\n" tag))
    ;;     (let ((total 0))
    ;;       (mapcar
    ;;        (lambda (file)
    ;;          (let ((clock-data (with-current-buffer (find-file-noselect file)
    ;;                              (org-clock-get-table-data (buffer-name) params))))
    ;;            (when (> (nth 1 clock-data) 0)
    ;;              (setq total (+ total (nth 1 clock-data)))
    ;;              (insert (format "| | File *%s* | %.2f |\n"
    ;;                              (file-name-nondirectory file)
    ;;                              (/ (nth 1 clock-data) 60.0)))
    ;;              (dolist (entry (nth 2 clock-data))
    ;;                (insert (format "| | . %s%s | %s %.2f |\n"
    ;;                                (org-clocktable-indent-string (nth 0 entry))
    ;;                                (nth 1 entry)
    ;;                                (clocktable-by-tag/shift-cell (nth 0 entry))
    ;;                                (/ (nth 4 entry) 60.0)))))))
    ;;        (org-agenda-files))
    ;;       (save-excursion
    ;;         (re-search-backward "*Tag time*")
    ;;         (org-table-next-field)
    ;;         (org-table-blank-field)
    ;;         (insert (format "*%.2f*" (/ total 60.0)))))
    ;;     (org-table-align)))

    ;; (defun org-dblock-write:clocktable-by-tag (params)
    ;;   (insert "| Tag | Headline | Time (h) |\n")
    ;;   (insert "|     |          | <r>  |\n")
    ;;   (let ((tags (plist-get params :tags)))
    ;;     (mapcar (lambda (tag)
    ;;               (clocktable-by-tag/insert-tag (plist-put (plist-put params :match tag) :tags tag)))
    ;;             tags)))

    )

;; basic keys
  (global-unset-key (kbd "C-x h"))
  (global-unset-key (kbd "C-x n"))
  (global-unset-key (kbd "C-x e"))
  (global-unset-key (kbd "C-x i"))
  (global-unset-key (kbd "C-x C-h"))
  (global-unset-key (kbd "C-x C-n"))
  (global-unset-key (kbd "C-x C-e"))
  (global-unset-key (kbd "C-x C-i"))
  (global-unset-key (kbd "M-`"))

  (defun bt/tmux-copy ()
    (interactive)
    ;; FIXME still doesn't work for strings with "
    (shell-command
     (format-message "tmux set-buffer -b emacsclip \"%s\"" (buffer-substring (mark) (point))))
    (deactivate-mark))


  (load "~/.emacs.d/hide-comnt.el")
  (load "~/.emacs.d/modalka-keys.el")
  (modalka-mode)
  (selected-minor-mode)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet terraform-mode dumb-jump diminish deadgrep expand-region modalka yaml-mode whole-line-or-region web-mode use-package tuareg s rainbow-mode parent-mode org markdown-mode jinja2-mode helm-projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
