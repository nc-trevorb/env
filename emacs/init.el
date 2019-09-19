(progn
;; initial setup
 ;;; packages
  (require 'package)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?║))

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
  (setq require-final-newline nil)
  (setq-default show-trailing-whitespace t)
  ;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (global-auto-revert-mode)
  (setq set-mark-command-repeat-pop t)
  (setq register-preview-delay 0)
  (setq scroll-conservatively 101)

  (setq-default mode-line-format
                (list
                 "      "
         '(:eval (when buffer-read-only
                   (propertize "RO "
                               'face 'error)))

         "%b:%l " ;; buffer name : line number

         '(:eval (when (buffer-modified-p)
                   (propertize " [+]"
                               'face 'font-lock-warning-face)))

         " [%o] " ;; mode-name

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
    (setq projectile-globally-ignored-file-suffixes '(".png" ".gif" ".pdf")))

  (use-package helm-config
    :diminish (helm-mode . "")
    :bind
    (:map helm-map
          ("M-e" . 'helm-previous-line)
          ("M-n" . 'helm-next-line)
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
                   (window-height . 0.4))))

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

  (use-package dired
    :config
    ;; not working in :bind
    (define-key dired-mode-map (kbd "U") 'wdired-change-to-wdired-mode)

    (with-eval-after-load "wdired"
      (define-key wdired-mode-map (kbd "M-# M-$") 'wdired-finish-edit)))


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
     ("M-# C-c" . 'with-editor-finish)
     ("M-# C-q" . 'with-editor-cancel)

     :map magit-mode-map
     ("e" . 'magit-section-backward)
     ("n" . 'magit-section-forward))

    :config
    (defalias 'blame 'magit-blame)
    (magit-auto-revert-mode)
    (advice-add 'magit-status :before #'delete-other-windows)
    (setq magit-refs-sections-hook
          '(magit-insert-error-header
            magit-insert-branch-description
            magit-insert-local-branches
            magit-insert-tags)))

  (use-package org :defer t
    :diminish ('org-indent-mode . "")
    :init
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

    (defun bt/org-insert-heading ()
      (interactive)
      (bt/eol)
      (org-insert-heading-after-current)
      (bt/add))

    (defun bt/org-insert-list-elt ()
      (interactive)
      (bt/eol)
      (org-meta-return)
      (bt/add))

    (defun bt/org-insert-todo ()
      (interactive)
      (bt/org-insert-heading)
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

    (defun current-line-empty-p ()
      (save-excursion
        (beginning-of-line)
        (looking-at "[[:space:]]*$")))

    (setq bt/org-hide-level 1)
    (defun bt/dec-org-hiding ()
      (interactive)
      (if (> bt/org-hide-level 1)
          (setq bt/org-hide-level (- bt/org-hide-level 1)))
      (outline-hide-sublevels bt/org-hide-level))

    (defun bt/inc-org-hiding ()
      (interactive)
      (if (< bt/org-hide-level 12)
          (setq bt/org-hide-level (+ bt/org-hide-level 1)))
      (outline-hide-sublevels bt/org-hide-level))

    (defun bt/reset-org-hiding ()
      (interactive)
      (setq bt/org-hide-level 1)
      (outline-hide-sublevels bt/org-hide-level))

    ;; iterm remaps C-; to M-#
    :bind
    (:map modalka-mode-map
     ("H" . 'org-backward-heading-same-level)
     ("I" . 'org-forward-heading-same-level)
     ("U" . 'outline-up-heading)
     ("E" . 'outline-previous-visible-heading)
     ("N" . 'outline-next-visible-heading)
     ("," . 'outline-previous-visible-heading)
     ("." . 'outline-next-visible-heading)

     ("k" . 'bt/org-beginning-of-line)

     ;; ("|" . 'outline-hide-sublevels)
     ;; ("\\" . 'bt/reset-org-hiding)
     ;; ("[" . 'bt/dec-org-hiding)
     ;; ("]" . 'bt/inc-org-hiding)

     :map org-mode-map
     ;; ("M-# C-j" . 'bt/org-journal-entry)
     ("M-# D" . 'bt/org-insert-date)
     ("M-# T" . 'bt/org-insert-datetime)

     ("M-# C-n" . 'bt/org-insert-list-elt)
     ("M-# C-h" . 'bt/org-insert-heading)
     ("M-# C-m" . 'bt/org-insert-todo)

     ("M-# C-c" . 'bt/org-add-todo-counter)
     ("M-# C-t" . (lambda () (interactive) (org-todo "{ }")))
     ("M-# C-d" . (lambda () (interactive) (org-todo "{X}")))
     ("M-# C-s" . (lambda () (interactive) (org-todo "{skip}")))
     ("M-# C-b" . (lambda () (interactive) (org-todo "{blocked}")))
     ("M-# C-f" . (lambda () (interactive) (org-todo "{followup}")))
     ("M-# C-SPC" . (lambda () (interactive) (org-todo "")))

     ("M-# C-l" . 'org-promote-subtree)
     ("M-# C-y" . 'org-demote-subtree)

     ("M-# M-# C-w" . 'org-cut-subtree)
     ("M-# M-# C-c" . 'org-copy-subtree)
     ("M-# M-# C-p" . 'org-paste-subtree)
     )

    :hook
    (org-mode . visual-line-mode)

    :config
    (setq org-todo-keywords
          '((sequence "{ }" "{blocked}" "|" "{X}" "{skip}" "{followup}")))

    (setq org-startup-indented t)      ;; indent tasks and only show one star
    (setq org-catch-invisible-edits t) ;; don't allow edits to collapsed parts of a buffer
    ;; (setq org-startup-folded 'content) ;; show all headings at startup
    (setq org-enforce-todo-dependencies t) ;; can't finish a task with unfinished children
    (setq org-blank-before-new-entry nil) ;; no blank line between heading for org-metareturn
    (setq org-ellipsis " [...]")
    (setq org-archive-location "~/org/archive.org::* From %s")

    (add-to-list 'org-structure-template-alist '("r" "#+BEGIN_SRC ruby\n?\n#+END_SRC"))
    (add-to-list 'org-structure-template-alist '("o" "#+BEGIN_SRC ocaml\n?\n#+END_SRC"))

    (add-hook 'before-save-hook 'org-align-all-tags)
    (add-hook 'before-save-hook 'bt/org-update-all-statistics)
    )

;; basic keys
 ;;; new
  ;; (global-set-key (kbd "C-z") nil) ;; can still suspend with C-x C-z


  (global-unset-key (kbd "C-x h"))
  (global-unset-key (kbd "C-x n"))
  (global-unset-key (kbd "C-x e"))
  (global-unset-key (kbd "C-x i"))
  (global-unset-key (kbd "C-x C-h"))
  (global-unset-key (kbd "C-x C-n"))
  (global-unset-key (kbd "C-x C-e"))
  (global-unset-key (kbd "C-x C-i"))
  (global-unset-key (kbd "M-`"))

  (define-key my-keys-minor-mode-map (kbd "M-# q RET") 'delete-window)
  (define-key my-keys-minor-mode-map (kbd "M-# q a RET") 'save-buffers-kill-terminal)
  ;; (define-key my-keys-minor-mode-map (kbd "M-# w")     'save-buffer)

  (define-key my-keys-minor-mode-map (kbd "M-# v") 'eval-expression)
  (define-key my-keys-minor-mode-map (kbd "M-# )") 'eval-last-sexp)

  ;; (define-key my-keys-minor-mode-map (kbd "M-# e s") 'switch-to-buffer)
  ;; (define-key my-keys-minor-mode-map (kbd "M-# e i") 'ibuffer)
  ;; (define-key my-keys-minor-mode-map (kbd "M-# e SPC") 'helm-projectile-find-file)
  ;; (define-key my-keys-minor-mode-map (kbd "M-# e RET") (lambda () (interactive) (revert-buffer :ignore-auto :noconfirm)))
  ;; (define-key my-keys-minor-mode-map (kbd "M-# e d") 'dired-jump)

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

  (load "~/.emacs.d/modalka-keys.el")
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (diminish deadgrep expand-region modalka yaml-mode whole-line-or-region web-mode use-package tuareg s rainbow-mode parent-mode org markdown-mode magit jinja2-mode helm-projectile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
