(progn
;; initial setup
 ;;; packages
  (require 'package)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  ;; (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?┃))

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
  (setq visible-cursor nil)

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

  (add-hook 'prog-mode-hook
            (lambda ()
              (display-line-numbers-mode)
              (setq display-line-numbers-width 3)
              ))

 ;;; startup settings

  (setq initial-major-mode 'text-mode)
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message (concat "# " (replace-regexp-in-string " (.*\n.*" "" (emacs-version)) "\n\n"))

;; packages
 ;;; base/global
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
          ("M-g" . 'helm-keyboard-quit)

          ("M-+" . 'helm-execute-persistent-action)

          )

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
     ("M-o" . 'helm-multi-swoop-all-from-helm-swoop)
     )

    :config
    (setq helm-swoop-split-direction 'split-window-horizontally)
    )

  (use-package deadgrep
    :init
    (defun %gitignore ()
      (concat (projectile-project-root) ".gitignore"))

    (defun %deadgrep-push-button (name)
      (interactive)
      (save-excursion
        (%bob)
        (%eol)
        (%bw-bow)
        (%left)
        (search-forward name)
        (%left)
        (call-interactively 'push-button)))

    (defun %deadgrep-dir ()
      (interactive)
      (%deadgrep-push-button "Directory: ~/"))

    (defun %deadgrep-regex-search ()
      (interactive)
      (%deadgrep-push-button "regex"))

    (defun %deadgrep-string-search ()
      (interactive)
      (%deadgrep-push-button "string"))

    (defun %deadgrep-edit-search-term ()
      (interactive)
      (%deadgrep-push-button "change"))

    (defun %deadgrep-before ()
      (interactive)
      (%deadgrep-push-button "before"))

    (defun %deadgrep-after ()
      (interactive)
      (%deadgrep-push-button "after"))

    (defun %deadgrep-toggle-tests ()
      (interactive)
      (if %deadgrep-hiding-tests
          (shell-command (concat "sed -i '' -e '$ d' " (%gitignore)))
        (shell-command (concat "echo '*test.js' >> " (%gitignore))))
      (setq %deadgrep-hiding-tests (not %deadgrep-hiding-tests))
      (deadgrep-restart))

    (setq %deadgrep-hiding-tests
          (if (string= default-directory "~/")
              nil
            (string= "*test.js" (car (last (with-temp-buffer (insert-file-contents (%gitignore))
                                                             (split-string (buffer-string) "\n" t)))))))

    (defun kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer
            (delq (current-buffer)
                  (seq-filter 'buffer-file-name (buffer-list)))))

    (defun %deadgrep-open-all-files ()
      (interactive)
      )

    :bind
    (:map deadgrep-mode-map
          ("T" . '%deadgrep-toggle-tests)
          ("D" . '%deadgrep-dir)
          ("Q" . '%deadgrep-dir)
          ("R" . '%deadgrep-regex-search)
          ("S" . '%deadgrep-string-search)
          ("B" . '%deadgrep-before)
          ("A" . '%deadgrep-after)
          ("C-M-n" . 'deadgrep-forward-filename)
          ("C-M-e" . 'deadgrep-backward-filename)
          ("{" . '%deadgrep-edit-search-term)
          )
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
    )

  ;; (use-package linum
  ;;   :config
  ;;   ;; (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?║))
  ;;   (setq linum-format "%4d│")
  ;;   (add-hook 'prog-mode-hook #'linum-mode))

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
    (setq dumb-jump-max-find-time 4))

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


 ;;; deferred packages
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

    (setq-default web-mode-comment-formats (remove '("javascript" . "/*") web-mode-comment-formats))
    (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
    (add-to-list 'web-mode-comment-formats '("jsx" . "//"))

    ;; (setq-default web-mode-comment-formats
    ;;               '(("java"       . "/*")
    ;;                 ("javascript" . "//")
    ;;                 ("php"        . "/*")))

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

    (defun %python-repl ()
      (interactive)
      (insert "import IPython")
      (newline-and-indent)
      (insert "IPython.embed()")
      (save-buffer))

    (define-key my-keys-minor-mode-map (kbd "C-c >") '%python-repl))

  (use-package enh-ruby-mode :defer t
    :init
    (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
    (add-to-list 'auto-mode-alist
                 '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . enh-ruby-mode))
    (add-to-list 'auto-mode-alist
                 '("\\(?:Brewfile\\|Vagrantfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . enh-ruby-mode))
    (remove-hook 'enh-ruby-mode-hook 'erm-define-faces)

    :config
    (setq enh-ruby-deep-indent-paren nil)
    (setq enh-ruby-deep-indent-construct nil)

    (setq enh-ruby-extra-keywords
          '("include_examples" "shared_examples"
            "include_context" "shared_context"
            "describe" "context" "it"
            "xdescribe" "xcontext" "xit"
            "fdescribe" "fcontext" "fit"
            "before" "after"
            "let" "let_it_be"
            ))
    )

  ;; (use-package ruby-mode :defer t
  ;;   :config
  ;;   (setq ruby-insert-encoding-magic-comment nil)
  ;;   (setq ruby-align-to-stmt-keywords t) ;; fix indentation for `if`

  ;;   ;; (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
  ;;   (add-to-list 'auto-mode-alist
  ;;                '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
  ;;   (add-to-list 'auto-mode-alist
  ;;                '("\\(?:Brewfile\\|Vagrantfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

  ;;   (defun %rspec-swoop ()
  ;;     (interactive)
  ;;     (helm-swoop :$query "describe\\ \\|context\\|\\ it\\ \\|_examples")
  ;;     )

    ;; (define-key my-keys-minor-mode-map (kbd "C-M-t") '%rspec-swoop)

    ;; from https://emacs.stackexchange.com/a/39450
    ;; (defun ruby-smie-rules (kind token)
    ;;   (pcase (cons kind token)
    ;;     (`(:elem . basic) ruby-indent-level)
    ;;     ;; "foo" "bar" is the concatenation of the two strings, so the second
    ;;     ;; should be aligned with the first.
    ;;     (`(:elem . args) (if (looking-at "\\s\"") 0))
    ;;     ;; (`(:after . ",") (smie-rule-separator kind))
    ;;     (`(:before . ";")
    ;;      (cond
    ;;       ((smie-rule-parent-p "def" "begin" "do" "class" "module" "for"
    ;;                            "while" "until" "unless"
    ;;                            "if" "then" "elsif" "else" "when"
    ;;                            "rescue" "ensure" "{")
    ;;        (smie-rule-parent ruby-indent-level))
    ;;       ;; For (invalid) code between switch and case.
    ;;       ;; (if (smie-parent-p "switch") 4)
    ;;       ))
    ;;     (`(:before . ,(or `"(" `"[" `"{"))
    ;;      (cond
    ;;       ((and (equal token "{")
    ;;             (not (smie-rule-prev-p "(" "{" "[" "," "=>" "=" "return" ";"))
    ;;             (save-excursion
    ;;               (forward-comment -1)
    ;;               (not (eq (preceding-char) ?:))))
    ;;        ;; Curly block opener.
    ;;        (ruby-smie--indent-to-stmt))
    ;;       ((smie-rule-hanging-p)
    ;;        ;; Treat purely syntactic block-constructs as being part of their parent,
    ;;        ;; when the opening token is hanging and the parent is not an
    ;;        ;; open-paren.
    ;;        (cond
    ;;         ((eq (car (smie-indent--parent)) t) nil)
    ;;         ;; When after `.', let's always de-indent,
    ;;         ;; because when `.' is inside the line, the
    ;;         ;; additional indentation from it looks out of place.
    ;;         ((smie-rule-parent-p ".")
    ;;          ;; Traverse up the call chain until the parent is not `.',
    ;;          ;; or `.' at indentation, or at eol.
    ;;          (while (and (not (ruby-smie--bosp))
    ;;                      (equal (nth 2 (smie-backward-sexp ".")) ".")
    ;;                      (not (ruby-smie--bosp)))
    ;;            (forward-char -1))
    ;;          (smie-indent-virtual))
    ;;         (t (smie-rule-parent))))))
    ;;     (`(:after . ,(or `"(" "[" "{"))
    ;;      ;; FIXME: Shouldn't this be the default behavior of
    ;;      ;; `smie-indent-after-keyword'?
    ;;      (save-excursion
    ;;        (smie-rule-parent)))
    ;;     (`(:before . " @ ")
    ;;      (save-excursion
    ;;        (skip-chars-forward " \t")
    ;;        (cons 'column (current-column))))
    ;;     (`(:before . "do") (ruby-smie--indent-to-stmt))
    ;;     (`(:before . ".")
    ;;      (if (smie-rule-sibling-p)
    ;;          (and ruby-align-chained-calls 0)
    ;;        (smie-backward-sexp ".")
    ;;        (cons 'column (+ (current-column)
    ;;                         ruby-indent-level))))
    ;;     (`(:before . ,(or `"else" `"then" `"elsif" `"rescue" `"ensure"))
    ;;      (smie-rule-parent))
    ;;     (`(:before . "when")
    ;;      ;; Align to the previous `when', but look up the virtual
    ;;      ;; indentation of `case'.
    ;;      (if (smie-rule-sibling-p) 0 (smie-rule-parent)))
    ;;     (`(:after . ,(or "=" "+" "-" "*" "/" "&&" "||" "%" "**" "^" "&"
    ;;                      "<=>" ">" "<" ">=" "<=" "==" "===" "!=" "<<" ">>"
    ;;                      "+=" "-=" "*=" "/=" "%=" "**=" "&=" "|=" "^=" "|"
    ;;                      "<<=" ">>=" "&&=" "||=" "and" "or"))
    ;;      (and (smie-rule-parent-p ";" nil)
    ;;           (smie-indent--hanging-p)
    ;;           ruby-indent-level))
    ;;     (`(:after . ,(or "?" ":")) ruby-indent-level)
    ;;     (`(:before . ,(guard (memq (intern-soft token) ruby-alignable-keywords)))
    ;;      (when (not (ruby--at-indentation-p))
    ;;        (if (ruby-smie--indent-to-stmt-p token)
    ;;            (ruby-smie--indent-to-stmt)
    ;;          (cons 'column (current-column)))))
    ;;     (`(:before . "iuwu-mod")
    ;;      (smie-rule-parent ruby-indent-level))
    ;;     ))
    ;; )

  (use-package tuareg :defer t
    :config
    ;; merlin setup w/opam
    (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
      (when (and opam-share (file-directory-p opam-share))
        ;; Register Merlin
        (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
        (require 'ocp-indent)
        (autoload 'merlin-mode "merlin" nil t nil)
        ;; Automatically start it in OCaml buffers
        (add-hook 'tuareg-mode-hook 'merlin-mode t)
        (add-hook 'caml-mode-hook 'merlin-mode t)
        ;; Use opam switch to lookup ocamlmerlin binary
        (setq merlin-command 'opam)))

    (defun %first-merlin-error ()
      (interactive)
      ;; FIXME figure out how to stay at excursion when pressing enter, cancel with C-g
      (save-excursion
        (%bob)
        (merlin-error-next)))

    (defun %dune-promote ()
      (interactive)
      (let ((exit-code (shell-command "dune promote")))
        (if (= 0 exit-code)
            (message "promoted"))))

    (defun %dune-runtest ()
      (interactive)
      (let ((exit-code (shell-command "dune runtest")))
        (if (= 0 exit-code)
            (message "%s" (propertize "tests passed" 'face 'success)))))
    ;; (message "%s" (propertize "tests passed" 'face '(:foreground "red"))))))

    ;; (defun %bop ()
    ;;   (interactive)
    ;;   (save-excursion
    ;;     (move-to-window-line nil)
    ;;     (recenter-top-bottom)
    ;;     (recenter-top-bottom)))

    ;; (defun %eop ()
    ;;   (interactive)
    ;;   (save-excursion
    ;;     (move-to-window-line nil)
    ;;     (recenter-top-bottom)))

    (define-key my-keys-minor-mode-map (kbd "C-M-u") '%first-merlin-error)
    (define-key my-keys-minor-mode-map (kbd "C-M-e") 'merlin-error-prev)
    (define-key my-keys-minor-mode-map (kbd "C-M-n") 'merlin-error-next)

    (define-key my-keys-minor-mode-map (kbd "C-M-t") 'merlin-type-enclosing)

    (require 'ocamlformat)
    (define-key my-keys-minor-mode-map (kbd "C-M-f") 'ocamlformat)

    ;; menhir/ocamllex
    (add-to-list 'auto-mode-alist '("\\.mll\\'" . tuareg-mode))
    (add-to-list 'auto-mode-alist '("\\.mly\\'" . tuareg-mode)))

  ;; (use-package reason-mode
  ;;   :init
  ;;   (defun shell-cmd (cmd)
  ;;     "Returns the stdout output of a shell command or nil if the command returned
  ;;  an error"
  ;;     (car (ignore-errors (apply 'process-lines (split-string cmd)))))

  ;;   (defun reason-cmd-where (cmd)
  ;;     (let ((where (shell-cmd cmd)))
  ;;       (if (not (string-equal "unknown flag ----where" where))
  ;;           where)))

  ;;   (let* ((refmt-bin (or (reason-cmd-where "refmt ----where")
  ;;                         (shell-cmd "which refmt")
  ;;                         (shell-cmd "which bsrefmt")))
  ;;          (merlin-bin (or (reason-cmd-where "ocamlmerlin ----where")
  ;;                          (shell-cmd "which ocamlmerlin")))
  ;;          (merlin-base-dir (when merlin-bin
  ;;                             (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
  ;;     ;; Add merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
  ;;     (when merlin-bin
  ;;       (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
  ;;       (setq merlin-command merlin-bin))

  ;;     (when refmt-bin
  ;;       (setq refmt-command refmt-bin)))


  ;;   ;; (require 'reason-mode)
  ;;   (require 'merlin)
  ;;   ;; merlin setup w/opam
  ;;   (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  ;;     (when (and opam-share (file-directory-p opam-share))
  ;;       ;; Register Merlin
  ;;       (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
  ;;       (autoload 'merlin-mode "merlin" nil t nil)
  ;;       ;; Automatically start it in OCaml buffers
  ;;       (add-hook 'tuareg-mode-hook 'merlin-mode t)
  ;;       (add-hook 'caml-mode-hook 'merlin-mode t)
  ;;       ;; Use opam switch to lookup ocamlmerlin binary
  ;;       (setq merlin-command 'opam)))

  ;;   (add-hook 'reason-mode-hook (lambda ()
  ;;                                 (setq indent-line-function 'indent-relative)
  ;;                                 (add-hook 'before-save-hook 'refmt-before-save)
  ;;                                 (merlin-mode)))

  ;;   (define-key my-keys-minor-mode-map (kbd "C-M-t") 'merlin-type-enclosing)
  ;;   (setq merlin-ac-setup t)
  ;;   )

  (defun %git ()
    (interactive)
    (magit-status)
    (delete-other-windows)
    (magit-process-buffer)
    (other-window 1))

 ;;; magit
  (use-package magit :defer t
    :bind
    (:map my-keys-minor-mode-map
     ("M-# M-$" . 'with-editor-finish)
     ("M-# C-x" . 'with-editor-cancel)

     :map magit-mode-map
     ("e" . 'magit-section-backward)
     ("n" . 'magit-section-forward)
     ("M-RET" . 'magit-diff-visit-worktree-file)
     )

    :config
    (defalias 'blame 'magit-blame)
    (magit-auto-revert-mode)

    ;; from https://github.com/magit/magit/issues/3188#issuecomment-334135371
    (defun so/magit-submodule-update-recursive ()
      (interactive)
      (magit-run-git-async "submodule" "update" "--init" "--recursive"))

    (defun %magit-submodule-recursive-checkout ()
      (interactive)
      (magit-run-git-async "submodule" "foreach" "--recursive" "git" "checkout" "."))

    (defun %magit-update-latest-timestamp ()
      (interactive)
      (magit-run-git-async "commit" "--amend" "--no-edit" "--date" (format-time-string "%Y-%m-%d")))

    (defun %magit-checkout-last ()
      (interactive)
      (magit-run-git-async "checkout" "-"))

    (defun %magit-checkout-main ()
      (interactive)
      (magit-run-git-async "checkout" "main"))

    (defun %magit-rebase-main ()
      (interactive)
      (magit-run-git-async "rebase" "main"))

    (defun %magit-checkout-master ()
      (interactive)
      (magit-run-git-async "checkout" "master"))

    (defun %magit-rebase-master ()
      (interactive)
      (magit-run-git-async "rebase" "master"))

    (defun %magit-commit-no-verify ()
      (interactive)
      (magit-run-git-async "commit" "--no-verify"))

    (defun %magit-open-github-pr ()
      (interactive)
      (async-shell-command "gh pr view --web"))

    (defun %magit-open-github ()
      (interactive)
      (async-shell-command "gh repo view --web"))

    (defun %new-branch-at-HEAD ()
      (interactive)
      (magit-run-git-async "branch" "HEAD"))

    ;; (defun %magit-commit-without-hooks ()
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
      '("c" "recursive checkout" %magit-submodule-recursive-checkout))

    (transient-append-suffix 'magit-branch "b"
      '("M" "master" %magit-checkout-master))

    (transient-append-suffix 'magit-branch "b"
      '("m" "main" %magit-checkout-main))

    (transient-append-suffix 'magit-branch "b"
      '("l" "last branch" %magit-checkout-last))

    (transient-append-suffix 'magit-rebase "e"
      '("M" "master" %magit-rebase-master))

    (transient-append-suffix 'magit-rebase "e"
      '("m" "main" %magit-rebase-main))

    (transient-append-suffix 'magit-commit "S"
      '("t" "update timestamp" %magit-update-latest-timestamp))

    (transient-append-suffix 'magit-commit "c"
      '("n" "no hooks" %magit-commit-no-verify))

    ;; (transient-append-suffix 'magit-commit "c"
    ;;   '("n" "no hooks" %magit-commit-without-hooks))

    ;; open things in github, not submodule related ¯\_(ツ)_/¯
    (transient-append-suffix 'magit-submodule "c"
      '("g" "open github" %magit-open-github))

    (transient-append-suffix 'magit-submodule "g"
      '("p" "open PR in github" %magit-open-github-pr))

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

 ;;; org
  (use-package org :defer t
    :diminish ('org-indent-mode . "")
    :init
    (defun %org-update-all-statistics ()
      (interactive)
      (let ((current-prefix-arg 4)) ;; emulate C-u
        (call-interactively 'org-update-statistics-cookies)))

    (defun %org-todo ()
      (interactive)
      (let ((current-prefix-arg 4)) ;; emulate C-u
        (call-interactively 'org-todo)))

    (defun %org-journal-entry- (stars has-newline)
      (let ((header (concat
                     (if has-newline "\n" "")
                     (make-string stars ?*)
                     " ")))
        (end-of-buffer)
        (insert "\n\n")
        (delete-blank-lines)
        (insert header)
        (%org-insert-datetime)
        (insert " ")))

    (defun %org-journal-entry ()
      (interactive)
      (%org-journal-entry- 2 nil)
      (%add))

    (defun %org-insert-datetime ()
      (interactive)
      (setq current-prefix-arg '(16))      ; C-u C-u
      (call-interactively 'org-time-stamp))

    (defun %org-insert-date ()
      (interactive)
      (%org-insert-datetime)     ;; <2019-03-17 Sun 13:42>|
      (backward-char)              ;; <2019-03-17 Sun 13:42|>
      (org-delete-backward-char 6) ;; <2019-03-17 Sun|>
      (forward-char))               ;; <2019-03-17 Sun>|

    (defun %org-beginning-of-line ()
      (interactive)
      (beginning-of-line)
      (if (org-at-heading-p)
          (search-forward " ")))

    (defun %org-insert-heading ()
      (interactive)
      (if (%on-blank-line)
          (delete-char -1))
      (%eol)
      (org-insert-heading-after-current)
      (%add))

    (defun %org-insert-list-elt ()
      (interactive)
      (%eol)
      (org-meta-return)
      (%add))

    (defmacro %defun-todo-fn (name keyword)
      `(defun ,name ()
         (interactive)
         (org-todo ,keyword)))
    (put '%defun-todo-fn 'lisp-indent-function 'defun)

    ;; not working: https://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Interactive.html
    ;; (defmacro %defun-todo-reason-fn (name keyword)
    ;;   `(defun ,name (arg)
    ;;      (interactive (concat "s" ,keyword " "))
    ;;      (org-todo ,keyword)
    ;;      (insert arg)
    ;;      (insert "]")))
    ;; (put '%defun-todo-fn 'lisp-indent-function 'defun)

    (%defun-todo-fn %org-clear-todo "")
    (%defun-todo-fn %org-plan "[PLAN]")
    (%defun-todo-fn %org-todo "[TODO]")
    (%defun-todo-fn %org-wait "[WAIT]")
    (%defun-todo-fn %org-done "[DONE]")
    (%defun-todo-fn %org-skip "[SKIP]")

    (defun %org-reset-subtree ()
      (interactive)
      (save-excursion
        (org-mark-subtree)
        (%exchange-regex (regexp-quote "* [DONE]") "* [TODO]" nil (region-beginning) (region-end)))
      (save-excursion
        (org-mark-subtree)
        (%exchange-regex (regexp-quote "* [SKIP]") "* [TODO]" nil (region-beginning) (region-end))
        )
      (org-ctrl-c-ctrl-c)
      )

    (defun %org-finish ()
      (interactive)
      (save-excursion
        (%org-done)
        (org-archive-to-archive-sibling)))

    (defun %org-create-archive ()
      (interactive)
      (%org-insert-child-heading)
      (insert "Archive :ARCHIVE:")
      (%org-insert-child-heading)
      (insert "notes")
      (%up)
      (so/JK-org-move-to-top)
      (%org-full-save)
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

    (defun %org-insert-todo ()
      (interactive)
      (%org-insert-heading)
      (%org-todo))

    (defun %org-insert-note ()
      (interactive)
      (%eob)
      (delete-blank-lines)
      (if (not (%on-blank-line))
          (progn
            (%eol)
            (insert "\n")))
      (%org-insert-datetime)
      (insert "\n")
      (%add))

    (defun %org-checklist ()
      (interactive)
      (org-todo "")
      (%org-beginning-of-line)
      (insert "[/] ")
      )

    (defun %org-full-save ()
      (interactive)
      (%whitespace)
      (%org-update-all-statistics)
      (%save)
      )

    (defun %org-insert-child-heading ()
      (interactive)
      (call-interactively '%org-insert-heading)
      (org-demote-subtree))

    (defun %org-insert-timestamp-heading ()
      (interactive)
      (call-interactively '%org-insert-heading)
      (call-interactively '%org-insert-datetime))

    (defalias '%org-deadline-insert 'org-deadline)
    (defalias '%org-schedule-insert 'org-schedule)

    (defun %org-deadline-now ()
      (interactive)
      (org-back-to-heading)
      (%eol)
      (insert "\nDEADLINE: ")
      (call-interactively '%org-insert-datetime)
      )

    (defun %org-schedule-now ()
      (interactive)
      (org-back-to-heading)
      (%eol)
      (insert "\nSCHEDULED: ")
      (call-interactively '%org-insert-datetime)
      )

    (setq %org-hide-level 1)
    (defun %org-dec-hiding ()
      (interactive)
      (if (> %org-hide-level 1)
          (setq %org-hide-level (- %org-hide-level 1)))
      (outline-hide-sublevels %org-hide-level))

    (defun %org-inc-hiding ()
      (interactive)
      (if (< %org-hide-level 12)
          (setq %org-hide-level (+ %org-hide-level 1)))
      (outline-hide-sublevels %org-hide-level))

    (defun %org-reset-hiding ()
      (interactive)
      (setq %org-hide-level 1)
      (outline-hide-sublevels %org-hide-level))

    ;; (defalias '%table-view 'org-columns)
    (defalias '%org-agenda 'org-agenda-list)
    (defalias '%clock-in 'org-clock-in)
    (defalias '%clock-out 'org-clock-out)
    (defalias '%clock-cancel 'org-clock-cancel)
    ;; (defalias '%table-contents 'org-columns-content)
    (defun %refresh-table ()
      (interactive)
      (call-interactively 'org-columns-redo)
      (call-interactively 'org-columns-content)
      )

    (defun %org-promote ()
      (interactive)
      (if (org-at-heading-p)
          (call-interactively 'org-promote-subtree)
        (call-interactively 'org-outdent-item-tree)
      ))

    (defun %org-demote ()
      (interactive)
      (if (org-at-heading-p)
          (call-interactively 'org-demote-subtree)
        (call-interactively 'org-indent-item-tree)
        ))

    (defun %org-morning ()
      (interactive)
      (org-clone-subtree-with-time-shift 1 "+1d")
      (org-forward-heading-same-level 1)
      (org-metaup)
      )

    (defun %org-unfinish ()
      (interactive)
      (org-clone-subtree-with-time-shift 1 "+1d")
      )

    (defun %table-begin ()
      (interactive)
      (%clock-in)
      (setq current-prefix-arg '(4))      ;; C-u
      (org-columns-todo))

    (defun %table-finish ()
      (interactive)
      (%clock-out)
      (setq current-prefix-arg '(4))      ;; C-u
      (org-columns-todo))

    (defun %set-todo ()
      (interactive)
      (setq current-prefix-arg '(4))      ;; C-u
      (org-columns-todo))

    (defun %table-view ()
      (interactive)
      (save-excursion
        (%bob) ;; applies to entire file, using default COLUMNS
        (call-interactively 'org-columns)
        (call-interactively 'org-columns-content))

      (bind-key "n" '%down org-columns-map)
      (bind-key "e" '%up org-columns-map)

      (bind-key "[" 'org-columns-previous-allowed-value org-columns-map)
      (bind-key "]" 'org-columns-next-allowed-value org-columns-map)

      (bind-key "H" 'org-columns-previous-allowed-value org-columns-map)
      (bind-key "N" 'org-columns-next-allowed-value org-columns-map)
      (bind-key "E" '%up org-columns-map)
      (bind-key "I" '%up org-columns-map)

      (bind-key "t" '%set-todo org-columns-map)
      (bind-key "b" '%table-begin org-columns-map)
      (bind-key "f" '%table-finish org-columns-map)

      (bind-key "c" nil org-columns-map)
      (bind-key "cc" 'org-clock-jump-to-current-clock org-columns-map)
      (bind-key "ci" '%clock-in org-columns-map)
      (bind-key "co" '%clock-out org-columns-map)

      (bind-key "r" '%refresh-table org-columns-map)
      (bind-key "G" '%refresh-table org-columns-map)
      )

    ;; iterm remaps C-; to M-#
    :bind
    (:map modalka-mode-map
     ("m" . 'outline-previous-visible-heading)
     ("." . 'outline-next-visible-heading)
     ("k" . '%org-beginning-of-line)

     :map org-mode-map
     ("C-M-h" . 'org-backward-heading-same-level)
     ("C-M-n" . 'outline-next-visible-heading)
     ("C-M-e" . 'outline-previous-visible-heading)
     ("C-M-i" . 'org-forward-heading-same-level)
     ("C-M-u" . 'outline-up-heading)

     ("M-# <f8>" . '%org-full-save)

     ;; ("M-&" . '%org-inc-hiding)
     ;; ("M-]" . '%org-dec-hiding)

     ("M-<" . 'org-force-cycle-archived)
     ;; ("C-M-a" . '%org-archive-to-sibling) ;; C-M-:
     ;; ("C-M-l" . 'org-promote-subtree)
     ;; ("C-M-y" . 'org-demote-subtree)
     ("C-M-l" . '%org-promote)
     ("C-M-y" . '%org-demote)

     ("C-M-o" . '%org-insert-heading)
     ("C-M-_" . '%org-insert-child-heading)
     ("M-^" . '%org-insert-timestamp-heading) ;; C-M-return

     ;; ("C-M-f" . '%org-finish)
     ;; ("C-M-c" . '%org-complete)
     ;; ("C-M-p" . '%org-plan)
     ("C-M-x" . '%org-clear-todo)
     ("C-M-r" . '%org-reset-subtree)
     ("C-M-t" . '%org-todo)
     ("C-M-d" . '%org-done)
     ("C-M-w" . '%org-wait)
     ("C-M-s" . '%org-skip)

     ("C-M-a" . '%org-agenda)
     ("C-M-c" . '%org-checklist)

     ("M-# M-l" . '%org-journal-entry)
     ("M-# M-i M-l" . '%org-journal-entry)
     ("M-# M-i M-d" . '%org-insert-date)
     ("M-# M-i M-t" . '%org-insert-datetime)
     ("M-# M-i M-a" . '%org-create-archive)

     ;; ("M-# M-d M-i" . '%org-deadline-insert)
     ;; ("M-# M-d M-n" . '%org-deadline-now)
     ;; ("M-# M-s M-i" . '%org-schedule-insert)
     ;; ("M-# M-s M-n" . '%org-schedule-now)

     ;; ("M-# d" . 'org-deadline)
     ;; ("M-# s" . 'org-schedule)

     ;; ("M-# C-a" . 'org-agenda-list)

     ;; ("M-RET" . '%org-insert-note)
     ("M-# M-n" . '%org-insert-list-elt)
     ;; ("M-# M-h" . '%org-insert-heading)
     ;; ("M-# M-m" . '%org-insert-todo)

     ;; ("M-# M-t" . '%org-todo)
     ;; ("M-# M-d" . '%org-done)
     ;; ("M-# M-c" . '%org-complete)
     ;; ("M-# M-p" . '%org-plan)
     ;; ("M-# M-w" . '%org-wait)

     ("M-# M-b" . '%org-begin)
     ("M-# M-f" . '%org-finish)

     ("M-# T" . '%table-view)
     ("M-# c i" . '%clock-in)
     ("M-# c o" . '%clock-out)
     ("M-# c DEL" . '%clock-cancel)

     ("M-# M-w" . 'org-cut-subtree)
     ("M-# M-c" . 'org-copy-subtree)
     ("M-# M-p" . 'org-paste-subtree)
     )

    :hook
    (org-mode . visual-line-mode)
    ;; fixme see if this is needed
    (org-mode . (lambda () (progn
                             ;; stopped working in :bind
                             (bind-key "k" '%org-beginning-of-line modalka-mode-map)
                             (bind-key ";" '%reset-org-hiding modalka-mode-map)
                             (bind-key "[" '%noop modalka-mode-map)
                             (bind-key "]" '%noop modalka-mode-map)
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
    ;; (add-hook 'before-save-hook '%org-update-all-statistics)

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


;; functions
  (defun %tmux-copy ()
    (interactive)
    ;; FIXME still doesn't work for strings with "
    (shell-command
     (format-message "tmux set-buffer -b emacsclip \"%s\"" (buffer-substring (mark) (point))))
    (deactivate-mark))

  (defun %show-filepaths ()
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
   '(ocamlformat enh-ruby-mode yasnippet terraform-mode dumb-jump diminish expand-region modalka yaml-mode whole-line-or-region web-mode use-package tuareg s rainbow-mode parent-mode org markdown-mode jinja2-mode helm-projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
