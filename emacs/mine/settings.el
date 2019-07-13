(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(add-to-list 'custom-theme-load-path "~/.emacs.d/mine/theme/")
(load-theme 'btcolor t)

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "minor mode with all my keys"
  t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)
;; make my minor mode the most important minor mode
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
      (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?║))

;; (setq ag-highlight-search t)
(setq linum-format "%4d│")
;; FIXME don't put all linum modes in one place
(add-hook 'haskell-mode-hook #'linum-mode)
(add-hook 'ruby-mode-hook #'linum-mode)
(add-hook 'sh-mode-hook #'linum-mode)
(add-hook 'markdown-mode-hook #'linum-mode)
(add-hook 'js-mode-hook #'linum-mode)
(add-hook 'html-mode-hook #'linum-mode)
(add-hook 'yaml-mode-hook #'linum-mode)
(add-hook 'elm-mode-hook #'linum-mode)
(add-hook 'lisp-mode-hook #'linum-mode)
(add-hook 'emacs-lisp-mode-hook #'linum-mode)
(add-hook 'tuareg-mode-hook #'linum-mode)
(add-hook 'python-mode-hook #'linum-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq ruby-insert-encoding-magic-comment nil)
(setq x-select-enable-clipboard            t
      x-select-enable-primary              t
      save-interprogram-paste-before-kill  t
      apropos-do-all                       t
      save-place-file (concat user-emacs-directory "places"))

(setq-default auto-save-default nil)                    ;; no autosave
(setq make-backup-files nil)                            ;; no autosave
(setq backup-directory-alist `(("." . "~/.saves")))     ;; save backups in separate directory
(setq-default indent-tabs-mode nil)                     ;; use spaces instead of tabs
(setq-default coffee-tab-width 2)                       ;; coffee mode tab width
(setq-default js-indent-level 2)                        ;; js mode tab width
(setq-default c-basic-offset 8)                         ;; c mode tab width
(show-paren-mode 1)                                     ;; highlight matching parens
(fset 'yes-or-no-p 'y-or-n-p)                           ;; faster prompts
(setq confirm-nonexistent-file-or-buffer nil)           ;; don't prompt to create a new file
(define-key global-map (kbd "RET") 'newline-and-indent) ;; indent after pressing 'enter'
(setq set-mark-command-repeat-pop 1)                    ;; can press C-space to cycle through mark ring (after pressing C-x C-space)
(setq-default scss-compile-at-save nil)                 ;; don't try to compile scss files
(setq-default auto-compression-mode 0)                  ;; don't try to decompress files (like ~/.z)
(setq-default fill-column 100)                          ;; text width for fill-paragraph
(setq-default comment-column 0)                         ;; stop moving comments to the right
(column-number-mode t)                                  ;; show column number
(set-default 'truncate-lines t)                         ;; disable word wrap
;; (setq ruby-deep-indent-paren nil)                       ;; better indentation for multiline hashes in ruby
(winner-mode 1)                                         ;; undo/redo split layout changes
(setq echo-keystrokes 0.001)                            ;; like vim's showcmd
(setq require-final-newline nil)                        ;; don't add newline at end of file
(setq elm-format-on-save t)

(setq initial-major-mode 'text-mode) ;; FIXME set to org-mode (once I fix org-mode's load time)
(setq inhibit-startup-screen t)
(setq initial-scratch-message
      (concat "\
# " (replace-regexp-in-string " (.*\n.*" "" (emacs-version)) "\n\n"
  ))
