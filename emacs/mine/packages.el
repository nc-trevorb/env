(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(setq web-mode-markup-indent-offset 2) ;; html indent
(setq web-mode-code-indent-offset 2) ;; js indent

(add-to-list 'auto-mode-alist '("zshenv" . shell-script-mode))
(add-to-list 'auto-mode-alist '("zshrc" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))


(require 'projectile)

(projectile-global-mode) ;; like command-t
(setq projectile-globally-ignored-file-suffixes '(".png" ".gif" ".pdf"))

(require 'helm-config)
(helm-mode 1)
(define-key helm-map               (kbd "C-M-h")     'backward-kill-word)
(define-key helm-map               (kbd "ESC M-h")   'helm-help)

(define-key helm-find-files-map (kbd "M-n") 'next-history-element)
(define-key helm-find-files-map (kbd "M-p") 'previous-history-element)

(require 'helm-projectile)
(helm-projectile-on)

(setq projectile-globally-ignored-directories '(
                                                "node_modules"
                                                ;; "app/assets"
                                                "tmp"
                                                "vendor"
                                                "elpa"
                                                "zsh/zsh-syntax-highlighting"
                                                ))

;; So that helm does not use current window to display the helm window
(setq helm-split-window-in-side-p t)

;; make helm always open at the bottom
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

(font-lock-add-keywords 'javascript-mode
                        '(("Math" . font-lock-type-face)
                          ("Number" . font-lock-type-face)
                          ("Date" . font-lock-type-face)
                          ("String" . font-lock-type-face)
                          ("RegExp" . font-lock-type-face)
                          ("Array" . font-lock-type-face)
                          ("JSON" . font-lock-type-face)
                          ("Object" . font-lock-type-face)
                          ))

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; (require 'expand-region)
;; (global-set-key (kbd "M-s i") 'er/expand-region)
;; (global-set-key (kbd "M-s M-i") 'er/expand-region)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(require 'whole-line-or-region)
(whole-line-or-region-mode)

;; for easy keybindings with single-key repeats
(require 'smartrep)

;; requiring this so a font face loads
(require 'linum)
