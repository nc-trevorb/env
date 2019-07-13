(defun bt/open-mli-file ()
  (interactive)
  (split-window-below)
  (find-file (concat (buffer-file-name) "i"))
  )

;; (define-key my-keys-minor-mode-map (kbd ": I") 'bt/open-mli-file)

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

(define-key my-keys-minor-mode-map (kbd ": I") 'bt/open-mli-file)
(define-key my-keys-minor-mode-map (kbd ": e m") 'merlin-switch-to-ml)
(define-key my-keys-minor-mode-map (kbd ": e I") 'merlin-switch-to-mli)

;; FIXME repeating C-o
(define-key my-keys-minor-mode-map (kbd "C-c C-o") 'merlin-pop-stack)

;; (define-key my-keys-minor-mode-map (kbd "M-p") 'merlin-phrase-prev)
;; (define-key my-keys-minor-mode-map (kbd "M-n") 'merlin-phrase-next)

(define-key my-keys-minor-mode-map (kbd "C-M-e") 'merlin-error-prev)
(define-key my-keys-minor-mode-map (kbd "M-e") 'merlin-error-next)

(define-key my-keys-minor-mode-map (kbd "M-t") 'merlin-type-enclosing)
