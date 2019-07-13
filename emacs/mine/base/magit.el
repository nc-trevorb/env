(defalias 'blame 'magit-blame)

(defun bt/git ()
  (interactive)
  (magit-status)
  (delete-other-windows)
  (magit-process-buffer)
  (other-window 1)
  )

(define-key my-keys-minor-mode-map (kbd "M-g M-g") 'bt/git)

(advice-add 'magit-status :before #'delete-other-windows)
(setq magit-refs-sections-hook
      '(magit-insert-error-header
       magit-insert-branch-description
       magit-insert-local-branches
       magit-insert-tags))

