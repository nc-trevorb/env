;; (autoload 'python-mode "python-mode" "Major mode for python files" t)

(add-to-list 'auto-mode-alist '("\\.j2$" . jinja2-mode))

(defun bt/python-repl ()
  (interactive)
  (insert "import IPython")
  (newline-and-indent)
  (insert "IPython.embed()")
  (save-buffer)
  )

(define-key my-keys-minor-mode-map (kbd ": >") 'bt/python-repl)
