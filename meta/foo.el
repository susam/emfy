;;; Elisp Examples.

(defun hello-world ()
  "Show 'hello, world' message."
  (interactive)
  (message "hello, world"))

(global-set-key (kbd "C-c h") 'hello-world)
