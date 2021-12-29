;;; Emacs Lisp Examples.

(defun hello-world ()
  "Show 'hello, world' message."
  (interactive)
  (message "hello, world"))

(defun show-current-time ()
  "Show current time for 2 seconds."
  (interactive)
  (message (current-time-string))
  (sleep-for 2)
  (message nil))

(global-set-key (kbd "C-c h") 'hello-world)
(global-set-key (kbd "C-c t") 'show-current-time)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
