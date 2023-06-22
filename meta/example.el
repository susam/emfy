;;; Emacs Lisp Examples.  -*- lexical-binding: t; -*-

(defun hello-world ()
  "Show 'hello, world' message."
  (interactive)
  (message "hello, world"))

(defun show-current-time ()
  "Show current time."
  (interactive)
  (message (current-time-string)))

(defun fibonacci (n)
  "Compute nth Fibonacci number."
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fibonacci (- n 1))
              (fibonacci (- n 2))))))

(global-set-key (kbd "C-c h") 'hello-world)
(global-set-key (kbd "C-c t") 'show-current-time)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
