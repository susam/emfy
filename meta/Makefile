main:
	mv ../.git ../git
	emacs -q -l ../.emacs --eval '(progn (set-frame-size (selected-frame) 132 32) (find-file "example.md") (split-window-right) (find-file "example.el") (goto-char (1- (point-max))) (message ""))'
	mv ../git ../.git

loc:
	f() { grep -vE '^$$|^;|^ *".*"$$' ../.emacs; }; f; f | wc -l

lorem:
	printf '%s' "$$(cat lorem2.txt)" > lorem3.txt
	printf '%s\n' "$$(cat lorem2.txt)" > lorem4.txt

reset:
	mv ../git ../.git
