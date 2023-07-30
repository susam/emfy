loc:
	f() { grep -vE '^$$|^;|^ *".*"$$' ../init.el; }; f; f | wc -l

demo:
	mv ../.git ../git
	cp ../init.el init.el
	emacs -q -l init.el --eval '(progn (set-frame-size (selected-frame) 132 32) (find-file "example.md") (split-window-right) (find-file "example.el") (goto-char (1- (point-max))) (message ""))'
	mv ../git ../.git

wombat1:
	mv ../.git ../git
	sed -n '1,/load-theme/p' ../init.el > init.el
	emacs -q -l init.el --eval '(progn (set-frame-size (selected-frame) 80 12) (find-file "foo.el") (message ""))'
	mv ../git ../.git

wombat2:
	mv ../.git ../git
	sed -n '1,/Interactively do things/p' ../init.el > init.el
	emacs -q -l init.el --eval '(progn (set-frame-size (selected-frame) 80 12) (find-file "foo.el") (message ""))'
	mv ../git ../.git

lorem1:
	mv ../.git ../git
	sed -n '1,/show-trailing-whitespace/p' ../init.el > init.el
	emacs -q -l init.el --eval '(progn (set-frame-size (selected-frame) 80 12) (find-file "lorem1.txt") (message ""))'
	mv ../git ../.git

lorem2:
	mv ../.git ../git
	sed -n '1,/indicate-empty-lines/p' ../init.el > init.el
	emacs -q -l init.el --eval '(progn (set-frame-size (selected-frame) 80 12) (find-file "lorem2.txt") (message ""))'
	mv ../git ../.git

lorem3:
	printf '%s' "$$(cat lorem2.txt)" > lorem3.txt
	mv ../.git ../git
	sed -n '1,/indicate-buffer-boundaries/p' ../init.el > init.el
	emacs -q -l init.el --eval '(progn (set-frame-size (selected-frame) 80 12) (find-file "lorem3.txt") (message ""))'
	mv ../git ../.git

lorem4:
	printf '%s\n' "$$(cat lorem2.txt)" > lorem4.txt
	mv ../.git ../git
	sed -n '1,/indicate-buffer-boundaries/p' ../init.el > init.el
	emacs -q -l init.el --eval '(progn (set-frame-size (selected-frame) 80 12) (find-file "lorem4.txt") (message ""))'
	mv ../git ../.git

reset:
	mv ../git ../.git; :
	rm -f init.el lorem3.txt lorem4.txt
