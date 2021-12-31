loc:
	f() { grep -vE '^$$|^;|^ *".*"$$' ../.emacs; }; f; echo ';;;;'; f | wc -l

lorem:
	printf '%s' "$$(cat lorem2.txt)" > lorem3.txt
	printf '%s\n' "$$(cat lorem2.txt)" > lorem4.txt
