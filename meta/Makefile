help:
	@echo 'Usage: make [target]'
	@echo
	@echo 'Setup:'
	@echo '  setup-bare  Set up Emfy in fake home without installing packages.'
	@echo '  setup-full  Set up Emfy in fake home along with installed packages.'
	@echo
	@echo 'Checks:'
	@echo '  loc         Count non-blank, non-comment, and non-docstring lines.'
	@echo '  lint        Lint with Flycheck.'
	@echo '  benchmark   Print benchmarking info about emacs-init-time.'
	@echo '  run         Run Emacs with whatever state fake home directory is in.'
	@echo
	@echo 'Demos:'
	@echo '  demo        Set up full Emfy and run Emacs for demo screenshot.'
	@echo '  wombat1     Set up bare Emfy and demo Wombat defaults.'
	@echo '  wombat2     Set up bare Emfy and demo custom colours.'
	@echo '  lorem1      Set up bare Emfy and demo trailing spaces.'
	@echo '  lorem2      Set up bare Emfy and demo trailing newlines.'
	@echo '  lorem3      Set up bare Emfy and demo missing newline.'
	@echo '  lorem4      Set up bare Emfy and demo present newline.'
	@echo
	@echo 'Checklist:'
	@echo '  make loc'
	@echo '  make setup-full'
	@echo '  make lint'
	@echo '  make benchmark'
	@echo '  make demo'
	@echo '  make wombat1'
	@echo '  make wombat2'
	@echo '  make lorem1'
	@echo '  # etc.'

setup-bare:
	rm -rf .emacs.d/
	mkdir .emacs.d/
	cp ../init.el .emacs.d/

setup-full: setup-bare
	HOME=. emacs --eval '(progn (install-packages) (kill-emacs))'

loc:
	f() { grep -vE '^$$|^;|^ *".*"$$' ../init.el; }; f; f | wc -l

lint:
	emacs -l lint.el 2>&1 | \
	grep -vE 'with-eval-after-load|prefix "init"|key sequence is reserved'

benchmark:
	HOME=. emacs --eval "(progn (princ (emacs-init-time) 'external-debugging-output) (kill-emacs))"; echo

run:
	HOME=. emacs

demo:
	HOME=. emacs --eval '(progn (set-frame-size (selected-frame) 132 32) (find-file "example.md") (split-window-right) (find-file "example.el") (goto-char (1- (point-max))) (message ""))'

wombat1: setup-bare
	sed -n '1,/load-theme/p' ../init.el > .emacs.d/init.el
	HOME=. emacs --eval '(progn (set-frame-size (selected-frame) 80 12) (find-file "foo.el") (message ""))'

wombat2: setup-bare
	sed -n '1,/Highlight/p' ../init.el > .emacs.d/init.el
	HOME=. emacs --eval '(progn (set-frame-size (selected-frame) 80 12) (find-file "foo.el") (message ""))'

lorem1: setup-bare
	sed -n '1,/Show stray lines/p' ../init.el > .emacs.d/init.el
	HOME=. emacs --eval '(progn (set-frame-size (selected-frame) 80 12) (find-file "lorem1.txt") (message ""))'

lorem2: setup-bare
	sed -n '1,/indicate-empty-lines/p' ../init.el > .emacs.d/init.el
	HOME=. emacs --eval '(progn (set-frame-size (selected-frame) 80 12) (find-file "lorem2.txt") (message ""))'

lorem3: setup-bare
	sed -n '1,/indicate-buffer-boundaries/p' ../init.el > .emacs.d/init.el
	printf '%s' "$$(cat lorem2.txt)" > lorem3.txt
	HOME=. emacs --eval '(progn (set-frame-size (selected-frame) 80 12) (find-file "lorem3.txt") (message ""))'

# Indicate buffer boundaries (terminating newline present)
lorem4: setup-bare
	sed -n '1,/indicate-buffer-boundaries/p' ../init.el > .emacs.d/init.el
	printf '%s\n' "$$(cat lorem2.txt)" > lorem4.txt
	HOME=. emacs --eval '(progn (set-frame-size (selected-frame) 80 12) (find-file "lorem4.txt") (message ""))'

reset:
	rm -rf .emacs.d/ .tmp/
