

%:
	echo '(load "compiler.scm") (compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q
	nasm -f elf64 $(MAKECMDGOALS).s
	gcc -lc -o $(MAKECMDGOALS) $(MAKECMDGOALS).o

clean:
	rm -f foo.s foo.o foo