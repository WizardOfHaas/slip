main: lisp.asm
	nasm lisp.asm -o lisp.com

test:
	dosbox lisp.com