%include "scheme.s"

section .data

print_format:
	db "%d", 10, 0

sobVector0:
	dq MAKE_LITERAL_VECTOR sobPair9, sobString8, sobVector2

sobString1:
	dq MAKE_LITERAL_STRING "fg"

sobVector2:
	dq MAKE_LITERAL_VECTOR sobString5, sobInt45, sobSymbolc, sobInt100, sobString1

sobSymbolc:
	dq MAKE_LITERAL(T_SYMBOL, c)

sobInt100:
	dq MAKE_LITERAL(T_INTEGER, 100)

sobString5:
	dq MAKE_LITERAL_STRING "30"

sobInt45:
	dq MAKE_LITERAL(T_INTEGER, 45)

sobNil:
	dq SOB_NIL

sobString8:
	dq MAKE_LITERAL_STRING "25"

sobPair9:
	dq MAKE_LITERAL_PAIR(sobInt20, sobNil)

sobInt20:
	dq MAKE_LITERAL(T_INTEGER, 20)

section .bss

section .text

	extern exit, printf, scanf, malloc

	global main

main:

	push rbp
	mov rax, qword [const2]
	push rax
	call write_sob
	add rsp, 8
	pop rbp
	ret
