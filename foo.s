%include "scheme.s"

section .data

print_format:
	db "%d", 10, 0

sobVector0:
	MAKE_LITERAL_VECTOR sobString4, sobInt45, sobInt100, sobString3

sobVector1:
	MAKE_LITERAL_VECTOR sobPair8, sobString7, sobVector0

sobInt100:
	dq MAKE_LITERAL(T_INTEGER, 100)

sobString3:
	MAKE_LITERAL_STRING "fg"

sobString4:
	MAKE_LITERAL_STRING "30"

sobInt45:
	dq MAKE_LITERAL(T_INTEGER, 45)

sobNil:
	dq SOB_NIL

sobString7:
	MAKE_LITERAL_STRING "25"

sobPair8:
	dq MAKE_LITERAL_PAIR(sobInt20, sobNil)

sobInt20:
	dq MAKE_LITERAL(T_INTEGER, 20)

sobInt1:
	dq MAKE_LITERAL(T_INTEGER, 1)

sobInt23:
	dq MAKE_LITERAL(T_INTEGER, 23)

section .bss

section .text

	extern exit, printf, scanf, malloc

	global main

main:

	push rbp
	mov rax, qword [sobInt1]
	push rax
	call write_sob
	add rsp, 8
	pop rbp
	ret
