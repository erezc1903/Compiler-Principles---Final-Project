%include "scheme.s"

section .bss

section .data

sobFalse:
	dq SOB_FALSE
sobInt1:
	dq MAKE_LITERAL(T_INTEGER, 1)

sobInt2:
	dq MAKE_LITERAL(T_INTEGER, 2)



section .text

	extern exit, printf, scanf, malloc

	global main

main:

mov rax, qword [sobFalse]
cmp rax, SOB_FALSE
je L0
	mov rax, qword [sobInt1]

jmp Lend0
L0:
	mov rax, qword [sobInt2]

Lend0:
	push rbp
	push rax
	call write_sob_if_not_void
	add rsp, 8
	pop rbp
	ret

