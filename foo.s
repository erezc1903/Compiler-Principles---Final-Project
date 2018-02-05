%include "scheme.s"

section .bss

section .data

sobInt2:
	dq MAKE_LITERAL(T_INTEGER, 2)

sobFalse:
	dq SOB_FALSE
sobInt8:
	dq MAKE_LITERAL(T_INTEGER, 8)

sobTrue:
	dq SOB_TRUE

sobInt3:
	dq MAKE_LITERAL(T_INTEGER, 3)



section .text

	extern exit, printf, scanf, malloc

	global main

main:

	mov rax, qword [sobTrue]
	mov rax, qword [sobInt2]
	mov rax, qword [sobTrue]
	cmp rax, SOB_FALSE
	je L0
	mov rax, qword [sobFalse]
	cmp rax, SOB_FALSE
	jne Lend101

	mov rax, qword [sobInt8]
	cmp rax, SOB_FALSE
	jne Lend101

	mov rax, qword [sobTrue]
	cmp rax, SOB_FALSE
	jne Lend101

Lend101:


	jmp Lend0
L0:
		mov rax, qword [sobInt3]

Lend0:
	push rbp
	push rax
	call write_sob_if_not_void
	add rsp, 8
	pop rbp
	ret

