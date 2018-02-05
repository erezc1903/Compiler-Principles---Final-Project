%include "scheme.s"

section .bss

section .data

x:
	dq SOB_UNDEFINED

y:
	dq SOB_UNDEFINED

sobInt2:
	dq MAKE_LITERAL(T_INTEGER, 2)

sobFalse:
	dq SOB_FALSE
sobTrue:
	dq SOB_TRUE

sobInt3:
	dq MAKE_LITERAL(T_INTEGER, 3)

sobInt10:
	dq MAKE_LITERAL(T_INTEGER, 10)

sobInt7:
	dq MAKE_LITERAL(T_INTEGER, 7)

sobInt8:
	dq MAKE_LITERAL(T_INTEGER, 8)

sobInt9:
	dq MAKE_LITERAL(T_INTEGER, 9)

sobInt1:
	dq MAKE_LITERAL(T_INTEGER, 1)



section .text

	extern exit, printf, scanf, malloc

	global main

main:

	mov rax, qword [sobInt7]
	cmp rax, SOB_FALSE
	je L0
	mov rax, qword [sobInt8]

	jmp Lend0
L0:
		mov rax, qword [sobInt9]

Lend0:

	mov rax, qword [sobInt1]
	mov rbx, y
	mov qword [rbx], rax

	mov rax, qword [sobTrue]
	mov rax, qword [sobInt2]
	cmp rax, SOB_FALSE
	je L1
	mov rax, qword [sobFalse]
	cmp rax, SOB_FALSE
	jne Lend101
	cmp rax, SOB_FALSE
	jne Lend101
	mov rax, qword [sobTrue]
	cmp rax, SOB_FALSE
	jne Lend101
Lend101:


	jmp Lend1
L1:
		mov rax, qword [sobInt3]

Lend1:

	mov rax, qword [sobInt10]
	mov rbx, x
	mov qword [rbx], rax

	push rbp
	push rax
	call write_sob_if_not_void
	add rsp, 8
	pop rbp
	ret

