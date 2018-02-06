%include "scheme.s"

section .bss

section .data

x:
	dq SOB_UNDEFINED

sobInt5:
	dq MAKE_LITERAL(T_INTEGER, 5)

sobInt10:
	dq MAKE_LITERAL(T_INTEGER, 10)

sobInt333:
	dq MAKE_LITERAL(T_INTEGER, 333)

sobInt7:
	dq MAKE_LITERAL(T_INTEGER, 7)

sobInt8:
	dq MAKE_LITERAL(T_INTEGER, 8)

sobInt9:
	dq MAKE_LITERAL(T_INTEGER, 9)



section .text

	extern exit, printf, scanf, malloc

	global main

main:

	push rbp
	mov rbp, rsp

; start
	mov rax, qword [sobInt5]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
	mov rax, qword [sobInt10]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
	mov rax, qword [sobInt8]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
	mov rax, qword [sobInt333]
	mov rbx, x
	mov qword [rbx], rax
	mov rax, SOB_VOID

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
	mov rax, qword [sobInt7]
	cmp rax, SOB_FALSE
	je L0
	mov rax, qword [sobInt8]

	jmp Lend0
L0:
		mov rax, qword [sobInt9]

Lend0:

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end
	mov rsp, rbp
	pop rbp
	ret

