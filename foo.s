%include "scheme.s"

section .data

print_format:
	db "%d", 10, 0

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
