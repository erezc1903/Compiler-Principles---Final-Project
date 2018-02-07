%include "scheme.s"

section .bss

section .data

number?:
	dq SOB_UNDEFINED

a:
	dq SOB_UNDEFINED

pair?:
	dq SOB_UNDEFINED

sobNegInt4:
	dq MAKE_LITERAL (T_INTEGER, -4)

sobNegFrac1_2:
	dq MAKE_LITERAL_FRACTION (sobNegInt1, sobInt2)

sobNegInt1:
	dq MAKE_LITERAL (T_INTEGER, -1)

sobInt2:
	dq MAKE_LITERAL (T_INTEGER, 2)

sobNil:
	dq SOB_NIL



section .text

	extern exit, printf, scanf, malloc

	global main

main:

	push rbp
	mov rbp, rsp

; start
	mov rax, qword [sobNegInt4]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
	mov rax, qword [sobInt2]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
	mov rax, qword [sobNegFrac1_2]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
	mov rax, qword [sobNegFrac1_2]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_FRACTION
	je numberTrue201
	mov rax, SOB_FALSE
	jmp numberDone201
numberTrue201:
	mov rax, SOB_TRUE
numberDone201:
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_FRACTION
	je numberTrue202
	mov rax, SOB_FALSE
	jmp numberDone202
numberTrue202:
	mov rax, SOB_TRUE
numberDone202:
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
	mov rax, qword [sobNil]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_PAIR
	je pairTrue201
	mov rax, SOB_FALSE
	jmp pairDone201
pairTrue201:
	mov rax, SOB_TRUE
pairDone201:
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end
	mov rsp, rbp
	pop rbp
	ret

