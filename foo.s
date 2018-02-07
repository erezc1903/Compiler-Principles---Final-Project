%include "scheme.s"

section .bss

section .data

number?:
	dq SOB_UNDEFINED

sobInt3:
	dq MAKE_LITERAL (T_INTEGER, 3)

sobNil:
	dq SOB_NIL

sobFrac1_2:
	dq MAKE_LITERAL_FRACTION (sobInt1, sobInt2)

sobInt1:
	dq MAKE_LITERAL (T_INTEGER, 1)

sobInt2:
	dq MAKE_LITERAL (T_INTEGER, 2)



section .text

	extern exit, printf, scanf, malloc

	global main

; =============================== PRIMITIVE FUNCTIONS =========================

handle_pair?:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_PAIR
	je truePair?
	mov rax, SOB_FALSE
	jmp donePair?

truePair?:
	mov rax, SOB_TRUE

donePair?:
	leave
	ret


handle_boolean?:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_BOOL
	je trueBoolean?
	mov rax, SOB_FALSE
	jmp doneBoolean?

trueBoolean?:
	mov rax, SOB_TRUE

doneBoolean?:
	leave
	ret


handle_integer?:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_INTEGER
	je trueInteger?
	mov rax, SOB_FALSE
	jmp doneInteger?

trueInteger?:
	mov rax, SOB_TRUE

doneInteger?:
	leave
	ret


handle_null?:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_NIL
	je trueNull?
	mov rax, SOB_FALSE
	jmp doneNull?

trueNull?:
	mov rax, SOB_TRUE

doneNull?:
	leave
	ret


handle_number?:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_INTEGER
	je trueNumber?
	cmp rbx, T_FRACTION
	je trueNumber?
	mov rax, SOB_FALSE
	jmp doneNumber?

trueNumber?:
	mov rax, SOB_TRUE

doneNumber?:
	leave
	ret

; =============================== PRIMITIVE FUNCTIONS =========================
main:

	push rbp
	mov rbp, rsp

; start
	mov rax, qword [sobInt2]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start

	mov rax, qword [sobInt3]
	push rax
	call handle_number?
	add rsp, 8

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start

	mov rax, qword [sobNil]
	push rax
	call handle_number?
	add rsp, 8

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start

	mov rax, qword [sobFrac1_2]
	push rax
	call handle_number?
	add rsp, 8

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end
	mov rsp, rbp
	pop rbp
	ret

