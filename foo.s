%include "scheme.s"

section .bss

section .data

not:
	dq SOB_UNDEFINED

sobInt2:
	dq MAKE_LITERAL (T_INTEGER, 2)

sobInt1:
	dq MAKE_LITERAL (T_INTEGER, 1)

sobFalse:
	dq SOB_FALSE
sobInt3:
	dq MAKE_LITERAL (T_INTEGER, 3)



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


handle_char?:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_CHAR
	je trueChar?
	mov rax, SOB_FALSE
	jmp doneChar?

trueChar?:
	mov rax, SOB_TRUE

doneChar?:
	leave
	ret


handle_string?:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_STRING
	je trueString?
	mov rax, SOB_FALSE
	jmp doneString?

trueString?:
	mov rax, SOB_TRUE

doneString?:
	leave
	ret


handle_vector?:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_VECTOR
	je trueVector?
	mov rax, SOB_FALSE
	jmp doneVector?

trueVector?:
	mov rax, SOB_TRUE

doneVector?:
	leave
	ret


handle_not:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_BOOL
	jne retFalse
	mov rbx, rax
	cmp rbx, SOB_TRUE
	je retFalse
	mov rax, SOB_TRUE
	jmp doneNot

retFalse:
	mov rax, SOB_FALSE

doneNot:
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

	mov rax, qword [sobInt1]
	cmp rax, SOB_FALSE
	je L0
	mov rax, qword [sobInt2]

	jmp Lend0
L0:
		mov rax, qword [sobInt3]

Lend0:

	push rax
	call handle_not
	add rsp, 8

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start

	mov rax, qword [sobInt1]
	cmp rax, SOB_FALSE
	je L1
	mov rax, qword [sobFalse]

	jmp Lend1
L1:
		mov rax, qword [sobInt3]

Lend1:

	push rax
	call handle_not
	add rsp, 8

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end
	mov rsp, rbp
	pop rbp
	ret

