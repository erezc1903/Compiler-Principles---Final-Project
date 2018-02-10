%include "scheme.s"

section .bss

section .data

y:
	dq SOB_UNDEFINED

sobInt3:
	dq MAKE_LITERAL (T_INTEGER, 3)

sobInt2:
	dq MAKE_LITERAL (T_INTEGER, 2)



section .text

	extern exit, printf, scanf, malloc

	global main

main:

; =============================== PRIMITIVE FUNCTIONS =========================
; =============================== PRIMITIVE FUNCTIONS =========================
	push rbp
	mov rbp, rsp

; start
	; codegen for const start
	mov rax, qword [sobInt3]
	;code gen for constant end
	mov rbx, y
	mov qword [rbx], rax
	mov rax, SOB_VOID

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
	mov rax, [y]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt2]
	;code gen for constant end
	push rax

	push 1
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure101:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda101
	jmp endLabel101

bodyOfLambda101:
	push rbp
	mov rbp, rsp
	mov rax, [y]
	mov rsp, rbp
	pop rbp
	ret

endLabel101:
	mov rax, [rax]

; end of creating a closure of lambda-simple 0

	mov rcx, rax
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure101
	mov rbx, rax
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE rax
	call rax
	add rsp, 8*1
	jmp done_closure101
not_a_closure101:

	mov rax, SOB_VOID
done_closure101:

	add rsp, 8*2

; end of applic of lambda-simple code: 

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end
	mov rsp, rbp
	pop rbp
	ret

