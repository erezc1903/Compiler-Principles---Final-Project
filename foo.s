%include "scheme.s"

section .bss

section .data

sobInt1:
	dq MAKE_LITERAL (T_INTEGER, 1)

sobInt2:
	dq MAKE_LITERAL (T_INTEGER, 2)

sobInt3:
	dq MAKE_LITERAL (T_INTEGER, 3)

sobTrue:
	dq SOB_TRUE



section .text

	extern exit, printf, scanf, malloc

	global main

main:

; =============================== PRIMITIVE FUNCTIONS =========================
; =============================== PRIMITIVE FUNCTIONS =========================
	push rbp
	mov rbp, rsp

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobTrue]
	;code gen for constant end
	push rax

	push 1
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt3]
	;code gen for constant end
	push rax

	push 1
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt2]
	;code gen for constant end
	push rax

	push 1
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt1]
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
; start of creating a closure of lambda-simple 1

	xor rax, rax
	mov rdi, 8
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 16
	call malloc
	mov rbx, rax
; rbx hold a pointer to store the previous environment
	pop rdx
	push rdx
	push rbx
	xor rax, rax
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
	pop rbx
	pop rdx

	mov rcx, 0
copy_args_loop102:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args102
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop102

done_copy_args102:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop102:
	cmp r10, 1
	je make_closure102
	lea r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop102

make_closure102:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda102
	jmp endLabel102

bodyOfLambda102:
	push rbp
	mov rbp, rsp
; start of creating a closure of lambda-simple 2

	xor rax, rax
	mov rdi, 8
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 24
	call malloc
	mov rbx, rax
; rbx hold a pointer to store the previous environment
	pop rdx
	push rdx
	push rbx
	xor rax, rax
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
	pop rbx
	pop rdx

	mov rcx, 0
copy_args_loop103:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args103
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop103

done_copy_args103:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop103:
	cmp r10, 2
	je make_closure103
	lea r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop103

make_closure103:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda103
	jmp endLabel103

bodyOfLambda103:
	push rbp
	mov rbp, rsp
; start of creating a closure of lambda-simple 3

	xor rax, rax
	mov rdi, 8
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 32
	call malloc
	mov rbx, rax
; rbx hold a pointer to store the previous environment
	pop rdx
	push rdx
	push rbx
	xor rax, rax
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
	pop rbx
	pop rdx

	mov rcx, 0
copy_args_loop104:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args104
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop104

done_copy_args104:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop104:
	cmp r10, 3
	je make_closure104
	lea r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop104

make_closure104:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda104
	jmp endLabel104

bodyOfLambda104:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + (4+0)*8]
	cmp rax, SOB_FALSE
	jne Lend101
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 2*8]
	mov rax, qword [rax + 0*8]
	cmp rax, SOB_FALSE
	jne Lend101
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 1*8]
	mov rax, qword [rax + 0*8]
	cmp rax, SOB_FALSE
	jne Lend101
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	cmp rax, SOB_FALSE
	jne Lend101
Lend101:

	pop rbp
	ret

endLabel104:
	mov rax, [rax] ; rax now hold the closure object 

; end of creating a closure of lambda-simple 3

	pop rbp
	ret

endLabel103:
	mov rax, [rax] ; rax now hold the closure object 

; end of creating a closure of lambda-simple 2

	pop rbp
	ret

endLabel102:
	mov rax, [rax] ; rax now hold the closure object 

; end of creating a closure of lambda-simple 1

	pop rbp
	ret

endLabel101:
	mov rax, [rax] ; rax now hold the closure object 

; end of creating a closure of lambda-simple 0

	mov rcx, rax
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure104
	mov rbx, rax
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE rax
	call rax
	add rsp, 8*1
	jmp done_closure104
not_a_closure104:

	mov rax, SOB_VOID
done_closure104:

	add rsp, 8*2

; end of applic of lambda-simple code: 

	mov rcx, rax
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure103
	mov rbx, rax
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE rax
	call rax
	add rsp, 8*1
	jmp done_closure103
not_a_closure103:

	mov rax, SOB_VOID
done_closure103:

	add rsp, 8*2

; end of applic of lambda-simple code: 

	mov rcx, rax
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure102
	mov rbx, rax
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE rax
	call rax
	add rsp, 8*1
	jmp done_closure102
not_a_closure102:

	mov rax, SOB_VOID
done_closure102:

	add rsp, 8*2

; end of applic of lambda-simple code: 

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

