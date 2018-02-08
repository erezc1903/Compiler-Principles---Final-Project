%include "scheme.s"

section .bss

section .data

sobFrac2_3:
	dq MAKE_LITERAL_FRACTION (sobInt2, sobInt3)

sobInt2:
	dq MAKE_LITERAL (T_INTEGER, 2)

sobInt3:
	dq MAKE_LITERAL (T_INTEGER, 3)



section .text

	extern exit, printf, scanf, malloc

	global main

; =============================== PRIMITIVE FUNCTIONS =========================

pair?:
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


boolean?:
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


integer?:
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


null?:
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


number?:
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


char?:
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


string?:
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


vector?:
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


not:
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


rational?:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_FRACTION
	je trueRational?
	cmp rbx, T_INTEGER
	je trueRational?
	mov rax, SOB_FALSE
	jmp doneRational?

trueRational?:
	mov rax, SOB_TRUE

doneRational?:
	leave
	ret


zero?:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_INTEGER
	je chechIfZero
	mov rax, SOB_FALSE
	jmp doneZero?

chechIfZero:
	cmp rax, MAKE_LITERAL(T_INTEGER, 0)
	je isZero
	mov rax, SOB_FALSE
	jmp doneZero?

isZero:
	mov rax, SOB_TRUE
doneZero?:
	leave
	ret


car:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_PAIR
	jne notAPairForCar
	CAR rax
	jmp doneCar

notAPairForCar:
	mov rax, SOB_VOID
doneCar:
	leave
	ret


cdr:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_PAIR
	jne notAPairForCdr
	CDR rax
	jmp doneCdr

notAPairForCdr:
	mov rax, SOB_VOID
doneCdr:
	leave
	ret


numerator:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_FRACTION
	jne notAFractionForNumerator
	NUMERATOR rax
	jmp doneNumerator

notAFractionForNumerator:
	mov rax, SOB_VOID
doneNumerator:
	leave
	ret


denominator:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_FRACTION
	jne notAFractionForDenominator
	DENOMINATOR rax
	jmp doneDenominator

notAFractionForDenominator:
	mov rax, SOB_VOID
doneDenominator:
	leave
	ret

; =============================== PRIMITIVE FUNCTIONS =========================
main:

	push rbp
	mov rbp, rsp

; start

	mov rax, qword [sobFrac2_3]
	push rax
	call numerator
	add rsp, 8

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start

	mov rax, qword [sobFrac2_3]
	push rax
	call denominator
	add rsp, 8

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end
	mov rsp, rbp
	pop rbp
	ret

