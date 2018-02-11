%include "scheme.s"

section .bss

section .data

append:
	dq SOB_UNDEFINED

apply:
	dq SOB_UNDEFINED

greaterThen:
	dq SOB_UNDEFINED

equal:
	dq SOB_UNDEFINED

lessThen:
	dq SOB_UNDEFINED

plus:
	dq SOB_UNDEFINED

divide:
	dq SOB_UNDEFINED

multiply:
	dq SOB_UNDEFINED

substract:
	dq SOB_UNDEFINED

boolean?:
	dq SOB_UNDEFINED

car:
	dq SOB_UNDEFINED

cdr:
	dq SOB_UNDEFINED

charToInteger:
	dq SOB_UNDEFINED

char?:
	dq SOB_UNDEFINED

cons:
	dq SOB_UNDEFINED

denominator:
	dq SOB_UNDEFINED

eq?:
	dq SOB_UNDEFINED

integer?:
	dq SOB_UNDEFINED

integerToChar:
	dq SOB_UNDEFINED

list:
	dq SOB_UNDEFINED

makeString:
	dq SOB_UNDEFINED

makeVector:
	dq SOB_UNDEFINED

map:
	dq SOB_UNDEFINED

not:
	dq SOB_UNDEFINED

null?:
	dq SOB_UNDEFINED

number?:
	dq SOB_UNDEFINED

numerator:
	dq SOB_UNDEFINED

pair?:
	dq SOB_UNDEFINED

procedure?:
	dq SOB_UNDEFINED

rational?:
	dq SOB_UNDEFINED

remainder:
	dq SOB_UNDEFINED

setCar:
	dq SOB_UNDEFINED

setCdr:
	dq SOB_UNDEFINED

stringLength:
	dq SOB_UNDEFINED

stringRef:
	dq SOB_UNDEFINED

stringSet:
	dq SOB_UNDEFINED

stringToSymbol:
	dq SOB_UNDEFINED

string?:
	dq SOB_UNDEFINED

symbol?:
	dq SOB_UNDEFINED

symbolToString:
	dq SOB_UNDEFINED

vector:
	dq SOB_UNDEFINED

vectorLength:
	dq SOB_UNDEFINED

vectorRef:
	dq SOB_UNDEFINED

vectorSet:
	dq SOB_UNDEFINED

vector?:
	dq SOB_UNDEFINED

zero?:
	dq SOB_UNDEFINED

sobNegInt10:
	dq MAKE_LITERAL (T_INTEGER, -10)



section .text

	extern exit, printf, scanf, malloc

	global main

main:

; =============================== PRIMITIVE FUNCTIONS =========================
	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, pair?_code
	jmp end_pair?_code


pair?_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_PAIR
	je truePair?
	mov rax, SOB_FALSE
	jmp donePair?

truePair?:
	mov rax, SOB_TRUE

donePair?:
	mov rsp, rbp
	pop rbp
	ret

end_pair?_code:
	mov rax, [rax]
	mov qword [pair?], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, boolean?_code
	jmp end_boolean?_code


boolean?_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_BOOL
	je trueBoolean?
	mov rax, SOB_FALSE
	jmp doneBoolean?

trueBoolean?:
	mov rax, SOB_TRUE

doneBoolean?:
	mov rsp, rbp
	pop rbp
	ret

end_boolean?_code:
	mov rax, [rax]
	mov qword [boolean?], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, integer?_code
	jmp end_integer?_code


integer?_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_INTEGER
	je trueInteger?
	mov rax, SOB_FALSE
	jmp doneInteger?

trueInteger?:
	mov rax, SOB_TRUE

doneInteger?:
	mov rsp, rbp
	pop rbp
	ret

end_integer?_code:
	mov rax, [rax]
	mov qword [integer?], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, null?_code
	jmp end_null?_code


null?_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_NIL
	je trueNull?
	mov rax, SOB_FALSE
	jmp doneNull?

trueNull?:
	mov rax, SOB_TRUE

doneNull?:
	mov rsp, rbp
	pop rbp
	ret

end_null?_code:
	mov rax, [rax]
	mov qword [null?], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, number?_code
	jmp end_number?_code


number?_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
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
	mov rsp, rbp
	pop rbp
	ret

end_number?_code:
	mov rax, [rax]
	mov qword [number?], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, char?_code
	jmp end_char?_code


char?_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_CHAR
	je trueChar?
	mov rax, SOB_FALSE
	jmp doneChar?

trueChar?:
	mov rax, SOB_TRUE

doneChar?:
	mov rsp, rbp
	pop rbp
	ret

end_char?_code:
	mov rax, [rax]
	mov qword [char?], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, string?_code
	jmp end_string?_code


string?_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_STRING
	je trueString?
	mov rax, SOB_FALSE
	jmp doneString?

trueString?:
	mov rax, SOB_TRUE

doneString?:
	mov rsp, rbp
	pop rbp
	ret

end_string?_code:
	mov rax, [rax]
	mov qword [string?], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, vector?_code
	jmp end_vector?_code


vector?_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_VECTOR
	je trueVector?
	mov rax, SOB_FALSE
	jmp doneVector?

trueVector?:
	mov rax, SOB_TRUE

doneVector?:
	mov rsp, rbp
	pop rbp
	ret

end_vector?_code:
	mov rax, [rax]
	mov qword [vector?], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, not_code
	jmp end_not_code


not_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
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
	mov rsp, rbp
	pop rbp
	ret

end_not_code:
	mov rax, [rax]
	mov qword [not], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, rational?_code
	jmp end_rational?_code


rational?_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
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
	mov rsp, rbp
	pop rbp
	ret

end_rational?_code:
	mov rax, [rax]
	mov qword [rational?], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, zero?_code
	jmp end_zero?_code


zero?_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
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
	mov rsp, rbp
	pop rbp
	ret

end_zero?_code:
	mov rax, [rax]
	mov qword [zero?], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, car_code
	jmp end_car_code


car_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_PAIR
	jne notAPairForCar
	CAR rax
	jmp doneCar

notAPairForCar:
	mov rax, SOB_VOID
doneCar:
	mov rsp, rbp
	pop rbp
	ret

end_car_code:
	mov rax, [rax]
	mov qword [car], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, cdr_code
	jmp end_cdr_code


cdr_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_PAIR
	jne notAPairForCdr
	CDR rax
	jmp doneCdr

notAPairForCdr:
	mov rax, SOB_VOID
doneCdr:
	mov rsp, rbp
	pop rbp
	ret

end_cdr_code:
	mov rax, [rax]
	mov qword [cdr], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, numerator_code
	jmp end_numerator_code

numerator_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_FRACTION
	jne notAFractionForNumerator
	NUMERATOR rax
	jmp doneNumerator

notAFractionForNumerator:
	mov rax, SOB_VOID
doneNumerator:
	mov rsp, rbp
	pop rbp
	ret

end_numerator_code:
	mov rax, [rax]
	mov qword [numerator], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, denominator_code
	jmp end_denominator_code


denominator_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_FRACTION
	jne notAFractionForDenominator
	DENOMINATOR rax
	jmp doneDenominator

notAFractionForDenominator:
	mov rax, SOB_VOID
doneDenominator:
	mov rsp, rbp
	pop rbp
	ret

end_denominator_code:
	mov rax, [rax]
	mov qword [denominator], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, integer_to_char_code
	jmp end_integer_to_char_code

integer_to_char_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne badArgCount
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_INTEGER
	jne badInput
 ; not of type integer - can't convert
	mov rbx, rax
	DATA rbx
	cmp rbx, 0
	jl badInput
 ; negative integer - can't convert to char because it doesn't have an ascii representation of type integer - can't convert
	mov rbx, rax
	DATA rbx
	cmp rbx, 256
	jge badInput
 ; integer to large - can't convert to char because it doesn't have an ascii representation of type integer - can't convert
	xor rax, (T_CHAR ^ T_INTEGER)
	jmp doneIntegerToChar

badInput:

	mov rax, SOB_VOID
	jmp doneIntegerToChar
badArgCount:

	mov rax, SOB_VOID
doneIntegerToChar:
	mov rsp, rbp
	pop rbp
	ret

end_integer_to_char_code:
	mov rax, [rax]
	mov qword [integerToChar], rax

; =============================== PRIMITIVE FUNCTIONS =========================

start_of_instructions:

	push rbp
	mov rbp, rsp

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobNegInt10]
	;code gen for constant end
	push rax

	push 1
	mov rax, [integerToChar]
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

