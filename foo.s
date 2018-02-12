%include "scheme.s"

section .bss

section .data

append:
	dq SOB_UNDEFINED

apply:
	dq SOB_UNDEFINED

lessThan:
	dq SOB_UNDEFINED

equal:
	dq SOB_UNDEFINED

greaterThan:
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

sobInt1:
	dq MAKE_LITERAL (T_INTEGER, 1)

sobInt2:
	dq MAKE_LITERAL (T_INTEGER, 2)

sobInt3:
	dq MAKE_LITERAL (T_INTEGER, 3)

sobFalse:
	dq SOB_FALSE


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
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .badArgCount
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_PAIR
	je .truePair?
	mov rax, SOB_FALSE
	jmp .done

.truePair?:
	mov rax, SOB_TRUE

	jmp .done
.badArgCount:

	mov rax, SOB_VOID
.done:
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
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .badArgCount
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_BOOL
	je .trueBoolean?
	mov rax, SOB_FALSE
	jmp .done

.trueBoolean?:
	mov rax, SOB_TRUE

	jmp .done
.badArgCount:

	mov rax, SOB_VOID
.done:
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
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .badArgCount
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_INTEGER
	je .trueInteger?
	mov rax, SOB_FALSE
	jmp .done

.trueInteger?:
	mov rax, SOB_TRUE

	jmp .done
.badArgCount:

	mov rax, SOB_VOID
.done:
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
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .badArgCount
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_NIL
	je .trueNull?
	mov rax, SOB_FALSE
	jmp .done

.trueNull?:
	mov rax, SOB_TRUE

	jmp .done
.badArgCount:

	mov rax, SOB_VOID
.done:
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
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .badArgCount
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_INTEGER
	je .trueNumber?
	cmp rbx, T_FRACTION
	je .trueNumber?
	mov rax, SOB_FALSE
	jmp .done

.trueNumber?:
	mov rax, SOB_TRUE

	jmp .done
.badArgCount:

	mov rax, SOB_VOID
.done:
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
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .badArgCount
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_CHAR
	je .trueChar?
	mov rax, SOB_FALSE
	jmp .done

.trueChar?:
	mov rax, SOB_TRUE

	jmp .done
.badArgCount:

	mov rax, SOB_VOID
.done:
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
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .badArgCount
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_STRING
	je .trueString?
	mov rax, SOB_FALSE
	jmp .done

.trueString?:
	mov rax, SOB_TRUE

	jmp .done
.badArgCount:

	mov rax, SOB_VOID
.done:
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
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .badArgCount
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_VECTOR
	je .trueVector?
	mov rax, SOB_FALSE
	jmp .done

.trueVector?:
	mov rax, SOB_TRUE

	jmp .done
.badArgCount:

	mov rax, SOB_VOID
.done:

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
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .badArgCount
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_BOOL
	jne .retFalse
	mov rbx, rax
	cmp rbx, SOB_TRUE
	je .retFalse
	mov rax, SOB_TRUE
	jmp .done

.retFalse:
	mov rax, SOB_FALSE

	jmp .done
.badArgCount:

	mov rax, SOB_VOID
.done:
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
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .badArgCount
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_FRACTION
	je .trueRational?
	cmp rbx, T_INTEGER
	je .trueRational?
	mov rax, SOB_FALSE
	jmp .done

.trueRational?:
	mov rax, SOB_TRUE

	jmp .done
.badArgCount:

	mov rax, SOB_VOID
.done:
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
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .notANumber
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_INTEGER
	je .chechIfZero
	mov rax, SOB_FALSE
	jmp .done

.chechIfZero:
	cmp rax, MAKE_LITERAL(T_INTEGER, 0)
	je .isZero
	mov rax, SOB_FALSE
	jmp .done

.isZero:
	mov rax, SOB_TRUE
	jmp .done
.notANumber:

	mov rax, SOB_VOID
.done:
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
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .notAPair
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_PAIR
	jne .notAPair
	CAR rax
	jmp .done

.notAPair:
	mov rax, SOB_VOID
.done:
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
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .notAPair
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_PAIR
	jne .notAPair
	CDR rax
	jmp .done

.notAPair:
	mov rax, SOB_VOID
.done:
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
	MAKE_LITERAL_CLOSURE rax, rbx, cons_code
	jmp end_cons_code


cons_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*3]
	cmp rax, 2
	jne .badArgCount
	mov rdi, 8
	call malloc
	mov rbx, rax ; will hold the car address 
	mov rcx, qword [rbp + 4*8]
	mov qword [rbx], rcx
	push rbx
	mov rdi, 8
	call malloc
	mov rdx, rax ; will hold the cdr address 
	mov rcx, qword [rbp + 5*8]
	mov qword [rdx], rcx
	push rdx
	mov rdi, 8
	call malloc ; rax will hold the address of the new pair
	pop rdx
	pop rbx
	MAKE_MALLOC_LITERAL_PAIR rax, rbx, rdx
	mov rax, [rax]
	jmp .done

.badArgCount:

	mov rax, SOB_VOID
.done:
	mov rsp, rbp
	pop rbp
	ret

end_cons_code:
	mov rax, [rax]
	mov qword [cons], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, numerator_code
	jmp end_numerator_code

numerator_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .notAFraction
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_FRACTION
	jne .notAFraction
	NUMERATOR rax
	jmp .done

.notAFraction:
	mov rax, SOB_VOID
.done:
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
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .notAFraction
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_FRACTION
	jne .notAFraction
	DENOMINATOR rax
	jmp .done

.notAFraction:
	mov rax, SOB_VOID
.done:
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
	jne .badArgCount
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_INTEGER
	jne .badInput ; not of type integer - can't convert
	mov rbx, rax
	DATA rbx
	cmp rbx, 0
	jl .badInput ; negative integer - can't convert to char because it doesn't have an ascii representation of type integer - can't convert
	mov rbx, rax
	DATA rbx
	cmp rbx, 256
	jge .badInput ; integer to large - can't convert to char because it doesn't have an ascii representation of type integer - can't convert
	xor rax, (T_CHAR ^ T_INTEGER)
	jmp .done

.badInput:

	mov rax, SOB_VOID
	jmp .done
.badArgCount:

	mov rax, SOB_VOID
.done:
	mov rsp, rbp
	pop rbp
	ret

end_integer_to_char_code:
	mov rax, [rax]
	mov qword [integerToChar], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, char_to_integer_code
	jmp end_char_to_integer_code

char_to_integer_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .badArgCount
	mov rax, qword [rbp + 8*4]
	mov rbx, rax
	TYPE rbx
	cmp rbx, T_CHAR
	jne .badInput ; not of type char - can't convert
	xor rax, (T_INTEGER ^ T_CHAR)
	jmp .done

.badInput:

	mov rax, SOB_VOID
	jmp .done
.badArgCount:

	mov rax, SOB_VOID
.done:
	mov rsp, rbp
	pop rbp
	ret

end_char_to_integer_code:
	mov rax, [rax]
	mov qword [charToInteger], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, greater_than_code
	jmp end_greater_than_code

greater_than_code:
	push rbp
	mov rbp, rsp
	mov rcx, 0 ; rcx is a counter for the number of arguments
.checkIfArgsAreNumbers:

	cmp rcx, qword [rbp + 8*3]
	je .check_greater_than
	mov rbx, qword [rbp + 8*(4 + rcx)]
	TYPE rbx
	cmp rbx, T_INTEGER
	je .incCounter
	cmp rbx, T_FRACTION
	je .incCounter
	jmp .badArgs
.incCounter:

	inc rcx
	jmp .checkIfArgsAreNumbers
.check_number_of_args:

	mov rcx, qword [rbp + 8*3]
	cmp rcx, 1
	je .doneCheckGT
.check_greater_than:

	mov rcx, 0 ; rcx is a counter for the number of arguments
	mov r9, qword [rbp + 8*3]
	dec r9
.check_greater_than_loop:

	cmp rcx,r9
	je .doneCheckGT

	mov rbx, qword [rbp + 8*(4 + rcx)]
	DATA rbx
	mov rdx, qword [rbp + 8*(5 + rcx)]
	DATA rdx
	cmp rbx, rdx
	jle .retFalse
	inc rcx
	jmp .check_greater_than_loop
.retFalse:

	mov rax, SOB_FALSE
	jmp .done
.doneCheckGT:

	mov rax, SOB_TRUE
	jmp .done
.badArgs:

	mov rax, SOB_VOID
.done:
	mov rsp, rbp
	pop rbp
	ret

end_greater_than_code:
	mov rax, [rax]
	mov qword [greaterThan], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, less_than_code
	jmp end_less_than_code

less_than_code:
	push rbp
	mov rbp, rsp
	mov rcx, 0 ; rcx is a counter for the number of arguments
.checkIfArgsAreNumbers:

	cmp rcx, qword [rbp + 8*3]
	je .check_less_than
	mov rbx, qword [rbp + 8*(4 + rcx)]
	TYPE rbx
	cmp rbx, T_INTEGER
	je .incCounter
	cmp rbx, T_FRACTION
	je .incCounter
	jmp .badArgs
.incCounter:

	inc rcx
	jmp .checkIfArgsAreNumbers
.check_number_of_args:

	mov rcx, qword [rbp + 8*3]
	cmp rcx, 1
	je .doneCheckLT
.check_less_than:

	mov rcx, 0 ; rcx is a counter for the number of arguments
	mov r9, qword [rbp + 8*3]
	dec r9
.check_less_than_loop:

	cmp rcx,r9
	je .doneCheckLT

	mov rbx, qword [rbp + 8*(4 + rcx)]
	DATA rbx
	mov rdx, qword [rbp + 8*(5 + rcx)]
	DATA rdx
	cmp rbx, rdx
	jge .retFalse
	inc rcx
	jmp .check_less_than_loop
.retFalse:

	mov rax, SOB_FALSE
	jmp .done
.doneCheckLT:

	mov rax, SOB_TRUE
	jmp .done
.badArgs:

	mov rax, SOB_VOID
.done:
	mov rsp, rbp
	pop rbp
	ret

end_less_than_code:
	mov rax, [rax]
	mov qword [lessThan], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, equal_code
	jmp end_equal_code

equal_code:
	push rbp
	mov rbp, rsp
	mov rcx, 0 ; rcx is a counter for the number of arguments
.checkIfArgsAreNumbers:

	cmp rcx, qword [rbp + 8*3]
	je .check_equal
	mov rbx, qword [rbp + 8*(4 + rcx)]
	TYPE rbx
	cmp rbx, T_INTEGER
	je .incCounter
	cmp rbx, T_FRACTION
	je .incCounter
	jmp .badArgs
.incCounter:

	inc rcx
	jmp .checkIfArgsAreNumbers
.check_number_of_args:

	mov rcx, qword [rbp + 8*3]
	cmp rcx, 1
	je .doneCheckEQ
.check_equal:

	mov rcx, 0 ; rcx is a counter for the number of arguments
	mov r9, qword [rbp + 8*3]
	dec r9
.check_equal_loop:

	cmp rcx,r9
	je .doneCheckEQ

	mov rbx, qword [rbp + 8*(4 + rcx)]
	DATA rbx
	mov rdx, qword [rbp + 8*(5 + rcx)]
	DATA rdx
	cmp rbx, rdx
	jne .retFalse
	inc rcx
	jmp .check_equal_loop
.retFalse:

	mov rax, SOB_FALSE
	jmp .done
.doneCheckEQ:

	mov rax, SOB_TRUE
	jmp .done
.badArgs:

	mov rax, SOB_VOID
.done:
	mov rsp, rbp
	pop rbp
	ret

end_equal_code:
	mov rax, [rax]
	mov qword [equal], rax

; =============================== PRIMITIVE FUNCTIONS =========================

start_of_instructions:

	push rbp
	mov rbp, rsp

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobFalse]
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
	mov rdi, qword [rbp + 3*8]
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
	cmp r10, 0
	je done_copy_env102
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop102
done_copy_env102:

	mov qword [rbx + 8*r15], 0
make_closure102:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda102
	jmp endLabel102

bodyOfLambda102:
	push rbp
	mov rbp, rsp
; start of creating a closure of lambda-simple 2

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
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
	cmp r10, 1
	je done_copy_env103
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop103
done_copy_env103:

	mov qword [rbx + 8*r15], 0
make_closure103:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda103
	jmp endLabel103

bodyOfLambda103:
	push rbp
	mov rbp, rsp
; start of creating a closure of lambda-simple 3

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
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
	cmp r10, 2
	je done_copy_env104
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop104
done_copy_env104:

	mov qword [rbx + 8*r15], 0
make_closure104:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda104
	jmp endLabel104

bodyOfLambda104:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 2*8]
	mov rax, qword [rax + 0*8]
	cmp rax, SOB_FALSE
	je L0
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 1*8]
	mov rax, qword [rax + 0*8]

	jmp Lend0
L0:
		mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]

Lend0:

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

