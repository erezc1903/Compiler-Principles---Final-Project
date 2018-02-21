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

subtract:
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

maplist:
	dq SOB_UNDEFINED

map1:
	dq SOB_UNDEFINED

append_base_case:
	dq SOB_UNDEFINED

append_helper:
	dq SOB_UNDEFINED

sobVoid:
	dq SOB_VOID

sobFalse:
	dq SOB_FALSE
sobTrue:
	dq SOB_TRUE

sobNil:
	dq SOB_NIL

sobInt1:
	dq MAKE_LITERAL (T_INTEGER, 1)

sobPair8:
	dq MAKE_LITERAL_PAIR (sobInt2, sobPair6)

sobInt2:
	dq MAKE_LITERAL (T_INTEGER, 2)

sobPair6:
	dq MAKE_LITERAL_PAIR (sobInt3, sobNil)

sobInt3:
	dq MAKE_LITERAL (T_INTEGER, 3)

sobPair4:
	dq MAKE_LITERAL_PAIR (sobInt1, sobPair8)

sobPair1:
	dq MAKE_LITERAL_PAIR (sobTrue, sobPair2)

sobPair2:
	dq MAKE_LITERAL_PAIR (sobTrue, sobNil)

sobPair0:
	dq MAKE_LITERAL_PAIR (sobTrue, sobPair1)

sobUndef:
	dq SOB_UNDEFINED



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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_PAIR
	je .truePair?
	mov rax, sobFalse
	jmp .done

.truePair?:
	mov rax, sobTrue

	jmp .done
.badArgCount:

	mov rax, sobVoid
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_BOOL
	je .trueBoolean?
	mov rax, sobFalse
	jmp .done

.trueBoolean?:
	mov rax, sobTrue

	jmp .done
.badArgCount:

	mov rax, sobVoid
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_INTEGER
	je .trueInteger?
	mov rax, sobFalse
	jmp .done

.trueInteger?:
	mov rax, sobTrue

	jmp .done
.badArgCount:

	mov rax, sobVoid
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_NIL
	je .trueNull?
	mov rax, sobFalse
	jmp .done

.trueNull?:
	mov rax, sobTrue

	jmp .done
.badArgCount:

	mov rax, sobVoid
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_INTEGER
	je .trueNumber?
	cmp rbx, T_FRACTION
	je .trueNumber?
	mov rax, sobFalse
	jmp .done

.trueNumber?:
	mov rax, sobTrue

	jmp .done
.badArgCount:

	mov rax, sobVoid
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_CHAR
	je .trueChar?
	mov rax, sobFalse
	jmp .done

.trueChar?:
	mov rax, sobTrue

	jmp .done
.badArgCount:

	mov rax, sobVoid
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_STRING
	je .trueString?
	mov rax, sobFalse
	jmp .done

.trueString?:
	mov rax, sobTrue

	jmp .done
.badArgCount:

	mov rax, sobVoid
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_VECTOR
	je .trueVector?
	mov rax, sobFalse
	jmp .done

.trueVector?:
	mov rax, sobTrue

	jmp .done
.badArgCount:

	mov rax, sobVoid
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_BOOL
	jne .retFalse
	mov rbx, r10
	cmp rbx, sobTrue
	je .retFalse
	mov rax, sobTrue
	jmp .done

.retFalse:
	mov rax, sobFalse

	jmp .done
.badArgCount:

	mov rax, sobVoid
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_FRACTION
	je .trueRational?
	cmp rbx, T_INTEGER
	je .trueRational?
	mov rax, sobFalse
	jmp .done

.trueRational?:
	mov rax, sobTrue

	jmp .done
.badArgCount:

	mov rax, sobVoid
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_INTEGER
	je .chechIfZero
	mov rax, sobFalse
	jmp .done

.chechIfZero:
	cmp r10, MAKE_LITERAL(T_INTEGER, 0)
	je .isZero
	mov rax, sobFalse
	jmp .done

.isZero:
	mov rax, sobTrue
	jmp .done
.notANumber:

	mov rax, sobVoid
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_PAIR
	jne .notAPair
	DATA_UPPER r10
	add r10, start_of_data
	mov rax, r10
	jmp .done

.notAPair:
	mov rax, sobVoid
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_PAIR
	jne .notAPair
	DATA_LOWER r10
	add r10, start_of_data
	mov rax, r10
	jmp .done

.notAPair:
	mov rax, sobVoid
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
	mov r13, qword [rbp + 8*3]
	cmp r13, 2
	jne .badArgCount
	mov rdi, 8
	call malloc
	mov r14, [rbp + 4*8]
	mov r15, [rbp + 5*8]
	sub r14, start_of_data
	shl r14, 30
	sub r15, start_of_data
	or r14, r15
	shl r14, 4
	or r14, T_PAIR
	mov [rax], r14
	jmp .done

.badArgCount:

	mov rax, sobVoid
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_INTEGER
	je .done
	cmp rbx, T_FRACTION
	jne .notAFraction
	RUNTIME_NUMERATOR r10
	mov rax, r10
	jmp .done

.notAFraction:
	mov rax, sobVoid
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_INTEGER
	je .returnOne
	cmp rbx, T_FRACTION
	jne .notAFraction
	RUNTIME_DENOMINATOR r10
	mov rax, r10
	jmp .done

.notAFraction:
	mov rax, sobVoid
	jmp .done

.returnOne:

	mov rdi, 8
	mov qword [rax], MAKE_LITERAL(T_INTEGER, 1)
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_INTEGER
	jne .badInput ; not of type integer - can't convert
	mov rbx, r10
	DATA rbx
	cmp rbx, 0
	jl .badInput ; negative integer - can't convert to char because it doesn't have an ascii representation of type integer - can't convert
	mov rbx, r10
	DATA rbx
	cmp rbx, 256
	jge .badInput ; integer to large - can't convert to char because it doesn't have an ascii representation of type integer - can't convert
	xor r10, (T_CHAR ^ T_INTEGER)
	mov rdi, 8
	call malloc
	mov [rax], r10
	jmp .done

.badInput:

	mov rax, sobVoid
	jmp .done
.badArgCount:

	mov rax, sobVoid
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
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_CHAR
	jne .badInput ; not of type char - can't convert
	xor r10, (T_INTEGER ^ T_CHAR)
	mov rdi, 8
	call malloc
	mov [rax], r10
	jmp .done

.badInput:

	mov rax, sobVoid
	jmp .done
.badArgCount:

	mov rax, sobVoid
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
	MAKE_LITERAL_CLOSURE rax, rbx, plus_code
	jmp end_plus_code

plus_code:
	push rbp
	mov rbp, rsp
	mov rcx, 0 ; rcx is a counter for the number of arguments
.checkIfArgsAreNumbers:

	cmp rcx, qword [rbp + 8*3]
	je .make_addition
	mov rax, qword [rbp + 8*(4 + rcx)]
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_INTEGER
	je .incCounter
	cmp rbx, T_FRACTION
	je .incCounter
	jmp .badArgs
.incCounter:

	inc rcx
	jmp .checkIfArgsAreNumbers
.make_addition:

	mov rcx, 0 ; rcx is a counter for the number of arguments
	mov rdx, 0 ; rdx is the accumulator 
.addition_loop:

	cmp rcx, qword [rbp + 8*3]
	je .doneAddition

	mov rax, qword [rbp + 8*(4 + rcx)]
	mov r10, [rax]
	mov rbx, r10
	DATA rbx
	add rdx, rbx
	inc rcx
	jmp .addition_loop
.doneAddition:

	mov r10, rdx
	shl r10, 4
	or r10, T_INTEGER
	mov rdi, 8
	call malloc
	mov qword [rax], r10
	jmp .done
.badArgs:

	mov rax, sobVoid
.done:
	mov rsp, rbp
	pop rbp
	ret

end_plus_code:
	mov rax, [rax]
	mov qword [plus], rax

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
	mov rbx, [rbx]
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
	mov rbx, [rbx]
	DATA rbx
	mov rdx, qword [rbp + 8*(5 + rcx)]
	mov rdx, [rdx]
	DATA rdx
	cmp rbx, rdx
	jle .retFalse
	inc rcx
	jmp .check_greater_than_loop
.retFalse:

	mov rax, sobFalse
	jmp .done
.doneCheckGT:

	mov rax, sobTrue
	jmp .done
.badArgs:

	mov rax, sobVoid
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
	mov rbx, [rbx]
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
	mov rbx, [rbx]
	DATA rbx
	mov rdx, qword [rbp + 8*(5 + rcx)]
	mov rdx, [rdx]
	DATA rdx
	cmp rbx, rdx
	jge .retFalse
	inc rcx
	jmp .check_less_than_loop
.retFalse:

	mov rax, sobFalse
	jmp .done
.doneCheckLT:

	mov rax, sobTrue
	jmp .done
.badArgs:

	mov rax, sobVoid
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
	mov rbx, [rbx]
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
	mov rbx, [rbx]
	DATA rbx
	mov rdx, qword [rbp + 8*(5 + rcx)]
	mov rdx, [rdx]
	DATA rdx
	cmp rbx, rdx
	jne .retFalse
	inc rcx
	jmp .check_equal_loop
.retFalse:

	mov rax, sobFalse
	jmp .done
.doneCheckEQ:

	mov rax, sobTrue
	jmp .done
.badArgs:

	mov rax, sobVoid
.done:
	mov rsp, rbp
	pop rbp
	ret

end_equal_code:
	mov rax, [rax]
	mov qword [equal], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, remainder_code
	jmp end_remainder_code

remainder_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*3]
	cmp rax, 2
	jne .badArgCount
	mov rax, qword [rbp + 8*4]
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_INTEGER
	jne .badArgs
	mov rdx, qword [rbp + 8*5]
	mov r10, [rdx]
	mov rdx, r10
	TYPE rdx
	cmp rdx, T_INTEGER
	jne .badArgs
	mov rax, qword [rbp + 8*4]
	mov rax, [rax]
	DATA rax ; rax hold the dividend 
	mov rcx, qword [rbp + 8*5]
	mov r10, [rcx]
	DATA r10 ; r10 holds the divisor
	cmp rax, 0
	jl .firstArgsIsNeg
	cmp r10, 0
	jl .secondArgIsNeg
	mov rdx, 0
	div r10
	mov r15, rdx
	shl r15, 4
	or r15, T_INTEGER
	mov rdi, 8
	call malloc
	mov qword [rax], r15
	jmp .done
.firstArgsIsNeg:

	mov r15, 1
	neg rax
	cmp r10, 0
	jl .secondArgIsNeg
	jmp .devidendIsNeg
.secondArgIsNeg:

	neg r10
	cmp r15, 1
	je .devidendIsNeg
	mov rdx, 0
	div r10
	mov r15, rdx
	shl r15, 4
	or r15, T_INTEGER
	mov rdi, 8
	call malloc
	mov qword [rax], r15
	jmp .done
.devidendIsNeg:

	mov rdx, 0
	div r10
	mov r15, rdx
	neg r15
	shl r15, 4
	or r15, T_INTEGER
	mov rdi, 8
	call malloc
	mov qword [rax], r15
	jmp .done
.badArgCount:
	mov rax, sobVoid
	jmp .done

.badArgs:
	mov rax, sobVoid
.done:
	mov rsp, rbp
	pop rbp
	ret

end_remainder_code:
	mov rax, [rax]
	mov qword [remainder], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, string_length_code
	jmp end_string_length_code


string_length_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .notAString
	mov rax, qword [rbp + 8*4]
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_STRING
	jne .notAString
	mov rax, qword [rbp + 8*4]
	mov r10, [rax]
	STRING_LENGTH r10
	shl r10, 4
	or r10, T_INTEGER
	mov rdi, 8
	call malloc
	mov qword [rax], r10
	jmp .done

.notAString:
	mov rax, sobVoid
.done:
	mov rsp, rbp
	pop rbp
	ret

end_string_length_code:
	mov rax, [rax]
	mov qword [stringLength], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, string_ref_code
	jmp end_string_ref_code


string_ref_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*3]
	cmp rax, 2
	jne .badArgs
	mov rax, qword [rbp + 8*4]
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_STRING
	jne .badArgs
	mov rcx, qword [rbp + 8*5]
	mov r10, [rcx]
	TYPE r10
	cmp r10, T_INTEGER
	jne .badArgs
	mov rbx, qword [rbp + 8*4]
	mov r11, [rbx] ; the string 
	mov r9, qword [rbp + 8*5]
	mov r10, [r9] ; the position in the string
	DATA r10
	STRING_ELEMENTS r11 ; the individual chars of the string
	add r11, r10
	mov rdi, 8
	call malloc
	mov r11, qword [r11]
	shl r11, 4
	or r11, T_CHAR
	mov qword [rax], r11
	jmp .done

.badArgs:
	mov rax, sobVoid
.done:
	mov rsp, rbp
	pop rbp
	ret

end_string_ref_code:
	mov rax, [rax]
	mov qword [stringRef], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, string_set_code
	jmp end_string_set_code


string_set_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*3]
	cmp rax, 3
	jl .badArgs
	mov rax, qword [rbp + 8*4]
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_STRING
	jne .badArgs
	mov rcx, qword [rbp + 8*5]
	mov r10, [rcx]
	TYPE r10
	cmp r10, T_INTEGER
	jne .badArgs
	mov r15, qword [rbp + 8*6]
	mov r15, [r15]
	TYPE r15
	cmp r15, T_CHAR
	jne .badArgs
	mov r13, qword [rbp + 8*4] ; rbx holds a pointer to the string
	mov r13, [r13] ; hold the actual string
	mov r9, qword [rbp + 8*5]
	mov r10, [r9] ; r10 hold k - the position in the string
	DATA r10
	mov rbx, qword [rbp + 8*6] ; r12 holds a pointer to the char to replace the k-th element of the string 
	mov rbx, [rbx]
	DATA rbx
	STRING_ELEMENTS r13
	mov byte [r13 + r10*1], bl
	mov rax, sobVoid
	jmp .done

.badArgs:
	mov rax, sobVoid
.done:
	mov rsp, rbp
	pop rbp
	ret

end_string_set_code:
	mov rax, [rax]
	mov qword [stringSet], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, make_string_code
	jmp end_make_string_code

	make_string_code:

	push rbp
	mov rbp, rsp
	cmp qword [rbp + 3*8], 1
	je oneArgForString601
	cmp qword [rbp + 3*8], 2
	je twoArgForString601
	jmp badInputForString601
oneArgForString601:
	mov rcx, qword [rbp + 4*8]
	mov r10, [rcx]
	TYPE r10
	cmp r10, T_INTEGER
	jne badInputForString601
	mov r12, 0
	mov r8, qword [rbp + 4*8] ; r8 holds a pointer to the string length 
	mov r13, [r8]
	DATA r13
	inc r13
	shl r13, 3
	mov rdi, r13
	call malloc
	mov r13, [r8]
	DATA r13 
	mov qword [rax], r13
	shl qword [rax], 30
	lea r13, [rax + 1*8]
	sub r13, start_of_data
	or qword [rax], r13
	shl qword [rax], TYPE_BITS
	or qword [rax], T_STRING
	mov r15, [r8]
	DATA r15
	mov r14, 0
oneArgLoopForString601:

	cmp r14, r15
	je oneArgLoopEndForString601
	mov qword [rax + 1*8 + r14*1], r12
	inc r14
	jmp oneArgLoopForString601
oneArgLoopEndForString601:

	jmp endLabel601
twoArgForString601:

	mov r8, qword [rbp + 4*8]
	mov r10, [r8]
	TYPE r10
	cmp r10, T_INTEGER
	jne badInputForString601
	mov r11, [r8]
	DATA r11
	cmp r11, 0
	jl badInputForString601
	mov r8, qword [rbp + 4*8] ; length of the string
	mov r12, qword [rbp + 5*8] ; elements of the string
	mov r12, [r12]
	DATA r12
	mov r11, [r8]
	DATA r11
	inc r11
	shl r11, 3
	mov rdi, r11
	call malloc
	mov r11, [r8]
	DATA r11
	mov qword [rax], r11
	shl qword [rax], 30
	lea r11, [rax + 1*8]
	sub r11, start_of_data
	or qword [rax], r11
	shl qword [rax], TYPE_BITS
	or qword [rax], T_STRING
	mov r15, [r8]
	DATA r15
	mov r14, 0
twoArgLoopForString601:

	cmp r14, r15
	je twoArgLoopEndForString601
	mov qword [rax + 1*8 + r14*1], r12
	inc r14
	jmp twoArgLoopForString601
twoArgLoopEndForString601:

	jmp endLabel601
badInputForString601:

	mov rax, sobVoid
endLabel601:

	mov rsp, rbp
	pop rbp
	ret
end_make_string_code:
	mov rax, [rax]
	mov qword [makeString], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, multiply_code
	jmp end_multiply_code

multiply_code:
	push rbp
	mov rbp, rsp
	mov rcx, 0 ; rcx is a counter for the number of arguments
.checkIfArgsAreNumbers:

	cmp rcx, qword [rbp + 8*3]
	je .make_mul
	mov rbx, qword [rbp + 8*(4 + rcx)]
	mov rbx, [rbx]
	TYPE rbx
	cmp rbx, T_INTEGER
	je .incCounter
	cmp rbx, T_FRACTION
	je .incCounter
	jmp .badArgs
.incCounter:

	inc rcx
	jmp .checkIfArgsAreNumbers
.make_mul:

	mov rcx, 0 ; rcx is a counter for the number of arguments
	mov rax, 1 ; rax is the accumulator 
.mul_loop:

	cmp rcx, qword [rbp + 8*3]
	je .doneMul

	mov rbx, qword [rbp + 8*(4 + rcx)]
	mov rbx, [rbx]
	DATA rbx
	mul rbx
	inc rcx
	jmp .mul_loop
.doneMul:

	mov r10, rax
	shl r10, 4
	or r10, T_INTEGER
	mov rdi, 8
	call malloc
	mov qword [rax], r10
	jmp .done
.badArgs:

	mov rax, sobVoid
.done:
	mov rsp, rbp
	pop rbp
	ret

end_multiply_code:
	mov rax, [rax]
	mov qword [multiply], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, subtract_code
	jmp end_subtract_code

subtract_code:
	push rbp
	mov rbp, rsp
	mov rcx, 0 ; rcx is a counter for the number of arguments
.checkIfArgsAreNumbers:

	cmp rcx, qword [rbp + 8*3]
	je .make_subtraction
	mov rbx, qword [rbp + 8*(4 + rcx)]
	mov r10, [rbx]
	TYPE r10
	cmp r10, T_INTEGER
	je .incCounter
	cmp r10, T_FRACTION
	je .incCounter
	jmp .badArgs
.incCounter:

	inc rcx
	jmp .checkIfArgsAreNumbers
.make_subtraction:

	mov rcx, 1 ; rcx is a counter for the number of arguments
	mov rdx, qword [rbp + 8*4]
	mov rdx, [rdx]
	DATA rdx
.subtraction_loop:

	cmp rcx, qword [rbp + 8*3]
	je .doneSubtraction

	mov rbx, qword [rbp + 8*(4 + rcx)]
	mov rbx, [rbx]
	DATA rbx
	cmp rbx, 0
	jl .numberIsNeg
	sub rdx, rbx
	inc rcx
	jmp .subtraction_loop
.numberIsNeg:

	NOT rbx
	add rbx, 1
	add rdx, rbx
	inc rcx
	jmp .subtraction_loop
.doneSubtraction:

	mov r10, rdx
	shl r10, 4
	or r10, T_INTEGER
	mov rdi, 8
	call malloc
	mov qword [rax], r10
	jmp .done
.badArgs:

	mov rax, sobVoid
.done:
	mov rsp, rbp
	pop rbp
	ret

end_subtract_code:
	mov rax, [rax]
	mov qword [subtract], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, make_vector_code
	jmp end_make_vector_code

	make_vector_code:

	push rbp
	mov rbp, rsp
	cmp qword [rbp + 3*8], 1
	je oneArgForVector201
	cmp qword [rbp + 3*8], 2
	je twoArgForVector201
	jmp badInputForVector201
oneArgForVector201:
	mov rcx, qword [rbp + 4*8]
	mov r10, [rcx]
	TYPE r10
	cmp r10, T_INTEGER
	jne badInputForVector201
	mov rdi, 8
	call malloc
	mov qword [rax], 0
	shl qword [rax], TYPE_BITS
	or qword [rax], T_INTEGER
	mov r12, rax ; r12 now holds a pointer to a runtime constant zero
	mov r8, qword [rbp + 4*8] ; r8 holds a pointer to the vector length 
	mov r13, [r8]
	DATA r13
	inc r13
	shl r13, 3
	mov rdi, r13
	call malloc
	mov r13, [r8]
	DATA r13 
	mov qword [rax], r13
	shl qword [rax], 30
	lea r13, [rax + 1*8]
	sub r13, start_of_data
	or qword [rax], r13
	shl qword [rax], TYPE_BITS
	or qword [rax], T_VECTOR
	mov r15, [r8]
	DATA r15
	mov r14, 0
oneArgLoopForVector201:

	cmp r14, r15
	je oneArgLoopEndForVector201
	mov qword [rax + 1*8 + r14*8], r12
	inc r14
	jmp oneArgLoopForVector201
oneArgLoopEndForVector201:

	jmp endLabel201
twoArgForVector201:

	mov r8, qword [rbp + 4*8]
	mov r10, [r8]
	TYPE r10
	cmp r10, T_INTEGER
	jne badInputForVector201
	mov r11, [r8]
	DATA r11
	cmp r11, 0
	jl badInputForVector201
	mov r8, qword [rbp + 4*8] ; length of the vector
	mov r12, qword [rbp + 5*8] ; elements of the vector
	mov r11, [r8]
	DATA r11
	inc r11
	shl r11, 3
	mov rdi, r11
	call malloc
	mov r11, [r8]
	DATA r11
	mov qword [rax], r11
	shl qword [rax], 30
	lea r11, [rax + 1*8]
	sub r11, start_of_data
	or qword [rax], r11
	shl qword [rax], TYPE_BITS
	or qword [rax], T_VECTOR
	mov r15, [r8]
	DATA r15
	mov r14, 0
twoArgLoopForVector201:

	cmp r14, r15
	je twoArgLoopEndForVector201
	mov qword [rax + 1*8 + r14*8], r12
	inc r14
	jmp twoArgLoopForVector201
twoArgLoopEndForVector201:

	jmp endLabel201
badInputForVector201:

	mov rax, sobVoid
endLabel201:

	mov rsp, rbp
	pop rbp
	ret
end_make_vector_code:
	mov rax, [rax]
	mov qword [makeVector], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, procedure?_code
	jmp end_procedure?_code


procedure?_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .badArgCount
	mov rax, qword [rbp + 8*4]
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_CLOSURE
	je .trueProcedure?
	mov rax, sobFalse
	jmp .done

.trueProcedure?:
	mov rax, sobTrue

	jmp .done
.badArgCount:

	mov rax, sobVoid
.done:
	mov rsp, rbp
	pop rbp
	ret

end_procedure?_code:
	mov rax, [rax]
	mov qword [procedure?], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, apply_code
	jmp end_apply_code


apply_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*3]
	cmp rax, 2
	jne badInput101
	mov r10, qword [rbp + 8*4]
	mov r10, [r10]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_CLOSURE
	jne badInput101
	mov r10, qword [rbp + 8*5]
	mov r10, [r10]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_PAIR
	jne badInput101
; calculate list length
	mov r10, qword [rbp + 5*8]
	mov r13, 0
countArgs101:

	mov r10, [r10] ; r10 holds the head of the list
	mov r12, r10
	TYPE r12
	cmp r12, T_NIL
	je endCountArgs101
	DATA_LOWER r10
	add r10, start_of_data
	inc r13
	jmp countArgs101
endCountArgs101:

	mov r10, qword [rbp + 8*4] ; holds a pointer to f on the stack
	mov r10, [r10] ; holds the actual f on the stack
	lea r12, [rbp + 8*5] ; holds a pointer to the list of arguments on the stack
; r13 holds the number of arguments. calculated before
	mov r14, r13 ; r14 is the new number of arguments
	lea r15, [rbp + 2*8]
	shl r13, 3
	sub r15, r13 ; r15 holds the address of where to we should copy the old rbp
	mov r11, r15 ; r11 holds a backup of the address of where to we should copy the old rbp
	mov r8, qword [rbp + 1*8] ; r8 hold the return address
	mov rbx, qword [rbp]
	mov qword [r15], rbx ; r15 hold the old rbp
	add r15, 8
	mov qword [r15], r8
	CLOSURE_ENV rcx ; rcx now hold the environment of f
	add r15, 8
	mov qword [r15], rcx
	add r15, 8
	mov qword [r15], r14
	add r15, 8
	mov r12, [r12]
copyArgs101:

	mov rdx, r12
	mov rdx, [rdx]
	TYPE rdx
	cmp rdx, T_NIL
	je doneCopyArgs101
	mov rdx, r12
	mov rdx, [rdx]
	DATA_UPPER rdx
	add rdx, start_of_data
	mov qword [r15], rdx
	add r15, 8
	mov rdx, r12
	mov rdx, [rdx]
	DATA_LOWER rdx
	add rdx, start_of_data
	mov r12, rdx
	jmp copyArgs101
doneCopyArgs101:

	jmp Lend0
badInput101:

	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

Lend0:
	mov rsp, r11
	pop rbp
	mov rdx, r10
	CLOSURE_CODE rdx
	jmp rdx
end_apply_code:
	mov rax, [rax]
	mov qword [apply], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, vector_length_code
	jmp end_vector_length_code


vector_length_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*3]
	cmp rax, 1
	jne .notAVector
	mov rax, qword [rbp + 8*4]
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_VECTOR
	jne .notAVector
	mov rax, qword [rbp + 8*4]
	mov r10, [rax]
	VECTOR_LENGTH r10
	shl r10, 4
	or r10, T_INTEGER
	mov rdi, 8
	call malloc
	mov qword [rax], r10
	jmp .done

.notAVector:
	mov rax, sobVoid
.done:
	mov rsp, rbp
	pop rbp
	ret

end_vector_length_code:
	mov rax, [rax]
	mov qword [vectorLength], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, vector_ref_code
	jmp end_vector_ref_code


vector_ref_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*3]
	cmp rax, 2
	jne .badArgs
	mov rax, qword [rbp + 8*4]
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_VECTOR
	jne .badArgs
	mov rcx, qword [rbp + 8*5]
	mov r10, [rcx]
	TYPE r10
	cmp r10, T_INTEGER
	jne .badArgs
	mov rbx, qword [rbp + 8*4] ; a pointer to the vector
	mov rbx, [rbx]
	mov r9, qword [rbp + 8*5]
	mov r10, [r9] ; the position in the vector
	DATA r10
	VECTOR_ELEMENTS rbx
	mov rax, [rbx + r10*8]
	jmp .done

.badArgs:
	mov rax, sobVoid
.done:
	mov rsp, rbp
	pop rbp
	ret

end_vector_ref_code:
	mov rax, [rax]
	mov qword [vectorRef], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, vector_code
	jmp end_vector_code

	vector_code:

	push rbp
	mov rbp, rsp
	mov r11, qword [rbp + 3*8] ; length of the vector
	inc r11
	shl r11, 3
	mov rdi, r11
	call malloc
	mov r11, qword [rbp + 3*8] ; length of the vector
	mov qword [rax], r11
	shl qword [rax], 30
	lea r11, [rax + 1*8]
	sub r11, start_of_data
	or qword [rax], r11
	shl qword [rax], TYPE_BITS
	or qword [rax], T_VECTOR
	mov r15, qword [rbp + 3*8] ; length of the vector
	mov r14, 0
insertArgSLoopForVector401:

	cmp r14, r15
	je insertArgsLoopEndForVector401
	mov r12, qword [rbp + (4 + r14)*8]
	mov qword [rax + 1*8 + r14*8], r12
	inc r14
	jmp insertArgSLoopForVector401
insertArgsLoopEndForVector401:

	jmp endLabel401
endLabel401:

	mov rsp, rbp
	pop rbp
	ret
end_vector_code:
	mov rax, [rax]
	mov qword [vector], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, vector_set_code
	jmp end_vector_set_code


vector_set_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*3]
	cmp rax, 3
	jl .badArgs
	mov rax, qword [rbp + 8*4]
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_VECTOR
	jne .badArgs
	mov rcx, qword [rbp + 8*5]
	mov r10, [rcx]
	TYPE r10
	cmp r10, T_INTEGER
	jne .badArgs
	mov r13, qword [rbp + 8*4] ; rbx holds a pointer to the vector
	mov r13, [r13] ; hold the actual vector
	mov r9, qword [rbp + 8*5]
	mov r10, [r9] ; r10 hold k - the position in the vector
	DATA r10
	mov r12, qword [rbp + 8*6] ; r12 holds a pointer to the object to replace the k-th element of the vector 
	VECTOR_ELEMENTS r13
	mov qword [r13 + r10*8], r12
	mov rax, sobVoid
	jmp .done

.badArgs:
	mov rax, sobVoid
.done:
	mov rsp, rbp
	pop rbp
	ret

end_vector_set_code:
	mov rax, [rax]
	mov qword [vectorSet], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, set_car_code
	jmp end_set_car_code


set_car_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*3]
	cmp rax, 2
	jne .badArgs
	mov rax, qword [rbp + 8*4]
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_PAIR
	jne .badArgs
	mov r13, qword [rbp + 8*4] ; r13 holds a pointer to the pair
	mov r13, [r13] ; hold the actual pair
	mov r12, qword [rbp + 8*5] ; r12 holds a pointer to the element to replace the car element of the pair 
	sub r12, start_of_data
	shl r13, 30
	shr r13, 30
	shl r12, 34
	or r13,r12
	mov [rax], r13
	mov rax, sobVoid
	jmp .done

.badArgs:
	mov rax, sobVoid
.done:
	mov rsp, rbp
	pop rbp
	ret

end_set_car_code:
	mov rax, [rax]
	mov qword [setCar], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, set_cdr_code
	jmp end_set_cdr_code


set_cdr_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*3]
	cmp rax, 2
	jne .badArgs
	mov rax, qword [rbp + 8*4]
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_PAIR
	jne .badArgs
	mov r13, qword [rbp + 8*4] ; r13 holds a pointer to the pair
	mov r13, [r13] ; hold the actual pair
	mov r12, qword [rbp + 8*5] ; r12 holds a pointer to the element to replace the cdr element of the pair 
	sub r12, start_of_data
	shr r13, 34
	shl r13, 34
	or r13, T_PAIR
	shl r12, 4
	or r13,r12
	mov [rax], r13
	mov rax, sobVoid
	jmp .done

.badArgs:
	mov rax, sobVoid
.done:
	mov rsp, rbp
	pop rbp
	ret

end_set_cdr_code:
	mov rax, [rax]
	mov qword [setCdr], rax

; =============================== PRIMITIVE FUNCTIONS =========================

start_of_instructions:

	push rbp
	mov rbp, rsp

; start
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
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jl bad_arg_count101
	mov r15, 0
	mov r14, qword [rbp + 3*8]
	dec r14
	mov r13, sobNil

opt_args_loop101:

	cmp r14, r15
	je opt_args_loop_end101
	mov rdi, 8
	call malloc
	mov r8, qword [rbp + 4*8 + r14*8]
	mov r12, r8
	sub r12, start_of_data
	shl r12, 30
	mov r9, r13
	sub r9, start_of_data
	or r12, r9
	shl r12, 4
	or r12, T_PAIR
	mov  qword [rax], r12
	mov r13, rax
	dec r14
	jmp opt_args_loop101
opt_args_loop_end101:

	mov qword [rbp + 4*8 + (r15 + 1)*8], r13
; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+1)*8]
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 2
	mov rax, maplist
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure101
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure101
not_a_closure101:

	mov rax, sobVoid
done_closure101:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count101:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel101:
; end of creating a closure of lambda-simple 0

	mov rbx, map
	mov r10, [rax]
	mov qword [rbx], r10
	mov rax, sobVoid

	mov rax, [rax]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure102:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda102
	jmp endLabel102

bodyOfLambda102:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 2
	jne bad_arg_count102
; start of applic of lambda-simple code: 

; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+1)*8]
	push rax

	push 1
	mov rax, car
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure108
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure108
not_a_closure108:

	mov rax, sobVoid
done_closure108:

	add rsp, 8*2

; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, null?
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure107
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure107
not_a_closure107:

	mov rax, sobVoid
done_closure107:

	add rsp, 8*2

; end of applic of lambda-simple code: 

	mov r10, [rax]
	cmp r10, SOB_FALSE
	je L0
	; codegen for const start
	mov rax, sobNil
	;code gen for constant end

	jmp Lend1
L0:
	; start of applic of lambda-simple code: 

; start of applic of lambda-simple code: 

; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+1)*8]
	push rax
	mov rax, cdr
	push rax

	push 2
	mov rax, map1
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure106
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure106
not_a_closure106:

	mov rax, sobVoid
done_closure106:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 2
	mov rax, maplist
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure105
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure105
not_a_closure105:

	mov rax, sobVoid
done_closure105:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
; start of applic of lambda-simple code: 

; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+1)*8]
	push rax
	mov rax, car
	push rax

	push 2
	mov rax, map1
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure104
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure104
not_a_closure104:

	mov rax, sobVoid
done_closure104:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 2
	mov rax, apply
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure103
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure103
not_a_closure103:

	mov rax, sobVoid
done_closure103:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax

	push 2
	mov rax, cons
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure102
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure102
not_a_closure102:

	mov rax, sobVoid
done_closure102:

	add rsp, 8*3

; end of applic of lambda-simple code: 


Lend1:

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count102:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel102:
; end of creating a closure of lambda-simple 0

	mov rbx, maplist
	mov r10, [rax]
	mov qword [rbx], r10
	mov rax, sobVoid

	mov rax, [rax]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure103:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda103
	jmp endLabel103

bodyOfLambda103:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 2
	jne bad_arg_count103
; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+1)*8]
	push rax

	push 1
	mov rax, null?
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure114
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure114
not_a_closure114:

	mov rax, sobVoid
done_closure114:

	add rsp, 8*2

; end of applic of lambda-simple code: 

	mov r10, [rax]
	cmp r10, SOB_FALSE
	je L1
	; codegen for const start
	mov rax, sobNil
	;code gen for constant end

	jmp Lend2
L1:
	; start of applic of lambda-simple code: 

; start of applic of lambda-simple code: 

; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+1)*8]
	push rax

	push 1
	mov rax, cdr
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure113
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure113
not_a_closure113:

	mov rax, sobVoid
done_closure113:

	add rsp, 8*2

; end of applic of lambda-simple code: 

	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 2
	mov rax, map1
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure112
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure112
not_a_closure112:

	mov rax, sobVoid
done_closure112:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
; start of applic of lambda-simple code: 

; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+1)*8]
	push rax

	push 1
	mov rax, car
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure111
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure111
not_a_closure111:

	mov rax, sobVoid
done_closure111:

	add rsp, 8*2

; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure110
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure110
not_a_closure110:

	mov rax, sobVoid
done_closure110:

	add rsp, 8*2

; end of applic of lambda-simple code: 

	push rax

	push 2
	mov rax, cons
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure109
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure109
not_a_closure109:

	mov rax, sobVoid
done_closure109:

	add rsp, 8*3

; end of applic of lambda-simple code: 


Lend2:

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count103:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel103:
; end of creating a closure of lambda-simple 0

	mov rbx, map1
	mov r10, [rax]
	mov qword [rbx], r10
	mov rax, sobVoid

	mov rax, [rax]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure104:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda104
	jmp endLabel104

bodyOfLambda104:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 0
	jl bad_arg_count104
	mov r15, -1
	mov r14, qword [rbp + 3*8]
	dec r14
	mov r13, sobNil

opt_args_loop102:

	cmp r14, r15
	je opt_args_loop_end102
	mov rdi, 8
	call malloc
	mov r8, qword [rbp + 4*8 + r14*8]
	mov r12, r8
	sub r12, start_of_data
	shl r12, 30
	mov r9, r13
	sub r9, start_of_data
	or r12, r9
	shl r12, 4
	or r12, T_PAIR
	mov  qword [rax], r12
	mov r13, rax
	dec r14
	jmp opt_args_loop102
opt_args_loop_end102:

	mov qword [rbp + 4*8 + (r15 + 1)*8], r13
; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+0)*8]
	push rax
	; codegen for const start
	mov rax, sobNil
	;code gen for constant end
	push rax

	push 2
	mov rax, append_base_case
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure115
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure115
not_a_closure115:

	mov rax, sobVoid
done_closure115:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count104:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel104:
; end of creating a closure of lambda-simple 0

	mov rbx, append
	mov r10, [rax]
	mov qword [rbx], r10
	mov rax, sobVoid

	mov rax, [rax]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure105:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda105
	jmp endLabel105

bodyOfLambda105:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 2
	jne bad_arg_count105
; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+1)*8]
	push rax

	push 1
	mov rax, null?
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure117
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure117
not_a_closure117:

	mov rax, sobVoid
done_closure117:

	add rsp, 8*2

; end of applic of lambda-simple code: 

	mov r10, [rax]
	cmp r10, SOB_FALSE
	je L2
	mov rax, qword [rbp + (4+0)*8]

	jmp Lend3
L2:
	; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+1)*8]
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 2
	mov rax, append_helper
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure116
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure116
not_a_closure116:

	mov rax, sobVoid
done_closure116:

	add rsp, 8*3

; end of applic of lambda-simple code: 


Lend3:

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count105:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel105:
; end of creating a closure of lambda-simple 0

	mov rbx, append_base_case
	mov r10, [rax]
	mov qword [rbx], r10
	mov rax, sobVoid

	mov rax, [rax]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure106:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda106
	jmp endLabel106

bodyOfLambda106:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 2
	jne bad_arg_count106
; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
	mov rax, null?
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure125
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure125
not_a_closure125:

	mov rax, sobVoid
done_closure125:

	add rsp, 8*2

; end of applic of lambda-simple code: 

	mov r10, [rax]
	cmp r10, SOB_FALSE
	je L3
; start of applic of lambda-simple code: 

; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+1)*8]
	push rax

	push 1
	mov rax, cdr
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure124
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure124
not_a_closure124:

	mov rax, sobVoid
done_closure124:

	add rsp, 8*2

; end of applic of lambda-simple code: 

	push rax
; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+1)*8]
	push rax

	push 1
	mov rax, car
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure123
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure123
not_a_closure123:

	mov rax, sobVoid
done_closure123:

	add rsp, 8*2

; end of applic of lambda-simple code: 

	push rax

	push 2
	mov rax, append_base_case
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure122
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure122
not_a_closure122:

	mov rax, sobVoid
done_closure122:

	add rsp, 8*3

; end of applic of lambda-simple code: 


	jmp Lend4
L3:
	; start of applic of lambda-simple code: 

; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+1)*8]
	push rax
; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
	mov rax, cdr
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure121
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure121
not_a_closure121:

	mov rax, sobVoid
done_closure121:

	add rsp, 8*2

; end of applic of lambda-simple code: 

	push rax

	push 2
	mov rax, append_helper
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure120
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure120
not_a_closure120:

	mov rax, sobVoid
done_closure120:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
; start of applic of lambda-simple code: 

	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
	mov rax, car
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure119
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure119
not_a_closure119:

	mov rax, sobVoid
done_closure119:

	add rsp, 8*2

; end of applic of lambda-simple code: 

	push rax

	push 2
	mov rax, cons
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure118
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure118
not_a_closure118:

	mov rax, sobVoid
done_closure118:

	add rsp, 8*3

; end of applic of lambda-simple code: 


Lend4:

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count106:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel106:
; end of creating a closure of lambda-simple 0

	mov rbx, append_helper
	mov r10, [rax]
	mov qword [rbx], r10
	mov rax, sobVoid

	mov rax, [rax]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure107:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda107
	jmp endLabel107

bodyOfLambda107:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 0
	jl bad_arg_count107
	mov r15, -1
	mov r14, qword [rbp + 3*8]
	dec r14
	mov r13, sobNil

opt_args_loop103:

	cmp r14, r15
	je opt_args_loop_end103
	mov rdi, 8
	call malloc
	mov r8, qword [rbp + 4*8 + r14*8]
	mov r12, r8
	sub r12, start_of_data
	shl r12, 30
	mov r9, r13
	sub r9, start_of_data
	or r12, r9
	shl r12, 4
	or r12, T_PAIR
	mov  qword [rax], r12
	mov r13, rax
	dec r14
	jmp opt_args_loop103
opt_args_loop_end103:

	mov qword [rbp + 4*8 + (r15 + 1)*8], r13
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count107:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel107:
; end of creating a closure of lambda-simple 0

	mov rbx, list
	mov r10, [rax]
	mov qword [rbx], r10
	mov rax, sobVoid

	mov rax, [rax]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, sobPair4
	;code gen for constant end
	push rax
	mov rax, plus
	push rax

	push 2
	mov rax, apply
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure126
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure126
not_a_closure126:

	mov rax, sobVoid
done_closure126:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	mov rax, [rax]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, sobPair4
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, sobPair4
	;code gen for constant end
	push rax
	mov rax, cons
	push rax

	push 3
	mov rax, map
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure127
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure127
not_a_closure127:

	mov rax, sobVoid
done_closure127:

	add rsp, 8*4

; end of applic of lambda-simple code: 

	mov rax, [rax]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, sobPair0
	;code gen for constant end
	push rax
	mov rax, not
	push rax

	push 2
	mov rax, map
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure128
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure128
not_a_closure128:

	mov rax, sobVoid
done_closure128:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	mov rax, [rax]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end
	mov rsp, rbp
	pop rbp
	ret

