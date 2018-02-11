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

a:
	dq SOB_UNDEFINED

sobString10:
	MAKE_LITERAL_STRING "-6"

sobInt10:
	dq MAKE_LITERAL (T_INTEGER, 10)

sobFrac1_2:
	dq MAKE_LITERAL_FRACTION (sobInt1, sobInt2)

sobChar9:
	dq MAKE_LITERAL(T_CHAR, 97)

sobPair6:
	dq MAKE_LITERAL_PAIR (sobInt2, sobNil)

sobNil:
	dq SOB_NIL

sobPair4:
	dq MAKE_LITERAL_PAIR (sobInt1, sobPair6)

sobInt1:
	dq MAKE_LITERAL (T_INTEGER, 1)

sobInt2:
	dq MAKE_LITERAL (T_INTEGER, 2)

sobInt3:
	dq MAKE_LITERAL (T_INTEGER, 3)

sobInt4:
	dq MAKE_LITERAL (T_INTEGER, 4)

sobInt5:
	dq MAKE_LITERAL (T_INTEGER, 5)



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

; =============================== PRIMITIVE FUNCTIONS =========================

start_of_instructions:

	push rbp
	mov rbp, rsp

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt1]
	;code gen for constant end
	push rax
	mov rax, [a]
	push rax

	push 2
	mov rax, [number?]
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

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt1]
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, qword [sobPair4]
	;code gen for constant end
	push rax

	push 2
	mov rax, [pair?]
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

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt1]
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, qword [sobString10]
	;code gen for constant end
	push rax

	push 2
	mov rax, [integer?]
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

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
	; codegen for const start
	mov rax, qword [sobInt10]
	;code gen for constant end
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt1]
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, qword [sobInt3]
	;code gen for constant end
	push rax

	push 2
	mov rax, [boolean?]
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

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt1]
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, qword [sobInt3]
	;code gen for constant end
	push rax

	push 2
	mov rax, [number?]
	mov rcx, rax
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure105
	mov rbx, rax
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE rax
	call rax
	add rsp, 8*1
	jmp done_closure105
not_a_closure105:

	mov rax, SOB_VOID
done_closure105:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt1]
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, qword [sobNil]
	;code gen for constant end
	push rax

	push 2
	mov rax, [number?]
	mov rcx, rax
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure106
	mov rbx, rax
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE rax
	call rax
	add rsp, 8*1
	jmp done_closure106
not_a_closure106:

	mov rax, SOB_VOID
done_closure106:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt1]
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, qword [sobFrac1_2]
	;code gen for constant end
	push rax

	push 2
	mov rax, [number?]
	mov rcx, rax
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure107
	mov rbx, rax
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE rax
	call rax
	add rsp, 8*1
	jmp done_closure107
not_a_closure107:

	mov rax, SOB_VOID
done_closure107:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt1]
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, qword [sobChar9]
	;code gen for constant end
	push rax

	push 2
	mov rax, [char?]
	mov rcx, rax
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure108
	mov rbx, rax
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE rax
	call rax
	add rsp, 8*1
	jmp done_closure108
not_a_closure108:

	mov rax, SOB_VOID
done_closure108:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt1]
	;code gen for constant end
	push rax
	mov rax, [a]
	push rax

	push 2
	mov rax, [char?]
	mov rcx, rax
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure109
	mov rbx, rax
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE rax
	call rax
	add rsp, 8*1
	jmp done_closure109
not_a_closure109:

	mov rax, SOB_VOID
done_closure109:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt1]
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, qword [sobPair4]
	;code gen for constant end
	push rax

	push 2
	mov rax, [cdr]
	mov rcx, rax
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure110
	mov rbx, rax
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE rax
	call rax
	add rsp, 8*1
	jmp done_closure110
not_a_closure110:

	mov rax, SOB_VOID
done_closure110:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt1]
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, qword [sobInt2]
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, qword [sobInt1]
	;code gen for constant end
	push rax

	push 3
	mov rax, [cons]
	mov rcx, rax
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure111
	mov rbx, rax
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE rax
	call rax
	add rsp, 8*1
	jmp done_closure111
not_a_closure111:

	mov rax, SOB_VOID
done_closure111:

	add rsp, 8*4

; end of applic of lambda-simple code: 

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt5]
	;code gen for constant end
	push rax
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt4]
	;code gen for constant end
	push rax
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt3]
	;code gen for constant end
	push rax
; start of applic of lambda-simple code: 

	; codegen for const start
	mov rax, qword [sobInt2]
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, qword [sobInt1]
	;code gen for constant end
	push rax

	push 2
	mov rax, [cons]
	mov rcx, rax
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure115
	mov rbx, rax
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE rax
	call rax
	add rsp, 8*1
	jmp done_closure115
not_a_closure115:

	mov rax, SOB_VOID
done_closure115:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax

	push 2
	mov rax, [cons]
	mov rcx, rax
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure114
	mov rbx, rax
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE rax
	call rax
	add rsp, 8*1
	jmp done_closure114
not_a_closure114:

	mov rax, SOB_VOID
done_closure114:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax

	push 2
	mov rax, [cons]
	mov rcx, rax
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure113
	mov rbx, rax
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE rax
	call rax
	add rsp, 8*1
	jmp done_closure113
not_a_closure113:

	mov rax, SOB_VOID
done_closure113:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax

	push 2
	mov rax, [cons]
	mov rcx, rax
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure112
	mov rbx, rax
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE rax
	call rax
	add rsp, 8*1
	jmp done_closure112
not_a_closure112:

	mov rax, SOB_VOID
done_closure112:

	add rsp, 8*3

; end of applic of lambda-simple code: 

	push rax
	call write_sob_if_not_void
	add rsp, 8

; end
	mov rsp, rbp
	pop rbp
	ret

