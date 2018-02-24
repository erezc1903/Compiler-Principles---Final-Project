%include "scheme.s"

%include "gcd.s"

section .bss

	SymbolTable: resq  1

section .data

size_i:  ; Used to determine the size of the structure
	struc node
		symbol: resq  1
		next: resq  1
	endstruc

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

lessSthan:
	dq SOB_UNDEFINED

sobVoid:
	dq SOB_VOID

sobFalse:
	dq SOB_FALSE
sobTrue:
	dq SOB_TRUE

sobNil:
	dq SOB_NIL

sobInt5:
	dq MAKE_LITERAL (T_INTEGER, 5)

sobInt4:
	dq MAKE_LITERAL (T_INTEGER, 4)

sobInt8:
	dq MAKE_LITERAL (T_INTEGER, 8)

sobInt9:
	dq MAKE_LITERAL (T_INTEGER, 9)

sobInt6:
	dq MAKE_LITERAL (T_INTEGER, 6)

sobInt2:
	dq MAKE_LITERAL (T_INTEGER, 2)

sobInt44:
	dq MAKE_LITERAL (T_INTEGER, 44)

sobUndef:
	dq SOB_UNDEFINED



section .text

	extern exit, printf, scanf, malloc

	global main

addSymbol:
		push rbp            ; Save the stack
		mov rbp, rsp
		mov rdi, 16            ; 8 bytes for the symbol and 8 bytes for the next link
		call malloc         ; Call the malloc function - now eax has the address of the allocated memory
		mov rbx, [rbp + 3*8]
		mov [rax + symbol], rbx    ; Add the element to the node data field
		mov qword [rax + next], sobNil   ; Address of the next element is NULL, because it is the last element in the list
		mov rbx, [rbp + 2*8]  ; Retrieve the address to the Symbol Table
		cmp qword [rbx], 0
		je firstElement
		mov rbx, [rbx]      ; This parameter was the address of the address
		                     ; Now it is the address of the first element, in this case, not null
		; If it is not NULL, find the address of the last element
next_element:

		cmp qword [rbx + next], sobNil
		je found_last
		mov rbx, [rbx + next]
		jmp next_element
found_last:

		mov [rbx + next], rax   ; Last element is this one from the newly allocated memory block
		jmp doneAdding
firstElement:

		mov [rbx], rax ; Point the address of the first element to the allocated memory
doneAdding:

		mov rsp, rbp
		pop rbp
		ret               ; Return to the caller function and cleaning the stack

findSymbol:

		push rbp
		mov rbp, rsp
		mov r11, [rbp + 2*8]       ;a pointer to the head of the symbol table
		mov r12, [rbp + 3*8]          ; Address of the address of the first element
		mov r11, [r11]
	search_loop:

		mov r13, r11
		mov r11, [r11 + symbol]
		mov r15, [r11]
		DATA r15
		cmp r15, r12
		je done                  ; Don't do anything if the list is empty
		mov r11, r13
		mov r11, [r11 + next]       ; The next element in the list
		jmp search_loop
	done:

		mov rax, r11
		mov rsp, rbp
		pop rbp
		ret
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
	MAKE_LITERAL_CLOSURE rax, rbx, symbol?_code
	jmp end_symbol?_code

symbol?_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
	mov r10, [rax]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_SYMBOL
	je trueSymbol?
	mov rax, sobFalse
	jmp doneSymbol?

trueSymbol?:
	mov rax, sobTrue

doneSymbol?:
	mov rsp, rbp
	pop rbp
	ret

end_symbol?_code:
	mov rax, [rax]
	mov qword [symbol?], rax

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
	mov rbx, rax
	cmp rbx, sobFalse
	jne .retFalse
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

	mov r12, 0 ; represent the numerator -a- of the sum
	mov r13, 1 ; represent the denominator -b- of the sum
	mov r8, 0
.addition_loop:

	cmp r8, qword [rbp + 8*3]
	je .doneAddition

	mov r9, qword [rbp + 8*(4 + r8)]
	mov r10, [r9]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_FRACTION
	jne .makeFraction
	mov r11, r10
	NUMERATOR r10 ; holds the numerator -c- of the number to be added
	DATA r10
	DENOMINATOR r11 ; holds the denominator -d- of the number to be added
	DATA r11
.continueSumming:
	mov rax, r10
	mul r13
	mov rcx, rax ; rcx temporeraly hold a*d
	mov rax, r11
	mul r12
	add rcx, rax ; rcx temporeraly hold a*d + b*c
	mov r12, rcx ; r12 gets the new numerator
	mov rax, r11
	mul r13; rax temporeraly hold a*d
	mov r13, rax ; r12 gets the new denominator
	inc r8
	jmp .addition_loop
.makeFraction:

	mov r9, qword [rbp + 8*(4 + r8)]
	mov r10, [r9]
	DATA r10
	mov r11, 1
	jmp .continueSumming
.doneAddition:

	push r12
	push r13
	mov rax, 0
	call gcd
	mov r10, rax
	mov rax, r12
	cqo
	idiv r10
	mov r12, rax
	mov rax, r13
	cqo
	idiv r10
	mov r13, rax
	cmp r13, 1
	je .retInt
	mov rdi, 8
	call malloc
	mov r14, rax
	shl r12, TYPE_BITS
	or r12, T_INTEGER
	mov qword [r14], r12 ; r14 hold the numerator of the result
	mov rdi, 8
	call malloc
	mov r15, rax
	shl r13, TYPE_BITS
	or r13, T_INTEGER
	mov qword [r15], r13 ; r15 hold the denominator of the result
	mov r8, r14
	sub r8, start_of_data
	shl r8, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)
	mov r9, r15
	sub r9, start_of_data
	shl r9, TYPE_BITS
	or r8, r9
	or r8, T_FRACTION
	mov rdi, 8
	call malloc
	mov qword [rax], r8
	jmp .done
.retInt:

	shl r12, TYPE_BITS
	or r12, T_INTEGER
	mov rdi, 8
	call malloc
	mov qword [rax], r12
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
	je .retTrue
.check_greater_than:

	mov r8, 0 ; r8 is a counter for the number of arguments
	mov r9, qword [rbp + 8*3]
	dec r9
.check_greater_than_loop:

	cmp r8,r9
	je .retTrue

	mov r10, qword [rbp + 8*(4 + r8)]
	mov r10, [r10]
	mov r11, r10
	TYPE r10
	cmp r10, T_FRACTION
	jne .makeFirstFraction
	mov r10, r11
	NUMERATOR r10 ; holds the numerator -a- of the first number
	DATA r10
	DENOMINATOR r11 ; holds the denominator -b- of the second number
	DATA r11
.continueComparingAfterFirst:

	mov r12, qword [rbp + 8*(5 + r8)]
	mov r12, [r12]
	mov r13, r12
	TYPE r12
	cmp r12, T_FRACTION
	jne .makeSecondFraction
	mov r12, r13
	NUMERATOR r12 ; holds the numerator -c- of the number to be added
	DATA r12
	DENOMINATOR r13 ; holds the denominator -d- of the number to be added
	DATA r13
.continueComparingAfterSecond:

	mov rax, r10
	mul r13
	mov r10, rax
	mov rax, r11
	mul r12
	mov r12, rax
	cmp r10, r12
	jle .retFalse
	inc r8
	jmp .check_greater_than_loop
.makeFirstFraction:

	mov r10, qword [rbp + 8*(4 + r8)]
	mov r10, [r10]
	DATA r10
	mov r11, 1
	jmp .continueComparingAfterFirst
.makeSecondFraction:

	mov r12, qword [rbp + 8*(5 + r8)]
	mov r12, [r12]
	DATA r12
	mov r13, 1
	jmp .continueComparingAfterSecond
.retFalse:

	mov rax, sobFalse
	jmp .done
.retTrue:

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
	je .retTrue
.check_less_than:

	mov r8, 0 ; r8 is a counter for the number of arguments
	mov r9, qword [rbp + 8*3]
	dec r9
.check_less_than_loop:

	cmp r8,r9
	je .retTrue

	mov r10, qword [rbp + 8*(4 + r8)]
	mov r10, [r10]
	mov r11, r10
	TYPE r10
	cmp r10, T_FRACTION
	jne .makeFirstFraction
	mov r10, r11
	NUMERATOR r10 ; holds the numerator -a- of the first number
	DATA r10
	DENOMINATOR r11 ; holds the denominator -b- of the second number
	DATA r11
.continueComparingAfterFirst:

	mov r12, qword [rbp + 8*(5 + r8)]
	mov r12, [r12]
	mov r13, r12
	TYPE r12
	cmp r12, T_FRACTION
	jne .makeSecondFraction
	mov r12, r13
	NUMERATOR r12 ; holds the numerator -c- of the number to be added
	DATA r12
	DENOMINATOR r13 ; holds the denominator -d- of the number to be added
	DATA r13
.continueComparingAfterSecond:

	mov rax, r10
	mul r13
	mov r10, rax
	mov rax, r11
	mul r12
	mov r12, rax
	cmp r12, r10
	jle .retFalse
	inc r8
	jmp .check_less_than_loop
.makeFirstFraction:

	mov r10, qword [rbp + 8*(4 + r8)]
	mov r10, [r10]
	DATA r10
	mov r11, 1
	jmp .continueComparingAfterFirst
.makeSecondFraction:

	mov r12, qword [rbp + 8*(5 + r8)]
	mov r12, [r12]
	DATA r12
	mov r13, 1
	jmp .continueComparingAfterSecond
.retFalse:

	mov rax, sobFalse
	jmp .done
.retTrue:

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
	je .retTrue
.check_equal:

	mov r8, 0 ; r8 is a counter for the number of arguments
	mov r9, qword [rbp + 8*3]
	dec r9
.check_equal_loop:

	cmp r8,r9
	je .retTrue

	mov r10, qword [rbp + 8*(4 + r8)]
	mov r10, [r10]
	mov r11, r10
	TYPE r10
	cmp r10, T_FRACTION
	jne .makeFirstFraction
	mov r10, r11
	NUMERATOR r10 ; holds the numerator -a- of the first number
	DATA r10
	DENOMINATOR r11 ; holds the denominator -b- of the second number
	DATA r11
.continueComparingAfterFirst:

	mov r12, qword [rbp + 8*(5 + r8)]
	mov r12, [r12]
	mov r13, r12
	TYPE r12
	cmp r12, T_FRACTION
	jne .makeSecondFraction
	mov r12, r13
	NUMERATOR r12 ; holds the numerator -c- of the number to be added
	DATA r12
	DENOMINATOR r13 ; holds the denominator -d- of the number to be added
	DATA r13
.continueComparingAfterSecond:

	mov rax, r10
	mul r13
	mov r10, rax
	mov rax, r11
	mul r12
	mov r12, rax
	cmp r10, r12
	jne .retFalse
	inc r8
	jmp .check_equal_loop
.makeFirstFraction:

	mov r10, qword [rbp + 8*(4 + r8)]
	mov r10, [r10]
	DATA r10
	mov r11, 1
	jmp .continueComparingAfterFirst
.makeSecondFraction:

	mov r12, qword [rbp + 8*(5 + r8)]
	mov r12, [r12]
	DATA r12
	mov r13, 1
	jmp .continueComparingAfterSecond
.retFalse:

	mov rax, sobFalse
	jmp .done
.retTrue:

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
	je .make_multiplication
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
.make_multiplication:

	mov r12, 1 ; represent the numerator -a- of the sum
	mov r13, 1 ; represent the denominator -b- of the sum
	mov r8, 0
.multiplication_loop:

	cmp r8, qword [rbp + 8*3]
	je .doneMultiplication

	mov r9, qword [rbp + 8*(4 + r8)]
	mov r10, [r9]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_FRACTION
	jne .makeFraction
	mov r11, r10
	NUMERATOR r10 ; holds the numerator -c- of the number to be added
	DATA r10
	DENOMINATOR r11 ; holds the denominator -d- of the number to be added
	DATA r11
.continueMultiplying:
	mov rax, r10
	mul r12
	mov r12, rax ; r12 gets the new numerator
	mov rax, r11
	mul r13
	mov r13, rax ; r13 gets the new denominator
	inc r8
	jmp .multiplication_loop
.makeFraction:

	mov r9, qword [rbp + 8*(4 + r8)]
	mov r10, [r9]
	DATA r10
	mov r11, 1
	jmp .continueMultiplying
.doneMultiplication:

	push r12
	push r13
	mov rax, 0
	call gcd
	mov r10, rax
	mov rax, r12
	cqo
	idiv r10
	mov r12, rax
	mov rax, r13
	cqo
	idiv r10
	mov r13, rax
	cmp r13, 1
	je .retInt
	mov rdi, 8
	call malloc
	mov r14, rax
	shl r12, TYPE_BITS
	or r12, T_INTEGER
	mov qword [r14], r12 ; r14 hold the numerator of the result
	mov rdi, 8
	call malloc
	mov r15, rax
	shl r13, TYPE_BITS
	or r13, T_INTEGER
	mov qword [r15], r13 ; r15 hold the denominator of the result
	mov r8, r14
	sub r8, start_of_data
	shl r8, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)
	mov r9, r15
	sub r9, start_of_data
	shl r9, TYPE_BITS
	or r8, r9
	or r8, T_FRACTION
	mov rdi, 8
	call malloc
	mov qword [rax], r8
	jmp .done
.retInt:

	shl r12, TYPE_BITS
	or r12, T_INTEGER
	mov rdi, 8
	call malloc
	mov qword [rax], r12
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
.make_subtraction:

	mov r12, qword [rbp + 4*8]
	mov r12, [r12]
	TYPE r12
	cmp r12, T_FRACTION
	je .startWithFraction
	mov r12, qword [rbp + 4*8]
	mov r12, [r12]
	DATA r12 ; represent the numerator -a- of the sum
	mov r13, 1 ; represent the denominator -b- of the sum
.startSubstraction:

	mov r8, 1
.subtraction_loop:

	cmp r8, qword [rbp + 8*3]
	je .doneSubtraction

	mov r9, qword [rbp + 8*(4 + r8)]
	mov r10, [r9]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_FRACTION
	jne .makeFraction
	mov r11, r10
	NUMERATOR r10 ; holds the numerator -c- of the number to be added
	DATA r10
	DENOMINATOR r11 ; holds the denominator -d- of the number to be added
	DATA r11
.continueSubtracting:
	mov rax, r10
	mul r13
	mov rcx, rax ; rcx temporeraly hold a*d
	mov rax, r11
	mul r12
	sub rcx, rax ; rcx temporeraly hold a*d - b*c
	mov r12, rcx ; r12 gets the new numerator
	neg r12
	mov rax, r11
	mul r13; rax temporeraly hold a*d
	mov r13, rax ; r12 gets the new denominator
	inc r8
	jmp .subtraction_loop
.makeFraction:

	mov r9, qword [rbp + 8*(4 + r8)]
	mov r10, [r9]
	DATA r10
	mov r11, 1
	jmp .continueSubtracting
.startWithFraction:

	mov r12, qword [rbp + 4*8]
	mov r12, [r12]
	mov r13, r12
	NUMERATOR r12 ; holds the numerator -c- of the number to be added
	DATA r12
	DENOMINATOR r13 ; holds the denominator -d- of the number to be added
	DATA r13
	jmp .startSubstraction
.doneSubtraction:

	push r12
	push r13
	mov rax, 0
	call gcd
	mov r10, rax
	mov rax, r12
	cqo
	idiv r10
	mov r12, rax
	mov rax, r13
	cqo
	idiv r10
	mov r13, rax
	cmp r13, 1
	je .retInt
	mov rdi, 8
	call malloc
	mov r14, rax
	shl r12, TYPE_BITS
	or r12, T_INTEGER
	mov qword [r14], r12 ; r14 hold the numerator of the result
	mov rdi, 8
	call malloc
	mov r15, rax
	shl r13, TYPE_BITS
	or r13, T_INTEGER
	mov qword [r15], r13 ; r15 hold the denominator of the result
	mov r8, r14
	sub r8, start_of_data
	shl r8, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)
	mov r9, r15
	sub r9, start_of_data
	shl r9, TYPE_BITS
	or r8, r9
	or r8, T_FRACTION
	mov rdi, 8
	call malloc
	mov qword [rax], r8
	jmp .done
.retInt:

	shl r12, TYPE_BITS
	or r12, T_INTEGER
	mov rdi, 8
	call malloc
	mov qword [rax], r12
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

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, divide_code
	jmp end_divide_code

divide_code:
	push rbp
	mov rbp, rsp
	mov rcx, 0 ; rcx is a counter for the number of arguments
.checkIfArgsAreNumbers:

	cmp rcx, qword [rbp + 8*3]
	je .make_division
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
.make_division:

	mov r12, qword [rbp + 4*8]
	mov r12, [r12]
	TYPE r12
	cmp r12, T_FRACTION
	je .startWithFraction
	mov r12, qword [rbp + 4*8]
	mov r12, [r12]
	DATA r12 ; represent the numerator -a- of the sum
	mov r13, 1 ; represent the denominator -b- of the sum
.startDivision:

	mov r8, 1
.division_loop:

	cmp r8, qword [rbp + 8*3]
	je .doneDivision

	mov r9, qword [rbp + 8*(4 + r8)]
	mov r10, [r9]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_FRACTION
	jne .makeFraction
	mov r11, r10
	NUMERATOR r10 ; holds the numerator -c- of the number to be added
	DATA r10
	DENOMINATOR r11 ; holds the denominator -d- of the number to be added
	DATA r11
	xchg r10, r11
.continueDividing:
	mov rax, r10
	mul r12
	mov r12, rax ; r12 gets the new numerator
	mov rax, r11
	mul r13
	mov r13, rax ; r13 gets the new denominator
	inc r8
	jmp .division_loop
.startWithFraction:

	mov r12, qword [rbp + 4*8]
	mov r12, [r12]
	mov r13, r12
	NUMERATOR r12 ; holds the numerator -c- of the number to be added
	DATA r12
	DENOMINATOR r13 ; holds the denominator -d- of the number to be added
	DATA r13
	jmp .startDivision
.makeFraction:

	mov r9, qword [rbp + 8*(4 + r8)]
	mov r11, [r9]
	DATA r11
	mov r10, 1
	jmp .continueDividing
.doneDivision:

	cmp r13, 0
	jl .negFrac
.createFrac:
	push r12
	push r13
	mov rax, 0
	call gcd
	mov r10, rax
	mov rax, r12
	cqo
	idiv r10
	mov r12, rax
	mov rax, r13
	cqo
	idiv r10
	mov r13, rax
	cmp r13, 1
	je .retInt
	mov rdi, 8
	call malloc
	mov r14, rax
	shl r12, TYPE_BITS
	or r12, T_INTEGER
	mov qword [r14], r12 ; r14 hold the numerator of the result
	mov rdi, 8
	call malloc
	mov r15, rax
	shl r13, TYPE_BITS
	or r13, T_INTEGER
	mov qword [r15], r13 ; r15 hold the denominator of the result
	mov r8, r14
	sub r8, start_of_data
	shl r8, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)
	mov r9, r15
	sub r9, start_of_data
	shl r9, TYPE_BITS
	or r8, r9
	or r8, T_FRACTION
	mov rdi, 8
	call malloc
	mov qword [rax], r8
	jmp .done
.retInt:

	shl r12, TYPE_BITS
	or r12, T_INTEGER
	mov rdi, 8
	call malloc
	mov qword [rax], r12
	jmp .done
.negFrac:

	neg r12
	neg r13
	jmp .createFrac
.badArgs:

	mov rax, sobVoid
.done:
	mov rsp, rbp
	pop rbp
	ret

end_divide_code:
	mov rax, [rax]
	mov qword [divide], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, symbolToString_code
	jmp end_symbolToString_code

symbolToString_code:
	push rbp
	mov rbp, rsp
	mov rax, qword [rbp + 8*4]
	mov r10, [rax]
	DATA r10
	push r10
	push SymbolTable
	call findSymbol
	add rsp, 2*8
	mov rax, [rax]
	DATA rax
.doneSymbolToString:
	mov rsp, rbp
	pop rbp
	ret

end_symbolToString_code:
	mov rax, [rax]
	mov qword [symbolToString], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, stringToSymbol_code
	jmp end_stringToSymbol_code

stringToSymbol_code:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp + 8*4]
	mov rdi, 8
	call malloc
	mov r9, rax
	mov r8, r10
	shl r8, 4
	or r8, T_SYMBOL
	mov qword [rax], r8
	push rax
	push SymbolTable
	call addSymbol
	add rsp, 2*8
	mov rax, r9
.doneStringToSymbol:
	mov rsp, rbp
	pop rbp
	ret

end_stringToSymbol_code:
	mov rax, [rax]
	mov qword [stringToSymbol], rax

	mov rbp, rsp
	mov rdi, 16
	call malloc
	mov rbx, 1
	MAKE_LITERAL_CLOSURE rax, rbx, eq?_code
	jmp end_eq?_code

eq?_code:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp + 8*4]
	mov r9, qword [rbp + 8*5]
	mov rbx, [r10]
	mov rcx, [r9]
	TYPE rbx
	TYPE rcx
	cmp rbx, rcx
	jne .retFalse
	mov rbx, [r10]
	mov rcx, [r9]
	TYPE rbx
	TYPE rcx
	cmp rbx, T_SYMBOL
	je .checkSymbols
	mov rbx, [r10]
	mov rcx, [r9]
	cmp rbx, rcx
	je .retTrue
	jmp .retFalse
.checkSymbols:

	mov r10, [r10]
	DATA r10
	push r10
	push SymbolTable
	call findSymbol
	add rsp, 2*8
	mov r14, rax
	mov r9, [r9]
	DATA r9
	push r9
	push SymbolTable
	call findSymbol
	add rsp, 2*8
	cmp rax, r14
	je .retTrue
	mov rax, sobFalse
	jmp .doneEq?
.retTrue:
	mov rax, sobTrue
	jmp .doneEq?
.retFalse:

	mov rax, sobFalse
.doneEq?:
	mov rsp, rbp
	pop rbp
	ret

end_eq?_code:
	mov rax, [rax]
	mov qword [eq?], rax

; =============================== PRIMITIVE FUNCTIONS =========================
start_of_creation_of_symbol_table:


start_of_instructions:

	push rbp
	mov rbp, rsp

; start
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure5001:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda5001
	jmp endLabel5001

bodyOfLambda5001:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jl bad_arg_count5001
	mov r15, 0
	mov r14, qword [rbp + 3*8]
	dec r14
	mov r13, sobNil

opt_args_loop5001:

	cmp r14, r15
	je opt_args_loop_end5001
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
	jmp opt_args_loop5001
opt_args_loop_end5001:

	mov qword [rbp + 4*8 + (r15 + 1)*8], r13
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count5001:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel5001:
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
make_closure1001:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1001
	jmp endLabel1001

bodyOfLambda1001:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 2
	jne bad_arg_count1001
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
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

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 


Lend1:

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1001:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1001:
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
make_closure1002:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1002
	jmp endLabel1002

bodyOfLambda1002:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 2
	jne bad_arg_count1002
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
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

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 


Lend2:

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1002:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1002:
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
make_closure5002:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda5002
	jmp endLabel5002

bodyOfLambda5002:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 0
	jl bad_arg_count5002
	mov r15, -1
	mov r14, qword [rbp + 3*8]
	dec r14
	mov r13, sobNil

opt_args_loop5002:

	cmp r14, r15
	je opt_args_loop_end5002
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
	jmp opt_args_loop5002
opt_args_loop_end5002:

	mov qword [rbp + 4*8 + (r15 + 1)*8], r13
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count5002:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel5002:
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
make_closure1003:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1003
	jmp endLabel1003

bodyOfLambda1003:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 2
	jne bad_arg_count1003
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	cmp r10, SOB_FALSE
	je L2
	mov rax, qword [rbp + (4+0)*8]

	jmp Lend3
L2:
	; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 


Lend3:

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1003:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1003:
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
make_closure1004:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1004
	jmp endLabel1004

bodyOfLambda1004:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 2
	jne bad_arg_count1004
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	cmp r10, SOB_FALSE
	je L3
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 


	jmp Lend4
L3:
	; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+1)*8]
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 


Lend4:

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1004:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1004:
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
make_closure5003:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda5003
	jmp endLabel5003

bodyOfLambda5003:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 0
	jl bad_arg_count5003
	mov r15, -1
	mov r14, qword [rbp + 3*8]
	dec r14
	mov r13, sobNil

opt_args_loop5003:

	cmp r14, r15
	je opt_args_loop_end5003
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
	jmp opt_args_loop5003
opt_args_loop_end5003:

	mov qword [rbp + 4*8 + (r15 + 1)*8], r13
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count5003:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel5003:
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
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure5004:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda5004
	jmp endLabel5004

bodyOfLambda5004:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 0
	jl bad_arg_count5004
	mov r15, -1
	mov r14, qword [rbp + 3*8]
	dec r14
	mov r13, sobNil

opt_args_loop5004:

	cmp r14, r15
	je opt_args_loop_end5004
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
	jmp opt_args_loop5004
opt_args_loop_end5004:

	mov qword [rbp + 4*8 + (r15 + 1)*8], r13
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
	mov rax, null?
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure134
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure134
not_a_closure134:

	mov rax, sobVoid
done_closure134:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	cmp r10, SOB_FALSE
	je L4
	; codegen for const start
	mov rax, sobTrue
	;code gen for constant end

	jmp Lend5
L4:
	; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
	mov rax, cdr
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure133
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure133
not_a_closure133:

	mov rax, sobVoid
done_closure133:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, null?
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure132
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure132
not_a_closure132:

	mov rax, sobVoid
done_closure132:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	cmp r10, SOB_FALSE
	je L5
	; codegen for const start
	mov rax, sobTrue
	;code gen for constant end

	jmp Lend6
L5:
	; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
	mov rax, cdr
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure131
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure131
not_a_closure131:

	mov rax, sobVoid
done_closure131:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, car
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure130
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure130
not_a_closure130:

	mov rax, sobVoid
done_closure130:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
	mov rax, car
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure129
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure129
not_a_closure129:

	mov rax, sobVoid
done_closure129:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 2
	mov rax, lessThan
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	cmp r10, SOB_FALSE
	je L6
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
	mov rax, cdr
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax
	mov rax, lessSthan
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

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 


	jmp Lend7
L6:
		; codegen for const start
	mov rax, sobFalse
	;code gen for constant end

Lend7:


Lend6:


Lend5:

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count5004:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel5004:
; end of creating a closure of lambda-simple 0

	mov rbx, lessSthan
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

	mov rax, sobNil
	push rax
	; codegen for const start
	mov rax, sobInt44
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, sobInt4
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, sobInt4
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, sobInt5
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, sobInt2
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, sobInt6
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, sobInt9
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, sobInt8
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, sobInt4
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, sobInt5
	;code gen for constant end
	push rax

	push 10
	mov rax, lessSthan
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure135
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure135
not_a_closure135:

	mov rax, sobVoid
done_closure135:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rax, [rax]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure1005:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1005
	jmp endLabel1005

bodyOfLambda1005:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 0
	jne bad_arg_count1005
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	; codegen for const start
	mov rax, sobInt5
	;code gen for constant end
	push rax

	push 1
	mov rax, makeString
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure137
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure137
not_a_closure137:

	mov rax, sobVoid
done_closure137:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1005:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1005:
; end of creating a closure of lambda-simple 0

	push rax

	push 1
	mov rax, procedure?
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure136
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure136
not_a_closure136:

	mov rax, sobVoid
done_closure136:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rax, [rax]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end

; start
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	; codegen for const start
	mov rax, sobFalse
	;code gen for constant end
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	; codegen for const start
	mov rax, sobTrue
	;code gen for constant end
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure1136:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1136
	jmp endLabel1136

bodyOfLambda1136:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1136
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
copy_args_loop1137:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1137
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1137

done_copy_args1137:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1137:
	cmp r10, 0
	je done_copy_env1137
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1137
done_copy_env1137:

	mov qword [rbx + 8*r15], 0
make_closure1137:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1137
	jmp endLabel1137

bodyOfLambda1137:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1137
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure271
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure271
not_a_closure271:

	mov rax, sobVoid
done_closure271:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure270
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure270
not_a_closure270:

	mov rax, sobVoid
done_closure270:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure269
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure269
not_a_closure269:

	mov rax, sobVoid
done_closure269:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure268
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure268
not_a_closure268:

	mov rax, sobVoid
done_closure268:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure267
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure267
not_a_closure267:

	mov rax, sobVoid
done_closure267:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1137:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1137:
; end of creating a closure of lambda-simple 1

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1136:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1136:
; end of creating a closure of lambda-simple 0

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure1134:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1134
	jmp endLabel1134

bodyOfLambda1134:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1134
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
copy_args_loop1135:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1135
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1135

done_copy_args1135:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1135:
	cmp r10, 0
	je done_copy_env1135
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1135
done_copy_env1135:

	mov qword [rbx + 8*r15], 0
make_closure1135:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1135
	jmp endLabel1135

bodyOfLambda1135:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1135
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure266
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure266
not_a_closure266:

	mov rax, sobVoid
done_closure266:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure265
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure265
not_a_closure265:

	mov rax, sobVoid
done_closure265:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure264
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure264
not_a_closure264:

	mov rax, sobVoid
done_closure264:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1135:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1135:
; end of creating a closure of lambda-simple 1

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1134:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1134:
; end of creating a closure of lambda-simple 0

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure1132:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1132
	jmp endLabel1132

bodyOfLambda1132:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1132
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
copy_args_loop1133:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1133
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1133

done_copy_args1133:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1133:
	cmp r10, 0
	je done_copy_env1133
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1133
done_copy_env1133:

	mov qword [rbx + 8*r15], 0
make_closure1133:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1133
	jmp endLabel1133

bodyOfLambda1133:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1133
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure263
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure263
not_a_closure263:

	mov rax, sobVoid
done_closure263:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure262
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure262
not_a_closure262:

	mov rax, sobVoid
done_closure262:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1133:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1133:
; end of creating a closure of lambda-simple 1

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1132:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1132:
; end of creating a closure of lambda-simple 0

	push rax

	push 1
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure1130:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1130
	jmp endLabel1130

bodyOfLambda1130:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1130
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
copy_args_loop1131:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1131
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1131

done_copy_args1131:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1131:
	cmp r10, 0
	je done_copy_env1131
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1131
done_copy_env1131:

	mov qword [rbx + 8*r15], 0
make_closure1131:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1131
	jmp endLabel1131

bodyOfLambda1131:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1131
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure261
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure261
not_a_closure261:

	mov rax, sobVoid
done_closure261:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure260
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure260
not_a_closure260:

	mov rax, sobVoid
done_closure260:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure259
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure259
not_a_closure259:

	mov rax, sobVoid
done_closure259:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1131:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1131:
; end of creating a closure of lambda-simple 1

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1130:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1130:
; end of creating a closure of lambda-simple 0

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure258
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure258
not_a_closure258:

	mov rax, sobVoid
done_closure258:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure1121:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1121
	jmp endLabel1121

bodyOfLambda1121:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1121
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
copy_args_loop1122:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1122
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1122

done_copy_args1122:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1122:
	cmp r10, 0
	je done_copy_env1122
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1122
done_copy_env1122:

	mov qword [rbx + 8*r15], 0
make_closure1122:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1122
	jmp endLabel1122

bodyOfLambda1122:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1122
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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
copy_args_loop1128:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1128
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1128

done_copy_args1128:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1128:
	cmp r10, 1
	je done_copy_env1128
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1128
done_copy_env1128:

	mov qword [rbx + 8*r15], 0
make_closure1128:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1128
	jmp endLabel1128

bodyOfLambda1128:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1128
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
copy_args_loop1129:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1129
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1129

done_copy_args1129:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1129:
	cmp r10, 2
	je done_copy_env1129
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1129
done_copy_env1129:

	mov qword [rbx + 8*r15], 0
make_closure1129:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1129
	jmp endLabel1129

bodyOfLambda1129:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1129
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1129:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1129:
; end of creating a closure of lambda-simple 3

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1128:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1128:
; end of creating a closure of lambda-simple 2

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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
copy_args_loop1123:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1123
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1123

done_copy_args1123:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1123:
	cmp r10, 1
	je done_copy_env1123
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1123
done_copy_env1123:

	mov qword [rbx + 8*r15], 0
make_closure1123:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1123
	jmp endLabel1123

bodyOfLambda1123:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1123
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
copy_args_loop1124:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1124
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1124

done_copy_args1124:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1124:
	cmp r10, 2
	je done_copy_env1124
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1124
done_copy_env1124:

	mov qword [rbx + 8*r15], 0
make_closure1124:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1124
	jmp endLabel1124

bodyOfLambda1124:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1124
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1125:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1125
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1125

done_copy_args1125:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1125:
	cmp r10, 3
	je done_copy_env1125
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1125
done_copy_env1125:

	mov qword [rbx + 8*r15], 0
make_closure1125:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1125
	jmp endLabel1125

bodyOfLambda1125:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1125
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1126:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1126
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1126

done_copy_args1126:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1126:
	cmp r10, 4
	je done_copy_env1126
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1126
done_copy_env1126:

	mov qword [rbx + 8*r15], 0
make_closure1126:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1126
	jmp endLabel1126

bodyOfLambda1126:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1126
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1127:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1127
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1127

done_copy_args1127:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1127:
	cmp r10, 5
	je done_copy_env1127
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1127
done_copy_env1127:

	mov qword [rbx + 8*r15], 0
make_closure1127:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1127
	jmp endLabel1127

bodyOfLambda1127:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1127
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 1*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure257
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure257
not_a_closure257:

	mov rax, sobVoid
done_closure257:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure256
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure256
not_a_closure256:

	mov rax, sobVoid
done_closure256:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure255
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure255
not_a_closure255:

	mov rax, sobVoid
done_closure255:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1127:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1127:
; end of creating a closure of lambda-simple 6

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1126:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1126:
; end of creating a closure of lambda-simple 5

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1125:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1125:
; end of creating a closure of lambda-simple 4

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure254
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure254
not_a_closure254:

	mov rax, sobVoid
done_closure254:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure253
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure253
not_a_closure253:

	mov rax, sobVoid
done_closure253:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1124:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1124:
; end of creating a closure of lambda-simple 3

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1123:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1123:
; end of creating a closure of lambda-simple 2

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure252
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure252
not_a_closure252:

	mov rax, sobVoid
done_closure252:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure251
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure251
not_a_closure251:

	mov rax, sobVoid
done_closure251:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure250
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure250
not_a_closure250:

	mov rax, sobVoid
done_closure250:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1122:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1122:
; end of creating a closure of lambda-simple 1

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1121:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1121:
; end of creating a closure of lambda-simple 0

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure249
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure249
not_a_closure249:

	mov rax, sobVoid
done_closure249:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure248
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure248
not_a_closure248:

	mov rax, sobVoid
done_closure248:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure1112:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1112
	jmp endLabel1112

bodyOfLambda1112:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1112
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
copy_args_loop1113:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1113
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1113

done_copy_args1113:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1113:
	cmp r10, 0
	je done_copy_env1113
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1113
done_copy_env1113:

	mov qword [rbx + 8*r15], 0
make_closure1113:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1113
	jmp endLabel1113

bodyOfLambda1113:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1113
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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
copy_args_loop1119:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1119
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1119

done_copy_args1119:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1119:
	cmp r10, 1
	je done_copy_env1119
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1119
done_copy_env1119:

	mov qword [rbx + 8*r15], 0
make_closure1119:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1119
	jmp endLabel1119

bodyOfLambda1119:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1119
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
copy_args_loop1120:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1120
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1120

done_copy_args1120:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1120:
	cmp r10, 2
	je done_copy_env1120
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1120
done_copy_env1120:

	mov qword [rbx + 8*r15], 0
make_closure1120:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1120
	jmp endLabel1120

bodyOfLambda1120:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1120
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1120:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1120:
; end of creating a closure of lambda-simple 3

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1119:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1119:
; end of creating a closure of lambda-simple 2

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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
copy_args_loop1114:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1114
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1114

done_copy_args1114:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1114:
	cmp r10, 1
	je done_copy_env1114
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1114
done_copy_env1114:

	mov qword [rbx + 8*r15], 0
make_closure1114:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1114
	jmp endLabel1114

bodyOfLambda1114:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1114
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
copy_args_loop1115:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1115
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1115

done_copy_args1115:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1115:
	cmp r10, 2
	je done_copy_env1115
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1115
done_copy_env1115:

	mov qword [rbx + 8*r15], 0
make_closure1115:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1115
	jmp endLabel1115

bodyOfLambda1115:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1115
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1116:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1116
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1116

done_copy_args1116:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1116:
	cmp r10, 3
	je done_copy_env1116
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1116
done_copy_env1116:

	mov qword [rbx + 8*r15], 0
make_closure1116:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1116
	jmp endLabel1116

bodyOfLambda1116:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1116
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1117:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1117
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1117

done_copy_args1117:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1117:
	cmp r10, 4
	je done_copy_env1117
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1117
done_copy_env1117:

	mov qword [rbx + 8*r15], 0
make_closure1117:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1117
	jmp endLabel1117

bodyOfLambda1117:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1117
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1118:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1118
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1118

done_copy_args1118:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1118:
	cmp r10, 5
	je done_copy_env1118
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1118
done_copy_env1118:

	mov qword [rbx + 8*r15], 0
make_closure1118:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1118
	jmp endLabel1118

bodyOfLambda1118:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1118
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 1*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure247
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure247
not_a_closure247:

	mov rax, sobVoid
done_closure247:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure246
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure246
not_a_closure246:

	mov rax, sobVoid
done_closure246:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure245
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure245
not_a_closure245:

	mov rax, sobVoid
done_closure245:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1118:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1118:
; end of creating a closure of lambda-simple 6

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1117:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1117:
; end of creating a closure of lambda-simple 5

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1116:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1116:
; end of creating a closure of lambda-simple 4

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure244
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure244
not_a_closure244:

	mov rax, sobVoid
done_closure244:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure243
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure243
not_a_closure243:

	mov rax, sobVoid
done_closure243:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1115:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1115:
; end of creating a closure of lambda-simple 3

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1114:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1114:
; end of creating a closure of lambda-simple 2

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure242
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure242
not_a_closure242:

	mov rax, sobVoid
done_closure242:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure241
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure241
not_a_closure241:

	mov rax, sobVoid
done_closure241:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure240
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure240
not_a_closure240:

	mov rax, sobVoid
done_closure240:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1113:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1113:
; end of creating a closure of lambda-simple 1

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1112:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1112:
; end of creating a closure of lambda-simple 0

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure239
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure239
not_a_closure239:

	mov rax, sobVoid
done_closure239:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure238
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure238
not_a_closure238:

	mov rax, sobVoid
done_closure238:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure1110:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1110
	jmp endLabel1110

bodyOfLambda1110:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1110
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
copy_args_loop1111:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1111
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1111

done_copy_args1111:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1111:
	cmp r10, 0
	je done_copy_env1111
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1111
done_copy_env1111:

	mov qword [rbx + 8*r15], 0
make_closure1111:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1111
	jmp endLabel1111

bodyOfLambda1111:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1111
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure237
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure237
not_a_closure237:

	mov rax, sobVoid
done_closure237:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure236
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure236
not_a_closure236:

	mov rax, sobVoid
done_closure236:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure235
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure235
not_a_closure235:

	mov rax, sobVoid
done_closure235:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure234
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure234
not_a_closure234:

	mov rax, sobVoid
done_closure234:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure233
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure233
not_a_closure233:

	mov rax, sobVoid
done_closure233:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1111:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1111:
; end of creating a closure of lambda-simple 1

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1110:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1110:
; end of creating a closure of lambda-simple 0

	push rax

	push 1
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure1076:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1076
	jmp endLabel1076

bodyOfLambda1076:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1076
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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
copy_args_loop1109:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1109
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1109

done_copy_args1109:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1109:
	cmp r10, 0
	je done_copy_env1109
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1109
done_copy_env1109:

	mov qword [rbx + 8*r15], 0
make_closure1109:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1109
	jmp endLabel1109

bodyOfLambda1109:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1109
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1109:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1109:
; end of creating a closure of lambda-simple 1

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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
copy_args_loop1108:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1108
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1108

done_copy_args1108:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1108:
	cmp r10, 0
	je done_copy_env1108
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1108
done_copy_env1108:

	mov qword [rbx + 8*r15], 0
make_closure1108:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1108
	jmp endLabel1108

bodyOfLambda1108:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1108
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1108:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1108:
; end of creating a closure of lambda-simple 1

	push rax

	push 1
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
copy_args_loop1105:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1105
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1105

done_copy_args1105:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1105:
	cmp r10, 0
	je done_copy_env1105
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1105
done_copy_env1105:

	mov qword [rbx + 8*r15], 0
make_closure1105:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1105
	jmp endLabel1105

bodyOfLambda1105:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1105
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
copy_args_loop1106:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1106
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1106

done_copy_args1106:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1106:
	cmp r10, 1
	je done_copy_env1106
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1106
done_copy_env1106:

	mov qword [rbx + 8*r15], 0
make_closure1106:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1106
	jmp endLabel1106

bodyOfLambda1106:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1106
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
copy_args_loop1107:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1107
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1107

done_copy_args1107:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1107:
	cmp r10, 2
	je done_copy_env1107
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1107
done_copy_env1107:

	mov qword [rbx + 8*r15], 0
make_closure1107:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1107
	jmp endLabel1107

bodyOfLambda1107:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1107
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 1*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure232
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure232
not_a_closure232:

	mov rax, sobVoid
done_closure232:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure231
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure231
not_a_closure231:

	mov rax, sobVoid
done_closure231:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1107:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1107:
; end of creating a closure of lambda-simple 3

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1106:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1106:
; end of creating a closure of lambda-simple 2

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1105:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1105:
; end of creating a closure of lambda-simple 1

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure230
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure230
not_a_closure230:

	mov rax, sobVoid
done_closure230:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure229
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure229
not_a_closure229:

	mov rax, sobVoid
done_closure229:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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
copy_args_loop1080:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1080
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1080

done_copy_args1080:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1080:
	cmp r10, 0
	je done_copy_env1080
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1080
done_copy_env1080:

	mov qword [rbx + 8*r15], 0
make_closure1080:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1080
	jmp endLabel1080

bodyOfLambda1080:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1080
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
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
copy_args_loop1102:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1102
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1102

done_copy_args1102:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1102:
	cmp r10, 1
	je done_copy_env1102
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1102
done_copy_env1102:

	mov qword [rbx + 8*r15], 0
make_closure1102:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1102
	jmp endLabel1102

bodyOfLambda1102:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1102
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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
copy_args_loop1103:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1103
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1103

done_copy_args1103:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1103:
	cmp r10, 2
	je done_copy_env1103
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1103
done_copy_env1103:

	mov qword [rbx + 8*r15], 0
make_closure1103:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1103
	jmp endLabel1103

bodyOfLambda1103:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1103
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1104:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1104
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1104

done_copy_args1104:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1104:
	cmp r10, 3
	je done_copy_env1104
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1104
done_copy_env1104:

	mov qword [rbx + 8*r15], 0
make_closure1104:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1104
	jmp endLabel1104

bodyOfLambda1104:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1104
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1104:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1104:
; end of creating a closure of lambda-simple 4

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1103:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1103:
; end of creating a closure of lambda-simple 3

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure228
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure228
not_a_closure228:

	mov rax, sobVoid
done_closure228:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1102:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1102:
; end of creating a closure of lambda-simple 2

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure227
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure227
not_a_closure227:

	mov rax, sobVoid
done_closure227:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
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
copy_args_loop1099:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1099
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1099

done_copy_args1099:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1099:
	cmp r10, 1
	je done_copy_env1099
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1099
done_copy_env1099:

	mov qword [rbx + 8*r15], 0
make_closure1099:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1099
	jmp endLabel1099

bodyOfLambda1099:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1099
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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
copy_args_loop1100:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1100
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1100

done_copy_args1100:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1100:
	cmp r10, 2
	je done_copy_env1100
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1100
done_copy_env1100:

	mov qword [rbx + 8*r15], 0
make_closure1100:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1100
	jmp endLabel1100

bodyOfLambda1100:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1100
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1101:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1101
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1101

done_copy_args1101:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1101:
	cmp r10, 3
	je done_copy_env1101
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1101
done_copy_env1101:

	mov qword [rbx + 8*r15], 0
make_closure1101:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1101
	jmp endLabel1101

bodyOfLambda1101:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1101
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1101:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1101:
; end of creating a closure of lambda-simple 4

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1100:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1100:
; end of creating a closure of lambda-simple 3

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure226
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure226
not_a_closure226:

	mov rax, sobVoid
done_closure226:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1099:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1099:
; end of creating a closure of lambda-simple 2

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure225
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure225
not_a_closure225:

	mov rax, sobVoid
done_closure225:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
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
copy_args_loop1090:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1090
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1090

done_copy_args1090:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1090:
	cmp r10, 1
	je done_copy_env1090
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1090
done_copy_env1090:

	mov qword [rbx + 8*r15], 0
make_closure1090:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1090
	jmp endLabel1090

bodyOfLambda1090:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1090
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
copy_args_loop1091:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1091
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1091

done_copy_args1091:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1091:
	cmp r10, 2
	je done_copy_env1091
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1091
done_copy_env1091:

	mov qword [rbx + 8*r15], 0
make_closure1091:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1091
	jmp endLabel1091

bodyOfLambda1091:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1091
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1097:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1097
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1097

done_copy_args1097:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1097:
	cmp r10, 3
	je done_copy_env1097
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1097
done_copy_env1097:

	mov qword [rbx + 8*r15], 0
make_closure1097:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1097
	jmp endLabel1097

bodyOfLambda1097:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1097
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1098:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1098
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1098

done_copy_args1098:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1098:
	cmp r10, 4
	je done_copy_env1098
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1098
done_copy_env1098:

	mov qword [rbx + 8*r15], 0
make_closure1098:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1098
	jmp endLabel1098

bodyOfLambda1098:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1098
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1098:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1098:
; end of creating a closure of lambda-simple 5

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1097:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1097:
; end of creating a closure of lambda-simple 4

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1092:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1092
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1092

done_copy_args1092:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1092:
	cmp r10, 3
	je done_copy_env1092
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1092
done_copy_env1092:

	mov qword [rbx + 8*r15], 0
make_closure1092:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1092
	jmp endLabel1092

bodyOfLambda1092:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1092
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1093:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1093
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1093

done_copy_args1093:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1093:
	cmp r10, 4
	je done_copy_env1093
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1093
done_copy_env1093:

	mov qword [rbx + 8*r15], 0
make_closure1093:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1093
	jmp endLabel1093

bodyOfLambda1093:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1093
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1094:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1094
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1094

done_copy_args1094:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1094:
	cmp r10, 5
	je done_copy_env1094
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1094
done_copy_env1094:

	mov qword [rbx + 8*r15], 0
make_closure1094:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1094
	jmp endLabel1094

bodyOfLambda1094:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1094
; start of creating a closure of lambda-simple 7

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 64
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
copy_args_loop1095:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1095
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1095

done_copy_args1095:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1095:
	cmp r10, 6
	je done_copy_env1095
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1095
done_copy_env1095:

	mov qword [rbx + 8*r15], 0
make_closure1095:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1095
	jmp endLabel1095

bodyOfLambda1095:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1095
; start of creating a closure of lambda-simple 8

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 72
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
copy_args_loop1096:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1096
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1096

done_copy_args1096:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1096:
	cmp r10, 7
	je done_copy_env1096
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1096
done_copy_env1096:

	mov qword [rbx + 8*r15], 0
make_closure1096:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1096
	jmp endLabel1096

bodyOfLambda1096:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1096
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 1*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure224
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure224
not_a_closure224:

	mov rax, sobVoid
done_closure224:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure223
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure223
not_a_closure223:

	mov rax, sobVoid
done_closure223:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure222
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure222
not_a_closure222:

	mov rax, sobVoid
done_closure222:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1096:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1096:
; end of creating a closure of lambda-simple 8

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1095:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1095:
; end of creating a closure of lambda-simple 7

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1094:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1094:
; end of creating a closure of lambda-simple 6

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure221
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure221
not_a_closure221:

	mov rax, sobVoid
done_closure221:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure220
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure220
not_a_closure220:

	mov rax, sobVoid
done_closure220:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1093:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1093:
; end of creating a closure of lambda-simple 5

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1092:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1092:
; end of creating a closure of lambda-simple 4

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure219
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure219
not_a_closure219:

	mov rax, sobVoid
done_closure219:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure218
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure218
not_a_closure218:

	mov rax, sobVoid
done_closure218:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure217
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure217
not_a_closure217:

	mov rax, sobVoid
done_closure217:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1091:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1091:
; end of creating a closure of lambda-simple 3

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1090:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1090:
; end of creating a closure of lambda-simple 2

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure216
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure216
not_a_closure216:

	mov rax, sobVoid
done_closure216:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure215
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure215
not_a_closure215:

	mov rax, sobVoid
done_closure215:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
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
copy_args_loop1087:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1087
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1087

done_copy_args1087:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1087:
	cmp r10, 1
	je done_copy_env1087
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1087
done_copy_env1087:

	mov qword [rbx + 8*r15], 0
make_closure1087:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1087
	jmp endLabel1087

bodyOfLambda1087:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1087
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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
copy_args_loop1088:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1088
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1088

done_copy_args1088:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1088:
	cmp r10, 2
	je done_copy_env1088
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1088
done_copy_env1088:

	mov qword [rbx + 8*r15], 0
make_closure1088:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1088
	jmp endLabel1088

bodyOfLambda1088:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1088
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1089:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1089
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1089

done_copy_args1089:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1089:
	cmp r10, 3
	je done_copy_env1089
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1089
done_copy_env1089:

	mov qword [rbx + 8*r15], 0
make_closure1089:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1089
	jmp endLabel1089

bodyOfLambda1089:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1089
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1089:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1089:
; end of creating a closure of lambda-simple 4

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1088:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1088:
; end of creating a closure of lambda-simple 3

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure214
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure214
not_a_closure214:

	mov rax, sobVoid
done_closure214:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1087:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1087:
; end of creating a closure of lambda-simple 2

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure213
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure213
not_a_closure213:

	mov rax, sobVoid
done_closure213:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
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
copy_args_loop1084:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1084
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1084

done_copy_args1084:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1084:
	cmp r10, 1
	je done_copy_env1084
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1084
done_copy_env1084:

	mov qword [rbx + 8*r15], 0
make_closure1084:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1084
	jmp endLabel1084

bodyOfLambda1084:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1084
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
copy_args_loop1085:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1085
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1085

done_copy_args1085:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1085:
	cmp r10, 2
	je done_copy_env1085
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1085
done_copy_env1085:

	mov qword [rbx + 8*r15], 0
make_closure1085:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1085
	jmp endLabel1085

bodyOfLambda1085:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1085
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1086:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1086
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1086

done_copy_args1086:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1086:
	cmp r10, 3
	je done_copy_env1086
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1086
done_copy_env1086:

	mov qword [rbx + 8*r15], 0
make_closure1086:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1086
	jmp endLabel1086

bodyOfLambda1086:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1086
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 1*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure212
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure212
not_a_closure212:

	mov rax, sobVoid
done_closure212:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure211
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure211
not_a_closure211:

	mov rax, sobVoid
done_closure211:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure210
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure210
not_a_closure210:

	mov rax, sobVoid
done_closure210:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1086:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1086:
; end of creating a closure of lambda-simple 4

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1085:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1085:
; end of creating a closure of lambda-simple 3

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1084:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1084:
; end of creating a closure of lambda-simple 2

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure209
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure209
not_a_closure209:

	mov rax, sobVoid
done_closure209:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
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
copy_args_loop1081:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1081
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1081

done_copy_args1081:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1081:
	cmp r10, 1
	je done_copy_env1081
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1081
done_copy_env1081:

	mov qword [rbx + 8*r15], 0
make_closure1081:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1081
	jmp endLabel1081

bodyOfLambda1081:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1081
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
copy_args_loop1082:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1082
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1082

done_copy_args1082:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1082:
	cmp r10, 2
	je done_copy_env1082
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1082
done_copy_env1082:

	mov qword [rbx + 8*r15], 0
make_closure1082:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1082
	jmp endLabel1082

bodyOfLambda1082:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1082
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1083:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1083
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1083

done_copy_args1083:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1083:
	cmp r10, 3
	je done_copy_env1083
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1083
done_copy_env1083:

	mov qword [rbx + 8*r15], 0
make_closure1083:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1083
	jmp endLabel1083

bodyOfLambda1083:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1083
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 1*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure208
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure208
not_a_closure208:

	mov rax, sobVoid
done_closure208:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure207
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure207
not_a_closure207:

	mov rax, sobVoid
done_closure207:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1083:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1083:
; end of creating a closure of lambda-simple 4

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1082:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1082:
; end of creating a closure of lambda-simple 3

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1081:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1081:
; end of creating a closure of lambda-simple 2

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure206
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure206
not_a_closure206:

	mov rax, sobVoid
done_closure206:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure205
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure205
not_a_closure205:

	mov rax, sobVoid
done_closure205:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1080:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1080:
; end of creating a closure of lambda-simple 1

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure204
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure204
not_a_closure204:

	mov rax, sobVoid
done_closure204:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure203
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure203
not_a_closure203:

	mov rax, sobVoid
done_closure203:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
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
copy_args_loop1077:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1077
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1077

done_copy_args1077:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1077:
	cmp r10, 0
	je done_copy_env1077
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1077
done_copy_env1077:

	mov qword [rbx + 8*r15], 0
make_closure1077:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1077
	jmp endLabel1077

bodyOfLambda1077:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1077
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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
copy_args_loop1078:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1078
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1078

done_copy_args1078:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1078:
	cmp r10, 1
	je done_copy_env1078
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1078
done_copy_env1078:

	mov qword [rbx + 8*r15], 0
make_closure1078:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1078
	jmp endLabel1078

bodyOfLambda1078:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1078
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
copy_args_loop1079:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1079
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1079

done_copy_args1079:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1079:
	cmp r10, 2
	je done_copy_env1079
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1079
done_copy_env1079:

	mov qword [rbx + 8*r15], 0
make_closure1079:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1079
	jmp endLabel1079

bodyOfLambda1079:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1079
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1079:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1079:
; end of creating a closure of lambda-simple 3

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1078:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1078:
; end of creating a closure of lambda-simple 2

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure202
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure202
not_a_closure202:

	mov rax, sobVoid
done_closure202:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1077:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1077:
; end of creating a closure of lambda-simple 1

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure201
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure201
not_a_closure201:

	mov rax, sobVoid
done_closure201:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1076:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1076:
; end of creating a closure of lambda-simple 0

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure200
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure200
not_a_closure200:

	mov rax, sobVoid
done_closure200:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure1006:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1006
	jmp endLabel1006

bodyOfLambda1006:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1006
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
copy_args_loop1007:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1007
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1007

done_copy_args1007:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1007:
	cmp r10, 0
	je done_copy_env1007
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1007
done_copy_env1007:

	mov qword [rbx + 8*r15], 0
make_closure1007:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1007
	jmp endLabel1007

bodyOfLambda1007:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1007
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
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
copy_args_loop1050:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1050
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1050

done_copy_args1050:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1050:
	cmp r10, 1
	je done_copy_env1050
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1050
done_copy_env1050:

	mov qword [rbx + 8*r15], 0
make_closure1050:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1050
	jmp endLabel1050

bodyOfLambda1050:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1050
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
copy_args_loop1051:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1051
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1051

done_copy_args1051:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1051:
	cmp r10, 2
	je done_copy_env1051
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1051
done_copy_env1051:

	mov qword [rbx + 8*r15], 0
make_closure1051:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1051
	jmp endLabel1051

bodyOfLambda1051:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1051
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1052:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1052
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1052

done_copy_args1052:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1052:
	cmp r10, 3
	je done_copy_env1052
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1052
done_copy_env1052:

	mov qword [rbx + 8*r15], 0
make_closure1052:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1052
	jmp endLabel1052

bodyOfLambda1052:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1052
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1074:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1074
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1074

done_copy_args1074:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1074:
	cmp r10, 4
	je done_copy_env1074
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1074
done_copy_env1074:

	mov qword [rbx + 8*r15], 0
make_closure1074:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1074
	jmp endLabel1074

bodyOfLambda1074:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1074
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1075:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1075
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1075

done_copy_args1075:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1075:
	cmp r10, 5
	je done_copy_env1075
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1075
done_copy_env1075:

	mov qword [rbx + 8*r15], 0
make_closure1075:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1075
	jmp endLabel1075

bodyOfLambda1075:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1075
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1075:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1075:
; end of creating a closure of lambda-simple 6

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1074:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1074:
; end of creating a closure of lambda-simple 5

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1072:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1072
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1072

done_copy_args1072:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1072:
	cmp r10, 4
	je done_copy_env1072
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1072
done_copy_env1072:

	mov qword [rbx + 8*r15], 0
make_closure1072:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1072
	jmp endLabel1072

bodyOfLambda1072:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1072
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1073:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1073
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1073

done_copy_args1073:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1073:
	cmp r10, 5
	je done_copy_env1073
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1073
done_copy_env1073:

	mov qword [rbx + 8*r15], 0
make_closure1073:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1073
	jmp endLabel1073

bodyOfLambda1073:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1073
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1073:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1073:
; end of creating a closure of lambda-simple 6

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1072:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1072:
; end of creating a closure of lambda-simple 5

	push rax

	push 1
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1069:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1069
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1069

done_copy_args1069:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1069:
	cmp r10, 4
	je done_copy_env1069
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1069
done_copy_env1069:

	mov qword [rbx + 8*r15], 0
make_closure1069:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1069
	jmp endLabel1069

bodyOfLambda1069:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1069
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1070:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1070
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1070

done_copy_args1070:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1070:
	cmp r10, 5
	je done_copy_env1070
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1070
done_copy_env1070:

	mov qword [rbx + 8*r15], 0
make_closure1070:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1070
	jmp endLabel1070

bodyOfLambda1070:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1070
; start of creating a closure of lambda-simple 7

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 64
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
copy_args_loop1071:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1071
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1071

done_copy_args1071:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1071:
	cmp r10, 6
	je done_copy_env1071
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1071
done_copy_env1071:

	mov qword [rbx + 8*r15], 0
make_closure1071:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1071
	jmp endLabel1071

bodyOfLambda1071:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1071
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 1*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure199
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure199
not_a_closure199:

	mov rax, sobVoid
done_closure199:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure198
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure198
not_a_closure198:

	mov rax, sobVoid
done_closure198:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1071:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1071:
; end of creating a closure of lambda-simple 7

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1070:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1070:
; end of creating a closure of lambda-simple 6

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1069:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1069:
; end of creating a closure of lambda-simple 5

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure197
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure197
not_a_closure197:

	mov rax, sobVoid
done_closure197:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure196
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure196
not_a_closure196:

	mov rax, sobVoid
done_closure196:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1056:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1056
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1056

done_copy_args1056:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1056:
	cmp r10, 4
	je done_copy_env1056
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1056
done_copy_env1056:

	mov qword [rbx + 8*r15], 0
make_closure1056:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1056
	jmp endLabel1056

bodyOfLambda1056:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1056
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1066:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1066
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1066

done_copy_args1066:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1066:
	cmp r10, 5
	je done_copy_env1066
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1066
done_copy_env1066:

	mov qword [rbx + 8*r15], 0
make_closure1066:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1066
	jmp endLabel1066

bodyOfLambda1066:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1066
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 7

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 64
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
copy_args_loop1067:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1067
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1067

done_copy_args1067:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1067:
	cmp r10, 6
	je done_copy_env1067
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1067
done_copy_env1067:

	mov qword [rbx + 8*r15], 0
make_closure1067:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1067
	jmp endLabel1067

bodyOfLambda1067:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1067
; start of creating a closure of lambda-simple 8

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 72
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
copy_args_loop1068:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1068
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1068

done_copy_args1068:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1068:
	cmp r10, 7
	je done_copy_env1068
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1068
done_copy_env1068:

	mov qword [rbx + 8*r15], 0
make_closure1068:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1068
	jmp endLabel1068

bodyOfLambda1068:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1068
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1068:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1068:
; end of creating a closure of lambda-simple 8

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1067:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1067:
; end of creating a closure of lambda-simple 7

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure195
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure195
not_a_closure195:

	mov rax, sobVoid
done_closure195:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1066:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1066:
; end of creating a closure of lambda-simple 6

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure194
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure194
not_a_closure194:

	mov rax, sobVoid
done_closure194:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1063:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1063
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1063

done_copy_args1063:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1063:
	cmp r10, 5
	je done_copy_env1063
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1063
done_copy_env1063:

	mov qword [rbx + 8*r15], 0
make_closure1063:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1063
	jmp endLabel1063

bodyOfLambda1063:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1063
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 7

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 64
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
copy_args_loop1064:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1064
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1064

done_copy_args1064:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1064:
	cmp r10, 6
	je done_copy_env1064
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1064
done_copy_env1064:

	mov qword [rbx + 8*r15], 0
make_closure1064:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1064
	jmp endLabel1064

bodyOfLambda1064:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1064
; start of creating a closure of lambda-simple 8

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 72
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
copy_args_loop1065:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1065
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1065

done_copy_args1065:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1065:
	cmp r10, 7
	je done_copy_env1065
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1065
done_copy_env1065:

	mov qword [rbx + 8*r15], 0
make_closure1065:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1065
	jmp endLabel1065

bodyOfLambda1065:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1065
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1065:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1065:
; end of creating a closure of lambda-simple 8

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1064:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1064:
; end of creating a closure of lambda-simple 7

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure193
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure193
not_a_closure193:

	mov rax, sobVoid
done_closure193:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1063:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1063:
; end of creating a closure of lambda-simple 6

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure192
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure192
not_a_closure192:

	mov rax, sobVoid
done_closure192:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1060:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1060
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1060

done_copy_args1060:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1060:
	cmp r10, 5
	je done_copy_env1060
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1060
done_copy_env1060:

	mov qword [rbx + 8*r15], 0
make_closure1060:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1060
	jmp endLabel1060

bodyOfLambda1060:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1060
; start of creating a closure of lambda-simple 7

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 64
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
copy_args_loop1061:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1061
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1061

done_copy_args1061:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1061:
	cmp r10, 6
	je done_copy_env1061
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1061
done_copy_env1061:

	mov qword [rbx + 8*r15], 0
make_closure1061:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1061
	jmp endLabel1061

bodyOfLambda1061:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1061
; start of creating a closure of lambda-simple 8

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 72
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
copy_args_loop1062:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1062
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1062

done_copy_args1062:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1062:
	cmp r10, 7
	je done_copy_env1062
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1062
done_copy_env1062:

	mov qword [rbx + 8*r15], 0
make_closure1062:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1062
	jmp endLabel1062

bodyOfLambda1062:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1062
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 1*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure191
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure191
not_a_closure191:

	mov rax, sobVoid
done_closure191:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure190
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure190
not_a_closure190:

	mov rax, sobVoid
done_closure190:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure189
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure189
not_a_closure189:

	mov rax, sobVoid
done_closure189:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1062:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1062:
; end of creating a closure of lambda-simple 8

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1061:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1061:
; end of creating a closure of lambda-simple 7

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1060:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1060:
; end of creating a closure of lambda-simple 6

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure188
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure188
not_a_closure188:

	mov rax, sobVoid
done_closure188:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1057:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1057
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1057

done_copy_args1057:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1057:
	cmp r10, 5
	je done_copy_env1057
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1057
done_copy_env1057:

	mov qword [rbx + 8*r15], 0
make_closure1057:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1057
	jmp endLabel1057

bodyOfLambda1057:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1057
; start of creating a closure of lambda-simple 7

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 64
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
copy_args_loop1058:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1058
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1058

done_copy_args1058:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1058:
	cmp r10, 6
	je done_copy_env1058
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1058
done_copy_env1058:

	mov qword [rbx + 8*r15], 0
make_closure1058:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1058
	jmp endLabel1058

bodyOfLambda1058:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1058
; start of creating a closure of lambda-simple 8

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 72
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
copy_args_loop1059:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1059
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1059

done_copy_args1059:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1059:
	cmp r10, 7
	je done_copy_env1059
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1059
done_copy_env1059:

	mov qword [rbx + 8*r15], 0
make_closure1059:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1059
	jmp endLabel1059

bodyOfLambda1059:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1059
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 1*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure187
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure187
not_a_closure187:

	mov rax, sobVoid
done_closure187:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure186
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure186
not_a_closure186:

	mov rax, sobVoid
done_closure186:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1059:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1059:
; end of creating a closure of lambda-simple 8

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1058:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1058:
; end of creating a closure of lambda-simple 7

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1057:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1057:
; end of creating a closure of lambda-simple 6

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure185
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure185
not_a_closure185:

	mov rax, sobVoid
done_closure185:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure184
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure184
not_a_closure184:

	mov rax, sobVoid
done_closure184:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1056:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1056:
; end of creating a closure of lambda-simple 5

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure183
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure183
not_a_closure183:

	mov rax, sobVoid
done_closure183:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure182
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure182
not_a_closure182:

	mov rax, sobVoid
done_closure182:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1053:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1053
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1053

done_copy_args1053:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1053:
	cmp r10, 4
	je done_copy_env1053
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1053
done_copy_env1053:

	mov qword [rbx + 8*r15], 0
make_closure1053:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1053
	jmp endLabel1053

bodyOfLambda1053:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1053
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1054:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1054
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1054

done_copy_args1054:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1054:
	cmp r10, 5
	je done_copy_env1054
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1054
done_copy_env1054:

	mov qword [rbx + 8*r15], 0
make_closure1054:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1054
	jmp endLabel1054

bodyOfLambda1054:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1054
; start of creating a closure of lambda-simple 7

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 64
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
copy_args_loop1055:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1055
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1055

done_copy_args1055:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1055:
	cmp r10, 6
	je done_copy_env1055
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1055
done_copy_env1055:

	mov qword [rbx + 8*r15], 0
make_closure1055:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1055
	jmp endLabel1055

bodyOfLambda1055:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1055
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1055:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1055:
; end of creating a closure of lambda-simple 7

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1054:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1054:
; end of creating a closure of lambda-simple 6

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure181
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure181
not_a_closure181:

	mov rax, sobVoid
done_closure181:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1053:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1053:
; end of creating a closure of lambda-simple 5

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure180
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure180
not_a_closure180:

	mov rax, sobVoid
done_closure180:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1052:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1052:
; end of creating a closure of lambda-simple 4

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure179
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure179
not_a_closure179:

	mov rax, sobVoid
done_closure179:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure178
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure178
not_a_closure178:

	mov rax, sobVoid
done_closure178:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1051:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1051:
; end of creating a closure of lambda-simple 3

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1050:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1050:
; end of creating a closure of lambda-simple 2

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure177
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure177
not_a_closure177:

	mov rax, sobVoid
done_closure177:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure176
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure176
not_a_closure176:

	mov rax, sobVoid
done_closure176:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
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
copy_args_loop1044:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1044
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1044

done_copy_args1044:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1044:
	cmp r10, 1
	je done_copy_env1044
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1044
done_copy_env1044:

	mov qword [rbx + 8*r15], 0
make_closure1044:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1044
	jmp endLabel1044

bodyOfLambda1044:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1044
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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
copy_args_loop1048:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1048
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1048

done_copy_args1048:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1048:
	cmp r10, 2
	je done_copy_env1048
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1048
done_copy_env1048:

	mov qword [rbx + 8*r15], 0
make_closure1048:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1048
	jmp endLabel1048

bodyOfLambda1048:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1048
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1049:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1049
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1049

done_copy_args1049:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1049:
	cmp r10, 3
	je done_copy_env1049
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1049
done_copy_env1049:

	mov qword [rbx + 8*r15], 0
make_closure1049:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1049
	jmp endLabel1049

bodyOfLambda1049:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1049
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1049:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1049:
; end of creating a closure of lambda-simple 4

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1048:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1048:
; end of creating a closure of lambda-simple 3

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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
copy_args_loop1045:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1045
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1045

done_copy_args1045:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1045:
	cmp r10, 2
	je done_copy_env1045
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1045
done_copy_env1045:

	mov qword [rbx + 8*r15], 0
make_closure1045:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1045
	jmp endLabel1045

bodyOfLambda1045:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1045
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1046:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1046
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1046

done_copy_args1046:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1046:
	cmp r10, 3
	je done_copy_env1046
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1046
done_copy_env1046:

	mov qword [rbx + 8*r15], 0
make_closure1046:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1046
	jmp endLabel1046

bodyOfLambda1046:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1046
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1047:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1047
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1047

done_copy_args1047:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1047:
	cmp r10, 4
	je done_copy_env1047
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1047
done_copy_env1047:

	mov qword [rbx + 8*r15], 0
make_closure1047:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1047
	jmp endLabel1047

bodyOfLambda1047:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1047
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1047:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1047:
; end of creating a closure of lambda-simple 5

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1046:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1046:
; end of creating a closure of lambda-simple 4

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1045:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1045:
; end of creating a closure of lambda-simple 3

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure175
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure175
not_a_closure175:

	mov rax, sobVoid
done_closure175:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure174
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure174
not_a_closure174:

	mov rax, sobVoid
done_closure174:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1044:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1044:
; end of creating a closure of lambda-simple 2

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure173
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure173
not_a_closure173:

	mov rax, sobVoid
done_closure173:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
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
copy_args_loop1018:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1018
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1018

done_copy_args1018:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1018:
	cmp r10, 1
	je done_copy_env1018
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1018
done_copy_env1018:

	mov qword [rbx + 8*r15], 0
make_closure1018:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1018
	jmp endLabel1018

bodyOfLambda1018:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1018
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
copy_args_loop1019:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1019
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1019

done_copy_args1019:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1019:
	cmp r10, 2
	je done_copy_env1019
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1019
done_copy_env1019:

	mov qword [rbx + 8*r15], 0
make_closure1019:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1019
	jmp endLabel1019

bodyOfLambda1019:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1019
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1020:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1020
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1020

done_copy_args1020:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1020:
	cmp r10, 3
	je done_copy_env1020
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1020
done_copy_env1020:

	mov qword [rbx + 8*r15], 0
make_closure1020:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1020
	jmp endLabel1020

bodyOfLambda1020:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1020
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1042:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1042
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1042

done_copy_args1042:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1042:
	cmp r10, 4
	je done_copy_env1042
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1042
done_copy_env1042:

	mov qword [rbx + 8*r15], 0
make_closure1042:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1042
	jmp endLabel1042

bodyOfLambda1042:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1042
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1043:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1043
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1043

done_copy_args1043:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1043:
	cmp r10, 5
	je done_copy_env1043
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1043
done_copy_env1043:

	mov qword [rbx + 8*r15], 0
make_closure1043:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1043
	jmp endLabel1043

bodyOfLambda1043:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1043
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1043:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1043:
; end of creating a closure of lambda-simple 6

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1042:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1042:
; end of creating a closure of lambda-simple 5

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1040:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1040
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1040

done_copy_args1040:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1040:
	cmp r10, 4
	je done_copy_env1040
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1040
done_copy_env1040:

	mov qword [rbx + 8*r15], 0
make_closure1040:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1040
	jmp endLabel1040

bodyOfLambda1040:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1040
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1041:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1041
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1041

done_copy_args1041:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1041:
	cmp r10, 5
	je done_copy_env1041
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1041
done_copy_env1041:

	mov qword [rbx + 8*r15], 0
make_closure1041:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1041
	jmp endLabel1041

bodyOfLambda1041:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1041
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1041:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1041:
; end of creating a closure of lambda-simple 6

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1040:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1040:
; end of creating a closure of lambda-simple 5

	push rax

	push 1
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1037:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1037
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1037

done_copy_args1037:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1037:
	cmp r10, 4
	je done_copy_env1037
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1037
done_copy_env1037:

	mov qword [rbx + 8*r15], 0
make_closure1037:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1037
	jmp endLabel1037

bodyOfLambda1037:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1037
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1038:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1038
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1038

done_copy_args1038:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1038:
	cmp r10, 5
	je done_copy_env1038
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1038
done_copy_env1038:

	mov qword [rbx + 8*r15], 0
make_closure1038:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1038
	jmp endLabel1038

bodyOfLambda1038:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1038
; start of creating a closure of lambda-simple 7

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 64
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
copy_args_loop1039:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1039
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1039

done_copy_args1039:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1039:
	cmp r10, 6
	je done_copy_env1039
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1039
done_copy_env1039:

	mov qword [rbx + 8*r15], 0
make_closure1039:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1039
	jmp endLabel1039

bodyOfLambda1039:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1039
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 1*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure172
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure172
not_a_closure172:

	mov rax, sobVoid
done_closure172:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure171
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure171
not_a_closure171:

	mov rax, sobVoid
done_closure171:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1039:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1039:
; end of creating a closure of lambda-simple 7

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1038:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1038:
; end of creating a closure of lambda-simple 6

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1037:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1037:
; end of creating a closure of lambda-simple 5

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure170
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure170
not_a_closure170:

	mov rax, sobVoid
done_closure170:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure169
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure169
not_a_closure169:

	mov rax, sobVoid
done_closure169:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1024:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1024
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1024

done_copy_args1024:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1024:
	cmp r10, 4
	je done_copy_env1024
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1024
done_copy_env1024:

	mov qword [rbx + 8*r15], 0
make_closure1024:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1024
	jmp endLabel1024

bodyOfLambda1024:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1024
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1034:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1034
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1034

done_copy_args1034:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1034:
	cmp r10, 5
	je done_copy_env1034
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1034
done_copy_env1034:

	mov qword [rbx + 8*r15], 0
make_closure1034:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1034
	jmp endLabel1034

bodyOfLambda1034:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1034
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 7

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 64
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
copy_args_loop1035:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1035
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1035

done_copy_args1035:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1035:
	cmp r10, 6
	je done_copy_env1035
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1035
done_copy_env1035:

	mov qword [rbx + 8*r15], 0
make_closure1035:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1035
	jmp endLabel1035

bodyOfLambda1035:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1035
; start of creating a closure of lambda-simple 8

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 72
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
copy_args_loop1036:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1036
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1036

done_copy_args1036:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1036:
	cmp r10, 7
	je done_copy_env1036
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1036
done_copy_env1036:

	mov qword [rbx + 8*r15], 0
make_closure1036:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1036
	jmp endLabel1036

bodyOfLambda1036:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1036
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1036:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1036:
; end of creating a closure of lambda-simple 8

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1035:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1035:
; end of creating a closure of lambda-simple 7

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure168
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure168
not_a_closure168:

	mov rax, sobVoid
done_closure168:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1034:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1034:
; end of creating a closure of lambda-simple 6

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure167
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure167
not_a_closure167:

	mov rax, sobVoid
done_closure167:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1031:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1031
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1031

done_copy_args1031:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1031:
	cmp r10, 5
	je done_copy_env1031
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1031
done_copy_env1031:

	mov qword [rbx + 8*r15], 0
make_closure1031:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1031
	jmp endLabel1031

bodyOfLambda1031:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1031
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 7

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 64
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
copy_args_loop1032:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1032
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1032

done_copy_args1032:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1032:
	cmp r10, 6
	je done_copy_env1032
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1032
done_copy_env1032:

	mov qword [rbx + 8*r15], 0
make_closure1032:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1032
	jmp endLabel1032

bodyOfLambda1032:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1032
; start of creating a closure of lambda-simple 8

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 72
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
copy_args_loop1033:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1033
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1033

done_copy_args1033:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1033:
	cmp r10, 7
	je done_copy_env1033
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1033
done_copy_env1033:

	mov qword [rbx + 8*r15], 0
make_closure1033:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1033
	jmp endLabel1033

bodyOfLambda1033:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1033
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1033:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1033:
; end of creating a closure of lambda-simple 8

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1032:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1032:
; end of creating a closure of lambda-simple 7

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure166
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure166
not_a_closure166:

	mov rax, sobVoid
done_closure166:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1031:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1031:
; end of creating a closure of lambda-simple 6

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure165
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure165
not_a_closure165:

	mov rax, sobVoid
done_closure165:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1028:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1028
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1028

done_copy_args1028:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1028:
	cmp r10, 5
	je done_copy_env1028
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1028
done_copy_env1028:

	mov qword [rbx + 8*r15], 0
make_closure1028:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1028
	jmp endLabel1028

bodyOfLambda1028:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1028
; start of creating a closure of lambda-simple 7

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 64
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
copy_args_loop1029:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1029
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1029

done_copy_args1029:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1029:
	cmp r10, 6
	je done_copy_env1029
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1029
done_copy_env1029:

	mov qword [rbx + 8*r15], 0
make_closure1029:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1029
	jmp endLabel1029

bodyOfLambda1029:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1029
; start of creating a closure of lambda-simple 8

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 72
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
copy_args_loop1030:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1030
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1030

done_copy_args1030:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1030:
	cmp r10, 7
	je done_copy_env1030
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1030
done_copy_env1030:

	mov qword [rbx + 8*r15], 0
make_closure1030:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1030
	jmp endLabel1030

bodyOfLambda1030:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1030
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 1*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure164
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure164
not_a_closure164:

	mov rax, sobVoid
done_closure164:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure163
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure163
not_a_closure163:

	mov rax, sobVoid
done_closure163:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure162
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure162
not_a_closure162:

	mov rax, sobVoid
done_closure162:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1030:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1030:
; end of creating a closure of lambda-simple 8

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1029:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1029:
; end of creating a closure of lambda-simple 7

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1028:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1028:
; end of creating a closure of lambda-simple 6

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure161
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure161
not_a_closure161:

	mov rax, sobVoid
done_closure161:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1025:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1025
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1025

done_copy_args1025:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1025:
	cmp r10, 5
	je done_copy_env1025
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1025
done_copy_env1025:

	mov qword [rbx + 8*r15], 0
make_closure1025:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1025
	jmp endLabel1025

bodyOfLambda1025:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1025
; start of creating a closure of lambda-simple 7

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 64
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
copy_args_loop1026:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1026
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1026

done_copy_args1026:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1026:
	cmp r10, 6
	je done_copy_env1026
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1026
done_copy_env1026:

	mov qword [rbx + 8*r15], 0
make_closure1026:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1026
	jmp endLabel1026

bodyOfLambda1026:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1026
; start of creating a closure of lambda-simple 8

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 72
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
copy_args_loop1027:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1027
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1027

done_copy_args1027:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1027:
	cmp r10, 7
	je done_copy_env1027
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1027
done_copy_env1027:

	mov qword [rbx + 8*r15], 0
make_closure1027:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1027
	jmp endLabel1027

bodyOfLambda1027:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1027
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 1*8]
	mov rax, qword [rax + 0*8]
	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure160
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure160
not_a_closure160:

	mov rax, sobVoid
done_closure160:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure159
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure159
not_a_closure159:

	mov rax, sobVoid
done_closure159:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1027:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1027:
; end of creating a closure of lambda-simple 8

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1026:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1026:
; end of creating a closure of lambda-simple 7

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1025:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1025:
; end of creating a closure of lambda-simple 6

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure158
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure158
not_a_closure158:

	mov rax, sobVoid
done_closure158:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure157
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure157
not_a_closure157:

	mov rax, sobVoid
done_closure157:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1024:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1024:
; end of creating a closure of lambda-simple 5

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure156
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure156
not_a_closure156:

	mov rax, sobVoid
done_closure156:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure155
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure155
not_a_closure155:

	mov rax, sobVoid
done_closure155:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1021:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1021
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1021

done_copy_args1021:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1021:
	cmp r10, 4
	je done_copy_env1021
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1021
done_copy_env1021:

	mov qword [rbx + 8*r15], 0
make_closure1021:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1021
	jmp endLabel1021

bodyOfLambda1021:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1021
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 6

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 56
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
copy_args_loop1022:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1022
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1022

done_copy_args1022:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1022:
	cmp r10, 5
	je done_copy_env1022
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1022
done_copy_env1022:

	mov qword [rbx + 8*r15], 0
make_closure1022:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1022
	jmp endLabel1022

bodyOfLambda1022:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1022
; start of creating a closure of lambda-simple 7

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 64
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
copy_args_loop1023:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1023
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1023

done_copy_args1023:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1023:
	cmp r10, 6
	je done_copy_env1023
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1023
done_copy_env1023:

	mov qword [rbx + 8*r15], 0
make_closure1023:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1023
	jmp endLabel1023

bodyOfLambda1023:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1023
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1023:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1023:
; end of creating a closure of lambda-simple 7

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1022:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1022:
; end of creating a closure of lambda-simple 6

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure154
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure154
not_a_closure154:

	mov rax, sobVoid
done_closure154:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1021:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1021:
; end of creating a closure of lambda-simple 5

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure153
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure153
not_a_closure153:

	mov rax, sobVoid
done_closure153:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1020:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1020:
; end of creating a closure of lambda-simple 4

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure152
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure152
not_a_closure152:

	mov rax, sobVoid
done_closure152:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure151
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure151
not_a_closure151:

	mov rax, sobVoid
done_closure151:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1019:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1019:
; end of creating a closure of lambda-simple 3

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1018:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1018:
; end of creating a closure of lambda-simple 2

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure150
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure150
not_a_closure150:

	mov rax, sobVoid
done_closure150:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure149
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure149
not_a_closure149:

	mov rax, sobVoid
done_closure149:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
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
copy_args_loop1012:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1012
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1012

done_copy_args1012:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1012:
	cmp r10, 1
	je done_copy_env1012
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1012
done_copy_env1012:

	mov qword [rbx + 8*r15], 0
make_closure1012:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1012
	jmp endLabel1012

bodyOfLambda1012:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1012
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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
copy_args_loop1016:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1016
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1016

done_copy_args1016:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1016:
	cmp r10, 2
	je done_copy_env1016
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1016
done_copy_env1016:

	mov qword [rbx + 8*r15], 0
make_closure1016:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1016
	jmp endLabel1016

bodyOfLambda1016:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1016
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1017:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1017
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1017

done_copy_args1017:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1017:
	cmp r10, 3
	je done_copy_env1017
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1017
done_copy_env1017:

	mov qword [rbx + 8*r15], 0
make_closure1017:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1017
	jmp endLabel1017

bodyOfLambda1017:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1017
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1017:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1017:
; end of creating a closure of lambda-simple 4

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1016:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1016:
; end of creating a closure of lambda-simple 3

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
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
copy_args_loop1013:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1013
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1013

done_copy_args1013:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1013:
	cmp r10, 2
	je done_copy_env1013
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1013
done_copy_env1013:

	mov qword [rbx + 8*r15], 0
make_closure1013:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1013
	jmp endLabel1013

bodyOfLambda1013:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1013
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1014:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1014
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1014

done_copy_args1014:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1014:
	cmp r10, 3
	je done_copy_env1014
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1014
done_copy_env1014:

	mov qword [rbx + 8*r15], 0
make_closure1014:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1014
	jmp endLabel1014

bodyOfLambda1014:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1014
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1015:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1015
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1015

done_copy_args1015:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1015:
	cmp r10, 4
	je done_copy_env1015
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1015
done_copy_env1015:

	mov qword [rbx + 8*r15], 0
make_closure1015:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1015
	jmp endLabel1015

bodyOfLambda1015:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1015
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1015:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1015:
; end of creating a closure of lambda-simple 5

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1014:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1014:
; end of creating a closure of lambda-simple 4

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1013:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1013:
; end of creating a closure of lambda-simple 3

	push rax

	push 1
	mov rax, qword [rbp + (4+0)*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure148
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure148
not_a_closure148:

	mov rax, sobVoid
done_closure148:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure147
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure147
not_a_closure147:

	mov rax, sobVoid
done_closure147:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1012:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1012:
; end of creating a closure of lambda-simple 2

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure146
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure146
not_a_closure146:

	mov rax, sobVoid
done_closure146:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	push rax

	push 1
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
copy_args_loop1008:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1008
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1008

done_copy_args1008:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1008:
	cmp r10, 1
	je done_copy_env1008
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1008
done_copy_env1008:

	mov qword [rbx + 8*r15], 0
make_closure1008:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1008
	jmp endLabel1008

bodyOfLambda1008:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1008
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
copy_args_loop1009:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1009
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1009

done_copy_args1009:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1009:
	cmp r10, 2
	je done_copy_env1009
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1009
done_copy_env1009:

	mov qword [rbx + 8*r15], 0
make_closure1009:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1009
	jmp endLabel1009

bodyOfLambda1009:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1009
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of creating a closure of lambda-simple 4

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 40
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
copy_args_loop1010:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1010
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1010

done_copy_args1010:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1010:
	cmp r10, 3
	je done_copy_env1010
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1010
done_copy_env1010:

	mov qword [rbx + 8*r15], 0
make_closure1010:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1010
	jmp endLabel1010

bodyOfLambda1010:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1010
; start of creating a closure of lambda-simple 5

	xor rax, rax
	mov rdi, qword [rbp + 3*8]
	call malloc
	mov rdx, rax; rdx hold a pointer to store the params
	push rdx
	xor rax, rax
	mov rdi, 48
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
copy_args_loop1011:
	cmp rcx, qword [rbp + 3*8] ; check if we are done copying the args in the lambda
	je done_copy_args1011
	mov r9, qword [rbp + 8*(4 + rcx)]
	mov [rdx + rcx*8], r9
	inc rcx
	jmp copy_args_loop1011

done_copy_args1011:
	mov [rbx], rdx
	mov r10, 0
	mov r15, 1

copy_env_loop1011:
	cmp r10, 4
	je done_copy_env1011
	mov r12, [rbp + 8*2]
	mov r12, [r12 + 8*r10]
	mov [rbx + 8*r15], r12
	inc r10
	inc r15
	jmp copy_env_loop1011
done_copy_env1011:

	mov qword [rbx + 8*r15], 0
make_closure1011:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda1011
	jmp endLabel1011

bodyOfLambda1011:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count1011
	mov rax, qword [rbp + (4+0)*8]
	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1011:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1011:
; end of creating a closure of lambda-simple 5

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1010:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1010:
; end of creating a closure of lambda-simple 4

	push rax

	push 1
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
	mov rax, qword [rbp + 2*8]
	mov rax, qword [rax + 0*8]
	mov rax, qword [rax + 0*8]
	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure145
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure145
not_a_closure145:

	mov rax, sobVoid
done_closure145:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure144
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure144
not_a_closure144:

	mov rax, sobVoid
done_closure144:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1009:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1009:
; end of creating a closure of lambda-simple 3

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1008:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1008:
; end of creating a closure of lambda-simple 2

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure143
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure143
not_a_closure143:

	mov rax, sobVoid
done_closure143:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure142
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure142
not_a_closure142:

	mov rax, sobVoid
done_closure142:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1007:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1007:
; end of creating a closure of lambda-simple 1

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count1006:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel1006:
; end of creating a closure of lambda-simple 0

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure141
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure141
not_a_closure141:

	mov rax, sobVoid
done_closure141:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure140
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure140
not_a_closure140:

	mov rax, sobVoid
done_closure140:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure139
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure139
not_a_closure139:

	mov rax, sobVoid
done_closure139:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov r10, [rax]
	mov rcx, r10
	TYPE rcx
	cmp rcx, T_CLOSURE
	jne not_a_closure138
	mov rbx, r10
	CLOSURE_ENV rbx
	push rbx
	CLOSURE_CODE r10
	call r10
	add rsp, 8*1
	jmp done_closure138
not_a_closure138:

	mov rax, sobVoid
done_closure138:

	pop r15
	shl r15, 3
	add rsp, r15

	add rsp, 8
; end of applic of lambda-simple code: 

	mov rax, [rax]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end
	mov rsp, rbp
	pop rbp
	ret

