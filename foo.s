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

factorial:
	dq SOB_UNDEFINED

fibonacci:
	dq SOB_UNDEFINED

equal?:
	dq SOB_UNDEFINED

sobVoid:
	dq SOB_VOID

sobFalse:
	dq SOB_FALSE
sobTrue:
	dq SOB_TRUE

sobNil:
	dq SOB_NIL

sobString6:
	MAKE_LITERAL_STRING "eq!"

sobString7:
	MAKE_LITERAL_STRING "no!"

sobInt0:
	dq MAKE_LITERAL (T_INTEGER, 0)

sobInt1:
	dq MAKE_LITERAL (T_INTEGER, 1)

sobInt6:
	dq MAKE_LITERAL (T_INTEGER, 6)

sobInt2:
	dq MAKE_LITERAL (T_INTEGER, 2)

sobInt11:
	dq MAKE_LITERAL (T_INTEGER, 11)

sobInt3:
	dq MAKE_LITERAL (T_INTEGER, 3)

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
	mov rbx, r10
	cmp rbx, sobFalse
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

	mov rdi, 8
	call malloc
	mov r8, sobString6
	shl r8, 4
	or r8, T_SYMBOL
	mov qword [rax], r8
	push rax
	push SymbolTable
	call addSymbol
	add rsp, 2*8
	mov rdi, 8
	call malloc
	mov r8, sobString7
	shl r8, 4
	or r8, T_SYMBOL
	mov qword [rax], r8
	push rax
	push SymbolTable
	call addSymbol
	add rsp, 2*8

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

	mov rax, sobNil
	push rax
	mov rax, subtract
	push rax
	mov rax, plus
	push rax

	push 2
; start of creating a closure of lambda-simple 0

	mov rbx, 0
	mov rdi, 16
	call malloc; rax now hold a pointer to the target closure
make_closure108:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda108
	jmp endLabel108

bodyOfLambda108:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 2
	jne bad_arg_count108
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+1)*8]
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 2
	mov rax, eq?
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

	mov r10, [rax]
	cmp r10, SOB_FALSE
	je L4
	mov rax, sobString6
	push rax
	push SymbolTable
	call findSymbol
	add rsp, 2*8

	;code gen for symbol end

	jmp Lend5
L4:
		mov rax, sobString7
	push rax
	push SymbolTable
	call findSymbol
	add rsp, 2*8

	;code gen for symbol end

Lend5:

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count108:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel108:
; end of creating a closure of lambda-simple 0

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
make_closure109:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda109
	jmp endLabel109

bodyOfLambda109:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count109
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	; codegen for const start
	mov rax, sobInt0
	;code gen for constant end
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 2
	mov rax, equal
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

	mov r10, [rax]
	cmp r10, SOB_FALSE
	je L5
	; codegen for const start
	mov rax, sobInt1
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
	; codegen for const start
	mov rax, sobInt1
	;code gen for constant end
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 2
	mov rax, subtract
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

	push 1
	mov rax, factorial
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
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 2
	mov rax, multiply
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


Lend6:

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count109:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel109:
; end of creating a closure of lambda-simple 0

	mov rbx, factorial
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
	mov rax, sobInt6
	;code gen for constant end
	push rax

	push 1
	mov rax, factorial
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
make_closure110:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda110
	jmp endLabel110

bodyOfLambda110:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 1
	jne bad_arg_count110
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	; codegen for const start
	mov rax, sobInt2
	;code gen for constant end
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 2
	mov rax, lessThan
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

	mov r10, [rax]
	cmp r10, SOB_FALSE
	je L6
	; codegen for const start
	mov rax, sobInt1
	;code gen for constant end

	jmp Lend7
L6:
	; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	; codegen for const start
	mov rax, sobInt2
	;code gen for constant end
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 2
	mov rax, subtract
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

	push rax

	push 1
	mov rax, fibonacci
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

	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	; codegen for const start
	mov rax, sobInt1
	;code gen for constant end
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 2
	mov rax, subtract
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

	push rax

	push 1
	mov rax, fibonacci
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

	push rax

	push 2
	mov rax, plus
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


Lend7:

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count110:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel110:
; end of creating a closure of lambda-simple 0

	mov rbx, fibonacci
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
	mov rax, sobInt11
	;code gen for constant end
	push rax

	push 1
	mov rax, fibonacci
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
make_closure111:

	MAKE_LITERAL_CLOSURE rax, rbx, bodyOfLambda111
	jmp endLabel111

bodyOfLambda111:
	push rbp
	mov rbp, rsp
	mov r10, qword [rbp +3*8]
	cmp r10, 2
	jne bad_arg_count111
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 1
	mov rax, pair?
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

	push rax

	push 1
	mov rax, not
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

	mov r10, [rax]
	cmp r10, SOB_FALSE
	je L7
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	mov rax, qword [rbp + (4+1)*8]
	push rax
	mov rax, qword [rbp + (4+0)*8]
	push rax

	push 2
	mov rax, eq?
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


	jmp Lend8
L7:
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

	push rax

	push 2
	mov rax, equal?
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
	cmp r10, SOB_FALSE
	je L8
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

	push rax

	push 2
	mov rax, equal?
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


	jmp Lend9
L8:
		; codegen for const start
	mov rax, sobFalse
	;code gen for constant end

Lend9:


Lend8:

	mov rsp, rbp
	pop rbp
	ret

bad_arg_count111:
	mov rax, sobVoid
	mov rsp, rbp
	pop rbp
	ret

endLabel111:
; end of creating a closure of lambda-simple 0

	mov rbx, equal?
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
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	; codegen for const start
	mov rax, sobInt3
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, sobInt1
	;code gen for constant end
	push rax

	push 2
	mov rax, cons
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

	push rax
; start of applic of lambda-simple code: 

	mov rax, sobNil
	push rax
	; codegen for const start
	mov rax, sobInt2
	;code gen for constant end
	push rax
	; codegen for const start
	mov rax, sobInt1
	;code gen for constant end
	push rax

	push 2
	mov rax, cons
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

	push rax

	push 2
	mov rax, equal?
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

	mov rax, [rax]
	push rax
	call write_sob_if_not_void
	add rsp, 8

; end
	mov rsp, rbp
	pop rbp
	ret

