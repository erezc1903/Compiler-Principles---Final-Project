;;;

%macro iabs 1
	cmp %1, 0
	jge %%cont
	neg %1
	%%cont:
%endmacro

gcd:
	push rbp
	mov rbp, rsp

	mov rdx, 0
	mov rax, [rbp + 8 + 1*8] ; first
	mov rbx, [rbp + 8 + 2*8] ; second
	iabs rax
	iabs rbx
	cmp rax, rbx
	jge .loop
	xchg rax, rbx
	
.loop:
	cmp rbx, 0
	je .done
	mov rdx, 0
	div rbx
	mov rax, rbx
	mov rbx, rdx
	jmp .loop

.done:
	pop rbp
	ret
