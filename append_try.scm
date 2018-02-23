;=========================================================================================================================================
;======================================================= FUNCTIONS FOR APPEND EXPRESSION =================================================
;=========================================================================================================================================


(define handle_append
		(lambda ()
			(string-append (applic-prolog "append_code" "end_append_code")

				"\nappend_code:\n"
				"\tpush rbp\n"
				"\tmov rbp, rsp\n"
				
				"\tmov rcx, 0 ; rcx is a counter for the number of arguments\n"
				".checkIfArgsArePairs:\n\n"
				"\tcmp rcx, qword [rbp + 8*3]\n"
				"\tje .create_new_list\n"
				"\tmov rbx, qword [rbp + 8*(4 + rcx)]\n"
				"\tmov rbx, [rbx]\n"
				"\tTYPE rbx\n"
				"\tcmp rbx, T_PAIR\n"
				"\tjne .badArgs\n"
				"\tinc rcx\n"
				"\tjmp .checkIfArgsArePairs\n"


			    ".create_new_list:\n\n"
			    "\tmov r8, 1\n"
			    "\tmov r9, qword [rbp + 4*8] ; r9 hold a pointer to the first pair\n"
			    "\tmov r10, r9 ; r10 hold a backup pointer to the first pair\n"
			    "\tmov r9, [r9]\n"
			    
			    ".find_end_of_first_pair:\n\n"
			    "\tcmp r9, sobNil\n"
			    "\tje .continue_to_the_rest_of_pairs\n"
			    ;"\tmov r9, [r9] ; r9 holds the first pair\n"
			    "\tTYPE r9 ; r9 hold to the cdr of first pair\n"
			    "\tcmp r9, T_PAIR\n"
			    "\tjne .continue_to_the_rest_of_pairs\n"
			    "\tmov r9, r10\n"
			    "\tmov r9, [r9]\n"
			    "\tDATA_LOWER r9\n"
			    "\tjmp .find_end_of_first_pair\n"


			    ".continue_to_the_rest_of_pairs:\n\n"
			    "\tcmp r8, qword [rbp + 8*3]\n"
			    "\tje .close_the_new_list\n"
			    "\tinc r8\n"

			    "\tmov r13, r11 ; r13 holds a pointer to the cdr of the current pair which is a pair\n"
				"\tmov r13, [r13] ; r13 holds the cdr of the current pair which is a pair\n"
				"\tmov r12, qword [rbp + 5*8 + r8*8] ; r12 holds a pointer to the next pair which will be the cdr of the last place in the current pair \n"
				"\tsub r12, start_of_data\n"

				"\tshr r13, 34\n"
				"\tshl r13, 34\n"
				"\tor r13, T_PAIR\n"
				"\tshl r12, 4\n" 
				"\tor r13,r12 ; the next pair is now appended to the current pair\n" 

				".find_end_of_current_pair:\n\n"
				"\tcmp r12, sobNil\n"
			    "\tje .continue_to_the_rest_of_pairs\n"
			    "\tmov r12, [r12] ; r12 holds the current pair\n"
			    "\tDATA_LOWER r12 ; r12 hold to the cdr of current pair\n"
			    "\tmov r11, r12\n"
			    "\tjmp .find_end_of_current_pair\n"

				".close_the_new_list:\n\n"
				"\tcmp r8, 1\n"
				"\tje .retNil\n"
				"\tmov rax, qword [rbp + 4*8]\n"
				"\tjmp .done\n"

				".retNil:\n\n"
				"\tmov rax, sobNil\n"
				"\tjmp .done\n"

				".badArgs:\n"
				"\tmov rax, sobVoid\n"

				".done:\n"
				"\tmov rsp, rbp\n" 
				"\tpop rbp\n"
				"\tret\n\n"

				"end_append_code:\n"
				"\tmov rax, [rax]\n"
				"\tmov qword [append], rax\n\n")))


;=========================================================================================================================================
;======================================================= END OF FUNCTIONS FOR APPEND EXPRESSION ==========================================
;=========================================================================================================================================