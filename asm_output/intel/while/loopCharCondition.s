./valid/while/loopCharCondition.wacc
calling the reference compiler on ./valid/while/loopCharCondition.wacc
-- Test: loopCharCondition.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Use a character as a loop condition. Enter the loop once only, then exit the loop.

# Output:
# Change c
# Should print "Change c" once before.
#

# Program:

begin
	char c = '\0' ;
	while c == '\0' do
		c = 'a' ;
		println "Change c"
	done ;
	println "Should print \"Change c\" once before."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
loopCharCondition.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 8
5	.L.str0:
6		.asciz "Change c"
7	# length of .L.str1
8		.int 36
9	.L.str1:
10		.asciz "Should print \"Change c\" once before."
11	.text
12	main:
13		push rbp
14		# push {rbx, r12}
15		sub rsp, 16
16		mov qword ptr [rsp], rbx
17		mov qword ptr [rsp + 8], r12
18		mov rbp, rsp
19		# Stack pointer unchanged, no stack allocated variables
20		mov rax, 0
21		mov r12, rax
22		jmp .L0
23	.L1:
24		mov rax, 97
25		mov r12, rax
26		# Stack pointer unchanged, no stack allocated arguments
27		lea rax, [rip + .L.str0]
28		push rax
29		pop rax
30		mov rax, rax
31		mov rdi, rax
32		# statement primitives do not return results (but will clobber r0/rax)
33		call _prints
34		call _println
35	.L0:
36		cmp r12, 0
37		je .L1
38		# Stack pointer unchanged, no stack allocated arguments
39		lea rax, [rip + .L.str1]
40		push rax
41		pop rax
42		mov rax, rax
43		mov rdi, rax
44		# statement primitives do not return results (but will clobber r0/rax)
45		call _prints
46		call _println
47		# Stack pointer unchanged, no stack allocated variables
48		mov rax, 0
49		# pop {rbx, r12}
50		mov rbx, qword ptr [rsp]
51		mov r12, qword ptr [rsp + 8]
52		add rsp, 16
53		pop rbp
54		ret
55	
56	.section .rodata
57	# length of .L._prints_str0
58		.int 4
59	.L._prints_str0:
60		.asciz "%.*s"
61	.text
62	_prints:
63		push rbp
64		mov rbp, rsp
65		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
66		and rsp, -16
67		mov rdx, rdi
68		mov esi, dword ptr [rdi - 4]
69		lea rdi, [rip + .L._prints_str0]
70		# on x86, al represents the number of SIMD registers used as variadic arguments
71		mov al, 0
72		call printf@plt
73		mov rdi, 0
74		call fflush@plt
75		mov rsp, rbp
76		pop rbp
77		ret
78	
79	.section .rodata
80	# length of .L._println_str0
81		.int 0
82	.L._println_str0:
83		.asciz ""
84	.text
85	_println:
86		push rbp
87		mov rbp, rsp
88		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
89		and rsp, -16
90		lea rdi, [rip + .L._println_str0]
91		call puts@plt
92		mov rdi, 0
93		call fflush@plt
94		mov rsp, rbp
95		pop rbp
96		ret
===========================================================
-- Finished

