./valid/runtimeErr/divideByZero/divideByZero.wacc
calling the reference compiler on ./valid/runtimeErr/divideByZero/divideByZero.wacc
-- Test: divideByZero.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt divide by zero

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	int x = 10 ;
	int y = 0 ;
	print x / y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
divideByZero.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12, r13}
7		sub rsp, 24
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov qword ptr [rsp + 16], r13
11		mov rbp, rsp
12		# Stack pointer unchanged, no stack allocated variables
13		mov rax, 10
14		mov r12, rax
15		mov rax, 0
16		mov r13, rax
17		# Stack pointer unchanged, no stack allocated arguments
18		mov eax, r12d
19		cmp r13d, 0
20		je _errDivZero
21		# sign extend EAX into EDX
22		cdq
23		idiv r13d
24		mov eax, eax
25		mov eax, eax
26		movsx rax, eax
27		push rax
28		pop rax
29		mov rax, rax
30		mov rdi, rax
31		# statement primitives do not return results (but will clobber r0/rax)
32		call _printi
33		# Stack pointer unchanged, no stack allocated variables
34		mov rax, 0
35		# pop {rbx, r12, r13}
36		mov rbx, qword ptr [rsp]
37		mov r12, qword ptr [rsp + 8]
38		mov r13, qword ptr [rsp + 16]
39		add rsp, 24
40		pop rbp
41		ret
42	
43	.section .rodata
44	# length of .L._prints_str0
45		.int 4
46	.L._prints_str0:
47		.asciz "%.*s"
48	.text
49	_prints:
50		push rbp
51		mov rbp, rsp
52		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
53		and rsp, -16
54		mov rdx, rdi
55		mov esi, dword ptr [rdi - 4]
56		lea rdi, [rip + .L._prints_str0]
57		# on x86, al represents the number of SIMD registers used as variadic arguments
58		mov al, 0
59		call printf@plt
60		mov rdi, 0
61		call fflush@plt
62		mov rsp, rbp
63		pop rbp
64		ret
65	
66	.section .rodata
67	# length of .L._printi_str0
68		.int 2
69	.L._printi_str0:
70		.asciz "%d"
71	.text
72	_printi:
73		push rbp
74		mov rbp, rsp
75		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
76		and rsp, -16
77		mov esi, edi
78		lea rdi, [rip + .L._printi_str0]
79		# on x86, al represents the number of SIMD registers used as variadic arguments
80		mov al, 0
81		call printf@plt
82		mov rdi, 0
83		call fflush@plt
84		mov rsp, rbp
85		pop rbp
86		ret
87	
88	_exit:
89		push rbp
90		mov rbp, rsp
91		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
92		and rsp, -16
93		call exit@plt
94		mov rsp, rbp
95		pop rbp
96		ret
97	
98	.section .rodata
99	# length of .L._errDivZero_str0
100		.int 40
101	.L._errDivZero_str0:
102		.asciz "fatal error: division or modulo by zero\n"
103	.text
104	_errDivZero:
105		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
106		and rsp, -16
107		lea rdi, [rip + .L._errDivZero_str0]
108		call _prints
109		mov dil, -1
110		call exit@plt
===========================================================
-- Finished

