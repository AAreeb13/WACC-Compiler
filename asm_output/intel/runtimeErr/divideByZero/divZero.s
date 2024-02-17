./valid/runtimeErr/divideByZero/divZero.wacc
calling the reference compiler on ./valid/runtimeErr/divideByZero/divZero.wacc
-- Test: divZero.wacc

-- Uploaded file: 
---------------------------------------------------------------
# division by zero

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = 10 / 0 ;
  println "should not reach here"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
divZero.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 21
5	.L.str0:
6		.asciz "should not reach here"
7	.text
8	main:
9		push rbp
10		# push {rbx, r12}
11		sub rsp, 16
12		mov qword ptr [rsp], rbx
13		mov qword ptr [rsp + 8], r12
14		mov rbp, rsp
15		# Stack pointer unchanged, no stack allocated variables
16		mov rbx, 0
17		mov eax, 10
18		cmp ebx, 0
19		je _errDivZero
20		# sign extend EAX into EDX
21		cdq
22		idiv ebx
23		mov eax, eax
24		mov eax, eax
25		movsx rax, eax
26		push rax
27		pop rax
28		mov rax, rax
29		mov r12, rax
30		# Stack pointer unchanged, no stack allocated arguments
31		lea rax, [rip + .L.str0]
32		push rax
33		pop rax
34		mov rax, rax
35		mov rdi, rax
36		# statement primitives do not return results (but will clobber r0/rax)
37		call _prints
38		call _println
39		# Stack pointer unchanged, no stack allocated variables
40		mov rax, 0
41		# pop {rbx, r12}
42		mov rbx, qword ptr [rsp]
43		mov r12, qword ptr [rsp + 8]
44		add rsp, 16
45		pop rbp
46		ret
47	
48	.section .rodata
49	# length of .L._prints_str0
50		.int 4
51	.L._prints_str0:
52		.asciz "%.*s"
53	.text
54	_prints:
55		push rbp
56		mov rbp, rsp
57		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
58		and rsp, -16
59		mov rdx, rdi
60		mov esi, dword ptr [rdi - 4]
61		lea rdi, [rip + .L._prints_str0]
62		# on x86, al represents the number of SIMD registers used as variadic arguments
63		mov al, 0
64		call printf@plt
65		mov rdi, 0
66		call fflush@plt
67		mov rsp, rbp
68		pop rbp
69		ret
70	
71	.section .rodata
72	# length of .L._println_str0
73		.int 0
74	.L._println_str0:
75		.asciz ""
76	.text
77	_println:
78		push rbp
79		mov rbp, rsp
80		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
81		and rsp, -16
82		lea rdi, [rip + .L._println_str0]
83		call puts@plt
84		mov rdi, 0
85		call fflush@plt
86		mov rsp, rbp
87		pop rbp
88		ret
89	
90	_exit:
91		push rbp
92		mov rbp, rsp
93		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
94		and rsp, -16
95		call exit@plt
96		mov rsp, rbp
97		pop rbp
98		ret
99	
100	.section .rodata
101	# length of .L._errDivZero_str0
102		.int 40
103	.L._errDivZero_str0:
104		.asciz "fatal error: division or modulo by zero\n"
105	.text
106	_errDivZero:
107		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
108		and rsp, -16
109		lea rdi, [rip + .L._errDivZero_str0]
110		call _prints
111		mov dil, -1
112		call exit@plt
===========================================================
-- Finished

