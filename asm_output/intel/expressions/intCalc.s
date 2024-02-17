./valid/expressions/intCalc.wacc
calling the reference compiler on ./valid/expressions/intCalc.wacc
-- Test: intCalc.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple integer calculation

# Output:
# 72
#

# Program:

begin
  int x = 42 ;
  int y = 30 ;
  int z = x + y ;
  println z
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intCalc.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12, r13, r14}
7		sub rsp, 32
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov qword ptr [rsp + 16], r13
11		mov qword ptr [rsp + 24], r14
12		mov rbp, rsp
13		# Stack pointer unchanged, no stack allocated variables
14		mov rax, 42
15		mov r12, rax
16		mov rax, 30
17		mov r13, rax
18		mov eax, r12d
19		add eax, r13d
20		jo _errOverflow
21		movsx rax, eax
22		push rax
23		pop rax
24		mov rax, rax
25		mov r14, rax
26		# Stack pointer unchanged, no stack allocated arguments
27		mov rax, r14
28		mov rdi, rax
29		# statement primitives do not return results (but will clobber r0/rax)
30		call _printi
31		call _println
32		# Stack pointer unchanged, no stack allocated variables
33		mov rax, 0
34		# pop {rbx, r12, r13, r14}
35		mov rbx, qword ptr [rsp]
36		mov r12, qword ptr [rsp + 8]
37		mov r13, qword ptr [rsp + 16]
38		mov r14, qword ptr [rsp + 24]
39		add rsp, 32
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
88	.section .rodata
89	# length of .L._println_str0
90		.int 0
91	.L._println_str0:
92		.asciz ""
93	.text
94	_println:
95		push rbp
96		mov rbp, rsp
97		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
98		and rsp, -16
99		lea rdi, [rip + .L._println_str0]
100		call puts@plt
101		mov rdi, 0
102		call fflush@plt
103		mov rsp, rbp
104		pop rbp
105		ret
106	
107	_exit:
108		push rbp
109		mov rbp, rsp
110		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
111		and rsp, -16
112		call exit@plt
113		mov rsp, rbp
114		pop rbp
115		ret
116	
117	.section .rodata
118	# length of .L._errOverflow_str0
119		.int 52
120	.L._errOverflow_str0:
121		.asciz "fatal error: integer overflow or underflow occurred\n"
122	.text
123	_errOverflow:
124		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
125		and rsp, -16
126		lea rdi, [rip + .L._errOverflow_str0]
127		call _prints
128		mov dil, -1
129		call exit@plt
===========================================================
-- Finished

