./valid/expressions/multExpr.wacc
calling the reference compiler on ./valid/expressions/multExpr.wacc
-- Test: multExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating multiplication

# Output:
# 15
#

# Program:

begin
  int x = 5 ;
  int y = 3 ;
  println x * y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
multExpr.s contents are:
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
13		mov rax, 5
14		mov r12, rax
15		mov rax, 3
16		mov r13, rax
17		# Stack pointer unchanged, no stack allocated arguments
18		mov eax, r12d
19		imul eax, r13d
20		jo _errOverflow
21		movsx rax, eax
22		push rax
23		pop rax
24		mov rax, rax
25		mov rdi, rax
26		# statement primitives do not return results (but will clobber r0/rax)
27		call _printi
28		call _println
29		# Stack pointer unchanged, no stack allocated variables
30		mov rax, 0
31		# pop {rbx, r12, r13}
32		mov rbx, qword ptr [rsp]
33		mov r12, qword ptr [rsp + 8]
34		mov r13, qword ptr [rsp + 16]
35		add rsp, 24
36		pop rbp
37		ret
38	
39	.section .rodata
40	# length of .L._prints_str0
41		.int 4
42	.L._prints_str0:
43		.asciz "%.*s"
44	.text
45	_prints:
46		push rbp
47		mov rbp, rsp
48		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
49		and rsp, -16
50		mov rdx, rdi
51		mov esi, dword ptr [rdi - 4]
52		lea rdi, [rip + .L._prints_str0]
53		# on x86, al represents the number of SIMD registers used as variadic arguments
54		mov al, 0
55		call printf@plt
56		mov rdi, 0
57		call fflush@plt
58		mov rsp, rbp
59		pop rbp
60		ret
61	
62	.section .rodata
63	# length of .L._printi_str0
64		.int 2
65	.L._printi_str0:
66		.asciz "%d"
67	.text
68	_printi:
69		push rbp
70		mov rbp, rsp
71		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
72		and rsp, -16
73		mov esi, edi
74		lea rdi, [rip + .L._printi_str0]
75		# on x86, al represents the number of SIMD registers used as variadic arguments
76		mov al, 0
77		call printf@plt
78		mov rdi, 0
79		call fflush@plt
80		mov rsp, rbp
81		pop rbp
82		ret
83	
84	.section .rodata
85	# length of .L._println_str0
86		.int 0
87	.L._println_str0:
88		.asciz ""
89	.text
90	_println:
91		push rbp
92		mov rbp, rsp
93		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
94		and rsp, -16
95		lea rdi, [rip + .L._println_str0]
96		call puts@plt
97		mov rdi, 0
98		call fflush@plt
99		mov rsp, rbp
100		pop rbp
101		ret
102	
103	_exit:
104		push rbp
105		mov rbp, rsp
106		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
107		and rsp, -16
108		call exit@plt
109		mov rsp, rbp
110		pop rbp
111		ret
112	
113	.section .rodata
114	# length of .L._errOverflow_str0
115		.int 52
116	.L._errOverflow_str0:
117		.asciz "fatal error: integer overflow or underflow occurred\n"
118	.text
119	_errOverflow:
120		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
121		and rsp, -16
122		lea rdi, [rip + .L._errOverflow_str0]
123		call _prints
124		mov dil, -1
125		call exit@plt
===========================================================
-- Finished

