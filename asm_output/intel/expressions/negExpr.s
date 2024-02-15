./valid/expressions/negExpr.wacc
calling the reference compiler on ./valid/expressions/negExpr.wacc
-- Test: negExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating negation

# Output:
# -42
#

# Program:

begin
	int x = 42 ;
  println -x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
negExpr.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12}
7		sub rsp, 16
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov rbp, rsp
11		# Stack pointer unchanged, no stack allocated variables
12		mov rax, 42
13		mov r12, rax
14		# Stack pointer unchanged, no stack allocated arguments
15		mov eax, 0
16		sub eax, r12d
17		jo _errOverflow
18		movsx rax, eax
19		push rax
20		pop rax
21		mov rax, rax
22		mov rdi, rax
23		# statement primitives do not return results (but will clobber r0/rax)
24		call _printi
25		call _println
26		# Stack pointer unchanged, no stack allocated variables
27		mov rax, 0
28		# pop {rbx, r12}
29		mov rbx, qword ptr [rsp]
30		mov r12, qword ptr [rsp + 8]
31		add rsp, 16
32		pop rbp
33		ret
34	
35	.section .rodata
36	# length of .L._prints_str0
37		.int 4
38	.L._prints_str0:
39		.asciz "%.*s"
40	.text
41	_prints:
42		push rbp
43		mov rbp, rsp
44		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
45		and rsp, -16
46		mov rdx, rdi
47		mov esi, dword ptr [rdi - 4]
48		lea rdi, [rip + .L._prints_str0]
49		# on x86, al represents the number of SIMD registers used as variadic arguments
50		mov al, 0
51		call printf@plt
52		mov rdi, 0
53		call fflush@plt
54		mov rsp, rbp
55		pop rbp
56		ret
57	
58	.section .rodata
59	# length of .L._printi_str0
60		.int 2
61	.L._printi_str0:
62		.asciz "%d"
63	.text
64	_printi:
65		push rbp
66		mov rbp, rsp
67		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
68		and rsp, -16
69		mov esi, edi
70		lea rdi, [rip + .L._printi_str0]
71		# on x86, al represents the number of SIMD registers used as variadic arguments
72		mov al, 0
73		call printf@plt
74		mov rdi, 0
75		call fflush@plt
76		mov rsp, rbp
77		pop rbp
78		ret
79	
80	.section .rodata
81	# length of .L._println_str0
82		.int 0
83	.L._println_str0:
84		.asciz ""
85	.text
86	_println:
87		push rbp
88		mov rbp, rsp
89		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
90		and rsp, -16
91		lea rdi, [rip + .L._println_str0]
92		call puts@plt
93		mov rdi, 0
94		call fflush@plt
95		mov rsp, rbp
96		pop rbp
97		ret
98	
99	_exit:
100		push rbp
101		mov rbp, rsp
102		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
103		and rsp, -16
104		call exit@plt
105		mov rsp, rbp
106		pop rbp
107		ret
108	
109	.section .rodata
110	# length of .L._errOverflow_str0
111		.int 52
112	.L._errOverflow_str0:
113		.asciz "fatal error: integer overflow or underflow occurred\n"
114	.text
115	_errOverflow:
116		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
117		and rsp, -16
118		lea rdi, [rip + .L._errOverflow_str0]
119		call _prints
120		mov dil, -1
121		call exit@plt
===========================================================
-- Finished

