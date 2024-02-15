./valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc
-- Test: intnegateOverflow3.wacc

-- Uploaded file: 
---------------------------------------------------------------
# multiplying a negative integer

# Output:
# -20000
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = -20000 ;
  println x ;
  x = x*100000000; #err here? 
  println x 
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intnegateOverflow3.s contents are:
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
12		mov rax, -20000
13		mov r12, rax
14		# Stack pointer unchanged, no stack allocated arguments
15		mov rax, r12
16		mov rdi, rax
17		# statement primitives do not return results (but will clobber r0/rax)
18		call _printi
19		call _println
20		imul eax, r12d, 100000000
21		jo _errOverflow
22		movsx rax, eax
23		push rax
24		pop rax
25		mov rax, rax
26		mov r12, rax
27		# Stack pointer unchanged, no stack allocated arguments
28		mov rax, r12
29		mov rdi, rax
30		# statement primitives do not return results (but will clobber r0/rax)
31		call _printi
32		call _println
33		# Stack pointer unchanged, no stack allocated variables
34		mov rax, 0
35		# pop {rbx, r12}
36		mov rbx, qword ptr [rsp]
37		mov r12, qword ptr [rsp + 8]
38		add rsp, 16
39		pop rbp
40		ret
41	
42	.section .rodata
43	# length of .L._prints_str0
44		.int 4
45	.L._prints_str0:
46		.asciz "%.*s"
47	.text
48	_prints:
49		push rbp
50		mov rbp, rsp
51		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
52		and rsp, -16
53		mov rdx, rdi
54		mov esi, dword ptr [rdi - 4]
55		lea rdi, [rip + .L._prints_str0]
56		# on x86, al represents the number of SIMD registers used as variadic arguments
57		mov al, 0
58		call printf@plt
59		mov rdi, 0
60		call fflush@plt
61		mov rsp, rbp
62		pop rbp
63		ret
64	
65	.section .rodata
66	# length of .L._printi_str0
67		.int 2
68	.L._printi_str0:
69		.asciz "%d"
70	.text
71	_printi:
72		push rbp
73		mov rbp, rsp
74		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
75		and rsp, -16
76		mov esi, edi
77		lea rdi, [rip + .L._printi_str0]
78		# on x86, al represents the number of SIMD registers used as variadic arguments
79		mov al, 0
80		call printf@plt
81		mov rdi, 0
82		call fflush@plt
83		mov rsp, rbp
84		pop rbp
85		ret
86	
87	.section .rodata
88	# length of .L._println_str0
89		.int 0
90	.L._println_str0:
91		.asciz ""
92	.text
93	_println:
94		push rbp
95		mov rbp, rsp
96		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
97		and rsp, -16
98		lea rdi, [rip + .L._println_str0]
99		call puts@plt
100		mov rdi, 0
101		call fflush@plt
102		mov rsp, rbp
103		pop rbp
104		ret
105	
106	_exit:
107		push rbp
108		mov rbp, rsp
109		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
110		and rsp, -16
111		call exit@plt
112		mov rsp, rbp
113		pop rbp
114		ret
115	
116	.section .rodata
117	# length of .L._errOverflow_str0
118		.int 52
119	.L._errOverflow_str0:
120		.asciz "fatal error: integer overflow or underflow occurred\n"
121	.text
122	_errOverflow:
123		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
124		and rsp, -16
125		lea rdi, [rip + .L._errOverflow_str0]
126		call _prints
127		mov dil, -1
128		call exit@plt
===========================================================
-- Finished

