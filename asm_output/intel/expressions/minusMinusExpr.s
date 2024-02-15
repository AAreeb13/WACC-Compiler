./valid/expressions/minusMinusExpr.wacc
calling the reference compiler on ./valid/expressions/minusMinusExpr.wacc
-- Test: minusMinusExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# -- should be recognised as two separate symbols

# Output:
# 3
#

# Program:

begin
  println 1--2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
minusMinusExpr.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		push rbx
7		mov rbp, rsp
8		# Stack pointer unchanged, no stack allocated arguments
9		mov eax, 1
10		sub eax, -2
11		jo _errOverflow
12		movsx rax, eax
13		push rax
14		pop rax
15		mov rax, rax
16		mov rdi, rax
17		# statement primitives do not return results (but will clobber r0/rax)
18		call _printi
19		call _println
20		mov rax, 0
21		pop rbx
22		pop rbp
23		ret
24	
25	.section .rodata
26	# length of .L._prints_str0
27		.int 4
28	.L._prints_str0:
29		.asciz "%.*s"
30	.text
31	_prints:
32		push rbp
33		mov rbp, rsp
34		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
35		and rsp, -16
36		mov rdx, rdi
37		mov esi, dword ptr [rdi - 4]
38		lea rdi, [rip + .L._prints_str0]
39		# on x86, al represents the number of SIMD registers used as variadic arguments
40		mov al, 0
41		call printf@plt
42		mov rdi, 0
43		call fflush@plt
44		mov rsp, rbp
45		pop rbp
46		ret
47	
48	.section .rodata
49	# length of .L._printi_str0
50		.int 2
51	.L._printi_str0:
52		.asciz "%d"
53	.text
54	_printi:
55		push rbp
56		mov rbp, rsp
57		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
58		and rsp, -16
59		mov esi, edi
60		lea rdi, [rip + .L._printi_str0]
61		# on x86, al represents the number of SIMD registers used as variadic arguments
62		mov al, 0
63		call printf@plt
64		mov rdi, 0
65		call fflush@plt
66		mov rsp, rbp
67		pop rbp
68		ret
69	
70	.section .rodata
71	# length of .L._println_str0
72		.int 0
73	.L._println_str0:
74		.asciz ""
75	.text
76	_println:
77		push rbp
78		mov rbp, rsp
79		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
80		and rsp, -16
81		lea rdi, [rip + .L._println_str0]
82		call puts@plt
83		mov rdi, 0
84		call fflush@plt
85		mov rsp, rbp
86		pop rbp
87		ret
88	
89	_exit:
90		push rbp
91		mov rbp, rsp
92		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
93		and rsp, -16
94		call exit@plt
95		mov rsp, rbp
96		pop rbp
97		ret
98	
99	.section .rodata
100	# length of .L._errOverflow_str0
101		.int 52
102	.L._errOverflow_str0:
103		.asciz "fatal error: integer overflow or underflow occurred\n"
104	.text
105	_errOverflow:
106		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
107		and rsp, -16
108		lea rdi, [rip + .L._errOverflow_str0]
109		call _prints
110		mov dil, -1
111		call exit@plt
===========================================================
-- Finished

