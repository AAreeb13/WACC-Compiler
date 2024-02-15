./valid/expressions/negDividendMod.wacc
calling the reference compiler on ./valid/expressions/negDividendMod.wacc
-- Test: negDividendMod.wacc

-- Uploaded file: 
---------------------------------------------------------------
# modulus of a negative number

# Output:
# -2
#

# Program:

begin
  int x = -5 ;
  int y = 3 ;
  println x % y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
negDividendMod.s contents are:
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
13		mov rax, -5
14		mov r12, rax
15		mov rax, 3
16		mov r13, rax
17		# Stack pointer unchanged, no stack allocated arguments
18		mov eax, r12d
19		cmp r13d, 0
20		je _errDivZero
21		# sign extend EAX into EDX
22		cdq
23		idiv r13d
24		mov eax, edx
25		mov eax, eax
26		movsx rax, eax
27		push rax
28		pop rax
29		mov rax, rax
30		mov rdi, rax
31		# statement primitives do not return results (but will clobber r0/rax)
32		call _printi
33		call _println
34		# Stack pointer unchanged, no stack allocated variables
35		mov rax, 0
36		# pop {rbx, r12, r13}
37		mov rbx, qword ptr [rsp]
38		mov r12, qword ptr [rsp + 8]
39		mov r13, qword ptr [rsp + 16]
40		add rsp, 24
41		pop rbp
42		ret
43	
44	.section .rodata
45	# length of .L._prints_str0
46		.int 4
47	.L._prints_str0:
48		.asciz "%.*s"
49	.text
50	_prints:
51		push rbp
52		mov rbp, rsp
53		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
54		and rsp, -16
55		mov rdx, rdi
56		mov esi, dword ptr [rdi - 4]
57		lea rdi, [rip + .L._prints_str0]
58		# on x86, al represents the number of SIMD registers used as variadic arguments
59		mov al, 0
60		call printf@plt
61		mov rdi, 0
62		call fflush@plt
63		mov rsp, rbp
64		pop rbp
65		ret
66	
67	.section .rodata
68	# length of .L._printi_str0
69		.int 2
70	.L._printi_str0:
71		.asciz "%d"
72	.text
73	_printi:
74		push rbp
75		mov rbp, rsp
76		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
77		and rsp, -16
78		mov esi, edi
79		lea rdi, [rip + .L._printi_str0]
80		# on x86, al represents the number of SIMD registers used as variadic arguments
81		mov al, 0
82		call printf@plt
83		mov rdi, 0
84		call fflush@plt
85		mov rsp, rbp
86		pop rbp
87		ret
88	
89	.section .rodata
90	# length of .L._println_str0
91		.int 0
92	.L._println_str0:
93		.asciz ""
94	.text
95	_println:
96		push rbp
97		mov rbp, rsp
98		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
99		and rsp, -16
100		lea rdi, [rip + .L._println_str0]
101		call puts@plt
102		mov rdi, 0
103		call fflush@plt
104		mov rsp, rbp
105		pop rbp
106		ret
107	
108	_exit:
109		push rbp
110		mov rbp, rsp
111		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
112		and rsp, -16
113		call exit@plt
114		mov rsp, rbp
115		pop rbp
116		ret
117	
118	.section .rodata
119	# length of .L._errDivZero_str0
120		.int 40
121	.L._errDivZero_str0:
122		.asciz "fatal error: division or modulo by zero\n"
123	.text
124	_errDivZero:
125		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
126		and rsp, -16
127		lea rdi, [rip + .L._errDivZero_str0]
128		call _prints
129		mov dil, -1
130		call exit@plt
===========================================================
-- Finished

