./valid/expressions/equalsOverBool.wacc
calling the reference compiler on ./valid/expressions/equalsOverBool.wacc
-- Test: equalsOverBool.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating equality and boolean operator precedence

# Output:
# true
# false
#

# Program:

begin
  bool p = true ;
  bool q = true ;
  bool r = false ;
  bool s = true ;
  println p && q != r || s ;
  println (p && q) != (r || s)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
equalsOverBool.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12, r13, r14, r15}
7		sub rsp, 40
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov qword ptr [rsp + 16], r13
11		mov qword ptr [rsp + 24], r14
12		mov qword ptr [rsp + 32], r15
13		mov rbp, rsp
14		# Stack pointer unchanged, no stack allocated variables
15		mov rax, 1
16		mov r12, rax
17		mov rax, 1
18		mov r13, rax
19		mov rax, 0
20		mov r14, rax
21		mov rax, 1
22		mov r15, rax
23		# Stack pointer unchanged, no stack allocated arguments
24		cmp r12, 1
25		jne .L0
26		cmp r13, r14
27		setne al
28		movsx rax, al
29		push rax
30		pop rax
31		cmp rax, 1
32	.L0:
33		sete al
34		movsx rax, al
35		push rax
36		pop rax
37		cmp rax, 1
38		je .L1
39		cmp r15, 1
40	.L1:
41		sete al
42		movsx rax, al
43		push rax
44		pop rax
45		mov rax, rax
46		mov rdi, rax
47		# statement primitives do not return results (but will clobber r0/rax)
48		call _printb
49		call _println
50		# Stack pointer unchanged, no stack allocated arguments
51		cmp r12, 1
52		jne .L2
53		cmp r13, 1
54	.L2:
55		sete al
56		movsx rax, al
57		push rax
58		cmp r14, 1
59		je .L3
60		cmp r15, 1
61	.L3:
62		sete al
63		movsx rax, al
64		push rax
65		pop rbx
66		pop rax
67		cmp rax, rbx
68		setne al
69		movsx rax, al
70		push rax
71		pop rax
72		mov rax, rax
73		mov rdi, rax
74		# statement primitives do not return results (but will clobber r0/rax)
75		call _printb
76		call _println
77		# Stack pointer unchanged, no stack allocated variables
78		mov rax, 0
79		# pop {rbx, r12, r13, r14, r15}
80		mov rbx, qword ptr [rsp]
81		mov r12, qword ptr [rsp + 8]
82		mov r13, qword ptr [rsp + 16]
83		mov r14, qword ptr [rsp + 24]
84		mov r15, qword ptr [rsp + 32]
85		add rsp, 40
86		pop rbp
87		ret
88	
89	.section .rodata
90	# length of .L._printb_str0
91		.int 5
92	.L._printb_str0:
93		.asciz "false"
94	# length of .L._printb_str1
95		.int 4
96	.L._printb_str1:
97		.asciz "true"
98	# length of .L._printb_str2
99		.int 4
100	.L._printb_str2:
101		.asciz "%.*s"
102	.text
103	_printb:
104		push rbp
105		mov rbp, rsp
106		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
107		and rsp, -16
108		cmp dil, 0
109		jne .L_printb0
110		lea rdx, [rip + .L._printb_str0]
111		jmp .L_printb1
112	.L_printb0:
113		lea rdx, [rip + .L._printb_str1]
114	.L_printb1:
115		mov esi, dword ptr [rdx - 4]
116		lea rdi, [rip + .L._printb_str2]
117		# on x86, al represents the number of SIMD registers used as variadic arguments
118		mov al, 0
119		call printf@plt
120		mov rdi, 0
121		call fflush@plt
122		mov rsp, rbp
123		pop rbp
124		ret
125	
126	.section .rodata
127	# length of .L._println_str0
128		.int 0
129	.L._println_str0:
130		.asciz ""
131	.text
132	_println:
133		push rbp
134		mov rbp, rsp
135		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
136		and rsp, -16
137		lea rdi, [rip + .L._println_str0]
138		call puts@plt
139		mov rdi, 0
140		call fflush@plt
141		mov rsp, rbp
142		pop rbp
143		ret
===========================================================
-- Finished

