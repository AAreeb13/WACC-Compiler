./valid/expressions/andOverOrExpr.wacc
calling the reference compiler on ./valid/expressions/andOverOrExpr.wacc
-- Test: andOverOrExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating boolean operator precedence

# Output:
# true
# false
#

# Program:

begin
  bool a = false ;
  bool b = false ;
  bool c = true ;
  println a && b || c ;
  println a && (b || c)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
andOverOrExpr.s contents are:
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
14		mov rax, 0
15		mov r12, rax
16		mov rax, 0
17		mov r13, rax
18		mov rax, 1
19		mov r14, rax
20		# Stack pointer unchanged, no stack allocated arguments
21		cmp r12, 1
22		jne .L0
23		cmp r13, 1
24	.L0:
25		sete al
26		movsx rax, al
27		push rax
28		pop rax
29		cmp rax, 1
30		je .L1
31		cmp r14, 1
32	.L1:
33		sete al
34		movsx rax, al
35		push rax
36		pop rax
37		mov rax, rax
38		mov rdi, rax
39		# statement primitives do not return results (but will clobber r0/rax)
40		call _printb
41		call _println
42		# Stack pointer unchanged, no stack allocated arguments
43		cmp r12, 1
44		jne .L2
45		cmp r13, 1
46		je .L3
47		cmp r14, 1
48	.L3:
49		sete al
50		movsx rax, al
51		push rax
52		pop rax
53		cmp rax, 1
54	.L2:
55		sete al
56		movsx rax, al
57		push rax
58		pop rax
59		mov rax, rax
60		mov rdi, rax
61		# statement primitives do not return results (but will clobber r0/rax)
62		call _printb
63		call _println
64		# Stack pointer unchanged, no stack allocated variables
65		mov rax, 0
66		# pop {rbx, r12, r13, r14}
67		mov rbx, qword ptr [rsp]
68		mov r12, qword ptr [rsp + 8]
69		mov r13, qword ptr [rsp + 16]
70		mov r14, qword ptr [rsp + 24]
71		add rsp, 32
72		pop rbp
73		ret
74	
75	.section .rodata
76	# length of .L._printb_str0
77		.int 5
78	.L._printb_str0:
79		.asciz "false"
80	# length of .L._printb_str1
81		.int 4
82	.L._printb_str1:
83		.asciz "true"
84	# length of .L._printb_str2
85		.int 4
86	.L._printb_str2:
87		.asciz "%.*s"
88	.text
89	_printb:
90		push rbp
91		mov rbp, rsp
92		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
93		and rsp, -16
94		cmp dil, 0
95		jne .L_printb0
96		lea rdx, [rip + .L._printb_str0]
97		jmp .L_printb1
98	.L_printb0:
99		lea rdx, [rip + .L._printb_str1]
100	.L_printb1:
101		mov esi, dword ptr [rdx - 4]
102		lea rdi, [rip + .L._printb_str2]
103		# on x86, al represents the number of SIMD registers used as variadic arguments
104		mov al, 0
105		call printf@plt
106		mov rdi, 0
107		call fflush@plt
108		mov rsp, rbp
109		pop rbp
110		ret
111	
112	.section .rodata
113	# length of .L._println_str0
114		.int 0
115	.L._println_str0:
116		.asciz ""
117	.text
118	_println:
119		push rbp
120		mov rbp, rsp
121		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
122		and rsp, -16
123		lea rdi, [rip + .L._println_str0]
124		call puts@plt
125		mov rdi, 0
126		call fflush@plt
127		mov rsp, rbp
128		pop rbp
129		ret
===========================================================
-- Finished

