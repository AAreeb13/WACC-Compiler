./valid/expressions/andExpr.wacc
calling the reference compiler on ./valid/expressions/andExpr.wacc
-- Test: andExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating and

# Output:
# false
# true
# false
#

# Program:

begin
  bool a = true ;
  bool b = false ;
  println a && b ;
  println a && true ;
  println b && false
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
andExpr.s contents are:
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
13		mov rax, 1
14		mov r12, rax
15		mov rax, 0
16		mov r13, rax
17		# Stack pointer unchanged, no stack allocated arguments
18		cmp r12, 1
19		jne .L0
20		cmp r13, 1
21	.L0:
22		sete al
23		movsx rax, al
24		push rax
25		pop rax
26		mov rax, rax
27		mov rdi, rax
28		# statement primitives do not return results (but will clobber r0/rax)
29		call _printb
30		call _println
31		# Stack pointer unchanged, no stack allocated arguments
32		cmp r12, 1
33		jne .L1
34		mov rax, 1
35		cmp rax, 1
36	.L1:
37		sete al
38		movsx rax, al
39		push rax
40		pop rax
41		mov rax, rax
42		mov rdi, rax
43		# statement primitives do not return results (but will clobber r0/rax)
44		call _printb
45		call _println
46		# Stack pointer unchanged, no stack allocated arguments
47		cmp r13, 1
48		jne .L2
49		mov rax, 0
50		cmp rax, 1
51	.L2:
52		sete al
53		movsx rax, al
54		push rax
55		pop rax
56		mov rax, rax
57		mov rdi, rax
58		# statement primitives do not return results (but will clobber r0/rax)
59		call _printb
60		call _println
61		# Stack pointer unchanged, no stack allocated variables
62		mov rax, 0
63		# pop {rbx, r12, r13}
64		mov rbx, qword ptr [rsp]
65		mov r12, qword ptr [rsp + 8]
66		mov r13, qword ptr [rsp + 16]
67		add rsp, 24
68		pop rbp
69		ret
70	
71	.section .rodata
72	# length of .L._printb_str0
73		.int 5
74	.L._printb_str0:
75		.asciz "false"
76	# length of .L._printb_str1
77		.int 4
78	.L._printb_str1:
79		.asciz "true"
80	# length of .L._printb_str2
81		.int 4
82	.L._printb_str2:
83		.asciz "%.*s"
84	.text
85	_printb:
86		push rbp
87		mov rbp, rsp
88		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
89		and rsp, -16
90		cmp dil, 0
91		jne .L_printb0
92		lea rdx, [rip + .L._printb_str0]
93		jmp .L_printb1
94	.L_printb0:
95		lea rdx, [rip + .L._printb_str1]
96	.L_printb1:
97		mov esi, dword ptr [rdx - 4]
98		lea rdi, [rip + .L._printb_str2]
99		# on x86, al represents the number of SIMD registers used as variadic arguments
100		mov al, 0
101		call printf@plt
102		mov rdi, 0
103		call fflush@plt
104		mov rsp, rbp
105		pop rbp
106		ret
107	
108	.section .rodata
109	# length of .L._println_str0
110		.int 0
111	.L._println_str0:
112		.asciz ""
113	.text
114	_println:
115		push rbp
116		mov rbp, rsp
117		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
118		and rsp, -16
119		lea rdi, [rip + .L._println_str0]
120		call puts@plt
121		mov rdi, 0
122		call fflush@plt
123		mov rsp, rbp
124		pop rbp
125		ret
===========================================================
-- Finished

