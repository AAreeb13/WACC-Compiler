./valid/expressions/equalsOverOr.wacc
calling the reference compiler on ./valid/expressions/equalsOverOr.wacc
-- Test: equalsOverOr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating equality and boolean operator precedence

# Output:
# true
# false
#

# Program:

begin
  bool p = false ;
  bool q = true ;
  bool r = true ;
  println p == q || r ;
  println p == (q || r)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
equalsOverOr.s contents are:
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
16		mov rax, 1
17		mov r13, rax
18		mov rax, 1
19		mov r14, rax
20		# Stack pointer unchanged, no stack allocated arguments
21		cmp r12, r13
22		sete al
23		movsx rax, al
24		push rax
25		pop rax
26		cmp rax, 1
27		je .L0
28		cmp r14, 1
29	.L0:
30		sete al
31		movsx rax, al
32		push rax
33		pop rax
34		mov rax, rax
35		mov rdi, rax
36		# statement primitives do not return results (but will clobber r0/rax)
37		call _printb
38		call _println
39		# Stack pointer unchanged, no stack allocated arguments
40		cmp r13, 1
41		je .L1
42		cmp r14, 1
43	.L1:
44		sete al
45		movsx rax, al
46		push rax
47		pop rbx
48		cmp r12, rbx
49		sete al
50		movsx rax, al
51		push rax
52		pop rax
53		mov rax, rax
54		mov rdi, rax
55		# statement primitives do not return results (but will clobber r0/rax)
56		call _printb
57		call _println
58		# Stack pointer unchanged, no stack allocated variables
59		mov rax, 0
60		# pop {rbx, r12, r13, r14}
61		mov rbx, qword ptr [rsp]
62		mov r12, qword ptr [rsp + 8]
63		mov r13, qword ptr [rsp + 16]
64		mov r14, qword ptr [rsp + 24]
65		add rsp, 32
66		pop rbp
67		ret
68	
69	.section .rodata
70	# length of .L._printb_str0
71		.int 5
72	.L._printb_str0:
73		.asciz "false"
74	# length of .L._printb_str1
75		.int 4
76	.L._printb_str1:
77		.asciz "true"
78	# length of .L._printb_str2
79		.int 4
80	.L._printb_str2:
81		.asciz "%.*s"
82	.text
83	_printb:
84		push rbp
85		mov rbp, rsp
86		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
87		and rsp, -16
88		cmp dil, 0
89		jne .L_printb0
90		lea rdx, [rip + .L._printb_str0]
91		jmp .L_printb1
92	.L_printb0:
93		lea rdx, [rip + .L._printb_str1]
94	.L_printb1:
95		mov esi, dword ptr [rdx - 4]
96		lea rdi, [rip + .L._printb_str2]
97		# on x86, al represents the number of SIMD registers used as variadic arguments
98		mov al, 0
99		call printf@plt
100		mov rdi, 0
101		call fflush@plt
102		mov rsp, rbp
103		pop rbp
104		ret
105	
106	.section .rodata
107	# length of .L._println_str0
108		.int 0
109	.L._println_str0:
110		.asciz ""
111	.text
112	_println:
113		push rbp
114		mov rbp, rsp
115		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
116		and rsp, -16
117		lea rdi, [rip + .L._println_str0]
118		call puts@plt
119		mov rdi, 0
120		call fflush@plt
121		mov rsp, rbp
122		pop rbp
123		ret
===========================================================
-- Finished

