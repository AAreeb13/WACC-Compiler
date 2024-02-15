./valid/expressions/greaterEqExpr.wacc
calling the reference compiler on ./valid/expressions/greaterEqExpr.wacc
-- Test: greaterEqExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating greater-than

# Output:
# false
# true
# true
#

# Program:

begin
  int x = 2 ;
  int y = 6 ;
  int z = 4 ;
  int a = 4 ;
  println x >= y ;
  println y >= z ;
  println z >= z
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
greaterEqExpr.s contents are:
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
15		mov rax, 2
16		mov r12, rax
17		mov rax, 6
18		mov r13, rax
19		mov rax, 4
20		mov r14, rax
21		mov rax, 4
22		mov r15, rax
23		# Stack pointer unchanged, no stack allocated arguments
24		cmp r12, r13
25		setge al
26		movsx rax, al
27		push rax
28		pop rax
29		mov rax, rax
30		mov rdi, rax
31		# statement primitives do not return results (but will clobber r0/rax)
32		call _printb
33		call _println
34		# Stack pointer unchanged, no stack allocated arguments
35		cmp r13, r14
36		setge al
37		movsx rax, al
38		push rax
39		pop rax
40		mov rax, rax
41		mov rdi, rax
42		# statement primitives do not return results (but will clobber r0/rax)
43		call _printb
44		call _println
45		# Stack pointer unchanged, no stack allocated arguments
46		cmp r14, r14
47		setge al
48		movsx rax, al
49		push rax
50		pop rax
51		mov rax, rax
52		mov rdi, rax
53		# statement primitives do not return results (but will clobber r0/rax)
54		call _printb
55		call _println
56		# Stack pointer unchanged, no stack allocated variables
57		mov rax, 0
58		# pop {rbx, r12, r13, r14, r15}
59		mov rbx, qword ptr [rsp]
60		mov r12, qword ptr [rsp + 8]
61		mov r13, qword ptr [rsp + 16]
62		mov r14, qword ptr [rsp + 24]
63		mov r15, qword ptr [rsp + 32]
64		add rsp, 40
65		pop rbp
66		ret
67	
68	.section .rodata
69	# length of .L._printb_str0
70		.int 5
71	.L._printb_str0:
72		.asciz "false"
73	# length of .L._printb_str1
74		.int 4
75	.L._printb_str1:
76		.asciz "true"
77	# length of .L._printb_str2
78		.int 4
79	.L._printb_str2:
80		.asciz "%.*s"
81	.text
82	_printb:
83		push rbp
84		mov rbp, rsp
85		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
86		and rsp, -16
87		cmp dil, 0
88		jne .L_printb0
89		lea rdx, [rip + .L._printb_str0]
90		jmp .L_printb1
91	.L_printb0:
92		lea rdx, [rip + .L._printb_str1]
93	.L_printb1:
94		mov esi, dword ptr [rdx - 4]
95		lea rdi, [rip + .L._printb_str2]
96		# on x86, al represents the number of SIMD registers used as variadic arguments
97		mov al, 0
98		call printf@plt
99		mov rdi, 0
100		call fflush@plt
101		mov rsp, rbp
102		pop rbp
103		ret
104	
105	.section .rodata
106	# length of .L._println_str0
107		.int 0
108	.L._println_str0:
109		.asciz ""
110	.text
111	_println:
112		push rbp
113		mov rbp, rsp
114		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
115		and rsp, -16
116		lea rdi, [rip + .L._println_str0]
117		call puts@plt
118		mov rdi, 0
119		call fflush@plt
120		mov rsp, rbp
121		pop rbp
122		ret
===========================================================
-- Finished

