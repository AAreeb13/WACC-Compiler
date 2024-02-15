./valid/expressions/greaterExpr.wacc
calling the reference compiler on ./valid/expressions/greaterExpr.wacc
-- Test: greaterExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating greater-than

# Output:
# false
# true
#

# Program:

begin
  int x = 2 ;
  int y = 6 ;
  int z = 4 ;
  println x > y ;
  println y > z
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
greaterExpr.s contents are:
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
14		mov rax, 2
15		mov r12, rax
16		mov rax, 6
17		mov r13, rax
18		mov rax, 4
19		mov r14, rax
20		# Stack pointer unchanged, no stack allocated arguments
21		cmp r12, r13
22		setg al
23		movsx rax, al
24		push rax
25		pop rax
26		mov rax, rax
27		mov rdi, rax
28		# statement primitives do not return results (but will clobber r0/rax)
29		call _printb
30		call _println
31		# Stack pointer unchanged, no stack allocated arguments
32		cmp r13, r14
33		setg al
34		movsx rax, al
35		push rax
36		pop rax
37		mov rax, rax
38		mov rdi, rax
39		# statement primitives do not return results (but will clobber r0/rax)
40		call _printb
41		call _println
42		# Stack pointer unchanged, no stack allocated variables
43		mov rax, 0
44		# pop {rbx, r12, r13, r14}
45		mov rbx, qword ptr [rsp]
46		mov r12, qword ptr [rsp + 8]
47		mov r13, qword ptr [rsp + 16]
48		mov r14, qword ptr [rsp + 24]
49		add rsp, 32
50		pop rbp
51		ret
52	
53	.section .rodata
54	# length of .L._printb_str0
55		.int 5
56	.L._printb_str0:
57		.asciz "false"
58	# length of .L._printb_str1
59		.int 4
60	.L._printb_str1:
61		.asciz "true"
62	# length of .L._printb_str2
63		.int 4
64	.L._printb_str2:
65		.asciz "%.*s"
66	.text
67	_printb:
68		push rbp
69		mov rbp, rsp
70		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
71		and rsp, -16
72		cmp dil, 0
73		jne .L_printb0
74		lea rdx, [rip + .L._printb_str0]
75		jmp .L_printb1
76	.L_printb0:
77		lea rdx, [rip + .L._printb_str1]
78	.L_printb1:
79		mov esi, dword ptr [rdx - 4]
80		lea rdi, [rip + .L._printb_str2]
81		# on x86, al represents the number of SIMD registers used as variadic arguments
82		mov al, 0
83		call printf@plt
84		mov rdi, 0
85		call fflush@plt
86		mov rsp, rbp
87		pop rbp
88		ret
89	
90	.section .rodata
91	# length of .L._println_str0
92		.int 0
93	.L._println_str0:
94		.asciz ""
95	.text
96	_println:
97		push rbp
98		mov rbp, rsp
99		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
100		and rsp, -16
101		lea rdi, [rip + .L._println_str0]
102		call puts@plt
103		mov rdi, 0
104		call fflush@plt
105		mov rsp, rbp
106		pop rbp
107		ret
===========================================================
-- Finished

