./valid/expressions/notExpr.wacc
calling the reference compiler on ./valid/expressions/notExpr.wacc
-- Test: notExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating not

# Output:
# false
# true
#

# Program:

begin
  bool a = true ;
  bool b = false ;
  println !a ;
  println !b
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
notExpr.s contents are:
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
19		setne al
20		movsx rax, al
21		push rax
22		pop rax
23		mov rax, rax
24		mov rdi, rax
25		# statement primitives do not return results (but will clobber r0/rax)
26		call _printb
27		call _println
28		# Stack pointer unchanged, no stack allocated arguments
29		cmp r13, 1
30		setne al
31		movsx rax, al
32		push rax
33		pop rax
34		mov rax, rax
35		mov rdi, rax
36		# statement primitives do not return results (but will clobber r0/rax)
37		call _printb
38		call _println
39		# Stack pointer unchanged, no stack allocated variables
40		mov rax, 0
41		# pop {rbx, r12, r13}
42		mov rbx, qword ptr [rsp]
43		mov r12, qword ptr [rsp + 8]
44		mov r13, qword ptr [rsp + 16]
45		add rsp, 24
46		pop rbp
47		ret
48	
49	.section .rodata
50	# length of .L._printb_str0
51		.int 5
52	.L._printb_str0:
53		.asciz "false"
54	# length of .L._printb_str1
55		.int 4
56	.L._printb_str1:
57		.asciz "true"
58	# length of .L._printb_str2
59		.int 4
60	.L._printb_str2:
61		.asciz "%.*s"
62	.text
63	_printb:
64		push rbp
65		mov rbp, rsp
66		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
67		and rsp, -16
68		cmp dil, 0
69		jne .L_printb0
70		lea rdx, [rip + .L._printb_str0]
71		jmp .L_printb1
72	.L_printb0:
73		lea rdx, [rip + .L._printb_str1]
74	.L_printb1:
75		mov esi, dword ptr [rdx - 4]
76		lea rdi, [rip + .L._printb_str2]
77		# on x86, al represents the number of SIMD registers used as variadic arguments
78		mov al, 0
79		call printf@plt
80		mov rdi, 0
81		call fflush@plt
82		mov rsp, rbp
83		pop rbp
84		ret
85	
86	.section .rodata
87	# length of .L._println_str0
88		.int 0
89	.L._println_str0:
90		.asciz ""
91	.text
92	_println:
93		push rbp
94		mov rbp, rsp
95		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
96		and rsp, -16
97		lea rdi, [rip + .L._println_str0]
98		call puts@plt
99		mov rdi, 0
100		call fflush@plt
101		mov rsp, rbp
102		pop rbp
103		ret
===========================================================
-- Finished

