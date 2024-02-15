./valid/expressions/boolCalc.wacc
calling the reference compiler on ./valid/expressions/boolCalc.wacc
-- Test: boolCalc.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple boolean calculation

# Output:
# false
#

# Program:

begin
  bool b1 = true ;
  bool b2 = false ;
  bool b3 = b1 && b2 ;
  println b3
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
boolCalc.s contents are:
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
14		mov rax, 1
15		mov r12, rax
16		mov rax, 0
17		mov r13, rax
18		cmp r12, 1
19		jne .L0
20		cmp r13, 1
21	.L0:
22		sete al
23		movsx rax, al
24		push rax
25		pop rax
26		mov rax, rax
27		mov r14, rax
28		# Stack pointer unchanged, no stack allocated arguments
29		mov rax, r14
30		mov rdi, rax
31		# statement primitives do not return results (but will clobber r0/rax)
32		call _printb
33		call _println
34		# Stack pointer unchanged, no stack allocated variables
35		mov rax, 0
36		# pop {rbx, r12, r13, r14}
37		mov rbx, qword ptr [rsp]
38		mov r12, qword ptr [rsp + 8]
39		mov r13, qword ptr [rsp + 16]
40		mov r14, qword ptr [rsp + 24]
41		add rsp, 32
42		pop rbp
43		ret
44	
45	.section .rodata
46	# length of .L._printb_str0
47		.int 5
48	.L._printb_str0:
49		.asciz "false"
50	# length of .L._printb_str1
51		.int 4
52	.L._printb_str1:
53		.asciz "true"
54	# length of .L._printb_str2
55		.int 4
56	.L._printb_str2:
57		.asciz "%.*s"
58	.text
59	_printb:
60		push rbp
61		mov rbp, rsp
62		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
63		and rsp, -16
64		cmp dil, 0
65		jne .L_printb0
66		lea rdx, [rip + .L._printb_str0]
67		jmp .L_printb1
68	.L_printb0:
69		lea rdx, [rip + .L._printb_str1]
70	.L_printb1:
71		mov esi, dword ptr [rdx - 4]
72		lea rdi, [rip + .L._printb_str2]
73		# on x86, al represents the number of SIMD registers used as variadic arguments
74		mov al, 0
75		call printf@plt
76		mov rdi, 0
77		call fflush@plt
78		mov rsp, rbp
79		pop rbp
80		ret
81	
82	.section .rodata
83	# length of .L._println_str0
84		.int 0
85	.L._println_str0:
86		.asciz ""
87	.text
88	_println:
89		push rbp
90		mov rbp, rsp
91		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
92		and rsp, -16
93		lea rdi, [rip + .L._println_str0]
94		call puts@plt
95		mov rdi, 0
96		call fflush@plt
97		mov rsp, rbp
98		pop rbp
99		ret
===========================================================
-- Finished

