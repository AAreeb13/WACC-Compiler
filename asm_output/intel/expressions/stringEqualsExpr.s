./valid/expressions/stringEqualsExpr.wacc
calling the reference compiler on ./valid/expressions/stringEqualsExpr.wacc
-- Test: stringEqualsExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating string equality

# Output:
# true
# false
# false
#

# Program:

begin
  string s1 = "Hello" ;
  string s2 = "foo" ;
  string s3 = "bar" ;
  bool b = s1 == s1 ;
  println b ;
  println s1 == s2 ;
  println s2 == s3
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
stringEqualsExpr.s contents are:
===========================================================
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 5
5	.L.str0:
6		.asciz "Hello"
7	# length of .L.str1
8		.int 3
9	.L.str1:
10		.asciz "foo"
11	# length of .L.str2
12		.int 3
13	.L.str2:
14		.asciz "bar"
15	.text
16	main:
17		push rbp
18		# push {rbx, r12, r13, r14, r15}
19		sub rsp, 40
20		mov qword ptr [rsp], rbx
21		mov qword ptr [rsp + 8], r12
22		mov qword ptr [rsp + 16], r13
23		mov qword ptr [rsp + 24], r14
24		mov qword ptr [rsp + 32], r15
25		mov rbp, rsp
26		# Stack pointer unchanged, no stack allocated variables
27		lea rax, [rip + .L.str0]
28		push rax
29		pop rax
30		mov rax, rax
31		mov r12, rax
32		lea rax, [rip + .L.str1]
33		push rax
34		pop rax
35		mov rax, rax
36		mov r13, rax
37		lea rax, [rip + .L.str2]
38		push rax
39		pop rax
40		mov rax, rax
41		mov r14, rax
42		cmp r12, r12
43		sete al
44		movsx rax, al
45		push rax
46		pop rax
47		mov rax, rax
48		mov r15, rax
49		# Stack pointer unchanged, no stack allocated arguments
50		mov rax, r15
51		mov rdi, rax
52		# statement primitives do not return results (but will clobber r0/rax)
53		call _printb
54		call _println
55		# Stack pointer unchanged, no stack allocated arguments
56		cmp r12, r13
57		sete al
58		movsx rax, al
59		push rax
60		pop rax
61		mov rax, rax
62		mov rdi, rax
63		# statement primitives do not return results (but will clobber r0/rax)
64		call _printb
65		call _println
66		# Stack pointer unchanged, no stack allocated arguments
67		cmp r13, r14
68		sete al
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

