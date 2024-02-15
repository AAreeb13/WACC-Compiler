./valid/expressions/charComparisonExpr.wacc
calling the reference compiler on ./valid/expressions/charComparisonExpr.wacc
-- Test: charComparisonExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# detailed battery of character comparison tests

# Output:
# false
# true
# true
# true
# false
# false
#

# Program:

begin
  char c1 = 'a' ;
  char c2 = 'z' ;
  println c1 == c2 ;
  println c1 != c2 ;
  println c1 < c2 ;
  println c1 <= c2 ;
  println c1 > c2 ;
  println c1 >= c2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
charComparisonExpr.s contents are:
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
13		mov rax, 97
14		mov r12, rax
15		mov rax, 122
16		mov r13, rax
17		# Stack pointer unchanged, no stack allocated arguments
18		cmp r12, r13
19		sete al
20		movsx rax, al
21		push rax
22		pop rax
23		mov rax, rax
24		mov rdi, rax
25		# statement primitives do not return results (but will clobber r0/rax)
26		call _printb
27		call _println
28		# Stack pointer unchanged, no stack allocated arguments
29		cmp r12, r13
30		setne al
31		movsx rax, al
32		push rax
33		pop rax
34		mov rax, rax
35		mov rdi, rax
36		# statement primitives do not return results (but will clobber r0/rax)
37		call _printb
38		call _println
39		# Stack pointer unchanged, no stack allocated arguments
40		cmp r12, r13
41		setl al
42		movsx rax, al
43		push rax
44		pop rax
45		mov rax, rax
46		mov rdi, rax
47		# statement primitives do not return results (but will clobber r0/rax)
48		call _printb
49		call _println
50		# Stack pointer unchanged, no stack allocated arguments
51		cmp r12, r13
52		setle al
53		movsx rax, al
54		push rax
55		pop rax
56		mov rax, rax
57		mov rdi, rax
58		# statement primitives do not return results (but will clobber r0/rax)
59		call _printb
60		call _println
61		# Stack pointer unchanged, no stack allocated arguments
62		cmp r12, r13
63		setg al
64		movsx rax, al
65		push rax
66		pop rax
67		mov rax, rax
68		mov rdi, rax
69		# statement primitives do not return results (but will clobber r0/rax)
70		call _printb
71		call _println
72		# Stack pointer unchanged, no stack allocated arguments
73		cmp r12, r13
74		setge al
75		movsx rax, al
76		push rax
77		pop rax
78		mov rax, rax
79		mov rdi, rax
80		# statement primitives do not return results (but will clobber r0/rax)
81		call _printb
82		call _println
83		# Stack pointer unchanged, no stack allocated variables
84		mov rax, 0
85		# pop {rbx, r12, r13}
86		mov rbx, qword ptr [rsp]
87		mov r12, qword ptr [rsp + 8]
88		mov r13, qword ptr [rsp + 16]
89		add rsp, 24
90		pop rbp
91		ret
92	
93	.section .rodata
94	# length of .L._printb_str0
95		.int 5
96	.L._printb_str0:
97		.asciz "false"
98	# length of .L._printb_str1
99		.int 4
100	.L._printb_str1:
101		.asciz "true"
102	# length of .L._printb_str2
103		.int 4
104	.L._printb_str2:
105		.asciz "%.*s"
106	.text
107	_printb:
108		push rbp
109		mov rbp, rsp
110		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
111		and rsp, -16
112		cmp dil, 0
113		jne .L_printb0
114		lea rdx, [rip + .L._printb_str0]
115		jmp .L_printb1
116	.L_printb0:
117		lea rdx, [rip + .L._printb_str1]
118	.L_printb1:
119		mov esi, dword ptr [rdx - 4]
120		lea rdi, [rip + .L._printb_str2]
121		# on x86, al represents the number of SIMD registers used as variadic arguments
122		mov al, 0
123		call printf@plt
124		mov rdi, 0
125		call fflush@plt
126		mov rsp, rbp
127		pop rbp
128		ret
129	
130	.section .rodata
131	# length of .L._println_str0
132		.int 0
133	.L._println_str0:
134		.asciz ""
135	.text
136	_println:
137		push rbp
138		mov rbp, rsp
139		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
140		and rsp, -16
141		lea rdi, [rip + .L._println_str0]
142		call puts@plt
143		mov rdi, 0
144		call fflush@plt
145		mov rsp, rbp
146		pop rbp
147		ret
===========================================================
-- Finished

