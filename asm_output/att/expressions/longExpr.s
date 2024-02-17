./valid/expressions/longExpr.wacc
calling the reference compiler on ./valid/expressions/longExpr.wacc
-- Test: longExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions

# Output:

# Exit:
# 153

begin

  int x = 1 + (2 + (3 + (4 + (5 + (6 + (7 + (8 + (9 + (10 + (11 + (12 + (13 + (14 + (15 + (16 + 17)))))))))))))));
  exit x

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longExpr.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12}
6		subq $16, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %rsp, %rbp
10		# Stack pointer unchanged, no stack allocated variables
11		movl $16, %eax
12		addl $17, %eax
13		jo _errOverflow
14		movslq %eax, %rax
15		pushq %rax
16		popq %rbx
17		movl $15, %eax
18		addl %ebx, %eax
19		jo _errOverflow
20		movslq %eax, %rax
21		pushq %rax
22		popq %rbx
23		movl $14, %eax
24		addl %ebx, %eax
25		jo _errOverflow
26		movslq %eax, %rax
27		pushq %rax
28		popq %rbx
29		movl $13, %eax
30		addl %ebx, %eax
31		jo _errOverflow
32		movslq %eax, %rax
33		pushq %rax
34		popq %rbx
35		movl $12, %eax
36		addl %ebx, %eax
37		jo _errOverflow
38		movslq %eax, %rax
39		pushq %rax
40		popq %rbx
41		movl $11, %eax
42		addl %ebx, %eax
43		jo _errOverflow
44		movslq %eax, %rax
45		pushq %rax
46		popq %rbx
47		movl $10, %eax
48		addl %ebx, %eax
49		jo _errOverflow
50		movslq %eax, %rax
51		pushq %rax
52		popq %rbx
53		movl $9, %eax
54		addl %ebx, %eax
55		jo _errOverflow
56		movslq %eax, %rax
57		pushq %rax
58		popq %rbx
59		movl $8, %eax
60		addl %ebx, %eax
61		jo _errOverflow
62		movslq %eax, %rax
63		pushq %rax
64		popq %rbx
65		movl $7, %eax
66		addl %ebx, %eax
67		jo _errOverflow
68		movslq %eax, %rax
69		pushq %rax
70		popq %rbx
71		movl $6, %eax
72		addl %ebx, %eax
73		jo _errOverflow
74		movslq %eax, %rax
75		pushq %rax
76		popq %rbx
77		movl $5, %eax
78		addl %ebx, %eax
79		jo _errOverflow
80		movslq %eax, %rax
81		pushq %rax
82		popq %rbx
83		movl $4, %eax
84		addl %ebx, %eax
85		jo _errOverflow
86		movslq %eax, %rax
87		pushq %rax
88		popq %rbx
89		movl $3, %eax
90		addl %ebx, %eax
91		jo _errOverflow
92		movslq %eax, %rax
93		pushq %rax
94		popq %rbx
95		movl $2, %eax
96		addl %ebx, %eax
97		jo _errOverflow
98		movslq %eax, %rax
99		pushq %rax
100		popq %rbx
101		movl $1, %eax
102		addl %ebx, %eax
103		jo _errOverflow
104		movslq %eax, %rax
105		pushq %rax
106		popq %rax
107		movq %rax, %rax
108		movq %rax, %r12
109		# Stack pointer unchanged, no stack allocated arguments
110		movq %r12, %rax
111		movq %rax, %rdi
112		# statement primitives do not return results (but will clobber r0/rax)
113		call _exit
114		# Stack pointer unchanged, no stack allocated variables
115		movq $0, %rax
116		# popq {%rbx, %r12}
117		movq (%rsp), %rbx
118		movq 8(%rsp), %r12
119		addq $16, %rsp
120		popq %rbp
121		ret
122	
123	.section .rodata
124	# length of .L._prints_str0
125		.int 4
126	.L._prints_str0:
127		.asciz "%.*s"
128	.text
129	_prints:
130		pushq %rbp
131		movq %rsp, %rbp
132		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
133		andq $-16, %rsp
134		movq %rdi, %rdx
135		movl -4(%rdi), %esi
136		leaq .L._prints_str0(%rip), %rdi
137		# on x86, al represents the number of SIMD registers used as variadic arguments
138		movb $0, %al
139		call printf@plt
140		movq $0, %rdi
141		call fflush@plt
142		movq %rbp, %rsp
143		popq %rbp
144		ret
145	
146	_exit:
147		pushq %rbp
148		movq %rsp, %rbp
149		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
150		andq $-16, %rsp
151		call exit@plt
152		movq %rbp, %rsp
153		popq %rbp
154		ret
155	
156	.section .rodata
157	# length of .L._errOverflow_str0
158		.int 52
159	.L._errOverflow_str0:
160		.asciz "fatal error: integer overflow or underflow occurred\n"
161	.text
162	_errOverflow:
163		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
164		andq $-16, %rsp
165		leaq .L._errOverflow_str0(%rip), %rdi
166		call _prints
167		movb $-1, %dil
168		call exit@plt
===========================================================
-- Finished

