./valid/expressions/longExpr2.wacc
calling the reference compiler on ./valid/expressions/longExpr2.wacc
-- Test: longExpr2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions

# Output:

# Exit:
# 10

begin

  int x = (2 + 3 + 2 + 1 + 1 + 1) - (1 + 2) * (3 - 4 / 6) / ( 2 * (18 - 17) + (3 * 4 / 4 + 6));
  exit x

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longExpr2.s contents are:
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
11		movl $2, %eax
12		addl $3, %eax
13		jo _errOverflow
14		movslq %eax, %rax
15		pushq %rax
16		popq %rax
17		movl %eax, %eax
18		addl $2, %eax
19		jo _errOverflow
20		movslq %eax, %rax
21		pushq %rax
22		popq %rax
23		movl %eax, %eax
24		addl $1, %eax
25		jo _errOverflow
26		movslq %eax, %rax
27		pushq %rax
28		popq %rax
29		movl %eax, %eax
30		addl $1, %eax
31		jo _errOverflow
32		movslq %eax, %rax
33		pushq %rax
34		popq %rax
35		movl %eax, %eax
36		addl $1, %eax
37		jo _errOverflow
38		movslq %eax, %rax
39		pushq %rax
40		movl $1, %eax
41		addl $2, %eax
42		jo _errOverflow
43		movslq %eax, %rax
44		pushq %rax
45		movq $6, %rbx
46		movl $4, %eax
47		cmpl $0, %ebx
48		je _errDivZero
49		# sign extend EAX into EDX
50		cltd
51		idivl %ebx
52		movl %eax, %eax
53		movl %eax, %eax
54		movslq %eax, %rax
55		pushq %rax
56		popq %rbx
57		movl $3, %eax
58		subl %ebx, %eax
59		jo _errOverflow
60		movslq %eax, %rax
61		pushq %rax
62		popq %rbx
63		popq %rax
64		movl %eax, %eax
65		imull %ebx, %eax
66		jo _errOverflow
67		movslq %eax, %rax
68		pushq %rax
69		movl $18, %eax
70		subl $17, %eax
71		jo _errOverflow
72		movslq %eax, %rax
73		pushq %rax
74		popq %rbx
75		movq $2, %rax
76		movl %eax, %eax
77		imull %ebx, %eax
78		jo _errOverflow
79		movslq %eax, %rax
80		pushq %rax
81		movq $3, %rax
82		imull $4, %eax, %eax
83		jo _errOverflow
84		movslq %eax, %rax
85		pushq %rax
86		movq $4, %rbx
87		popq %rax
88		movl %eax, %eax
89		cmpl $0, %ebx
90		je _errDivZero
91		# sign extend EAX into EDX
92		cltd
93		idivl %ebx
94		movl %eax, %eax
95		movl %eax, %eax
96		movslq %eax, %rax
97		pushq %rax
98		popq %rax
99		movl %eax, %eax
100		addl $6, %eax
101		jo _errOverflow
102		movslq %eax, %rax
103		pushq %rax
104		popq %rbx
105		popq %rax
106		movl %eax, %eax
107		addl %ebx, %eax
108		jo _errOverflow
109		movslq %eax, %rax
110		pushq %rax
111		popq %rbx
112		popq %rax
113		movl %eax, %eax
114		cmpl $0, %ebx
115		je _errDivZero
116		# sign extend EAX into EDX
117		cltd
118		idivl %ebx
119		movl %eax, %eax
120		movl %eax, %eax
121		movslq %eax, %rax
122		pushq %rax
123		popq %rbx
124		popq %rax
125		movl %eax, %eax
126		subl %ebx, %eax
127		jo _errOverflow
128		movslq %eax, %rax
129		pushq %rax
130		popq %rax
131		movq %rax, %rax
132		movq %rax, %r12
133		# Stack pointer unchanged, no stack allocated arguments
134		movq %r12, %rax
135		movq %rax, %rdi
136		# statement primitives do not return results (but will clobber r0/rax)
137		call _exit
138		# Stack pointer unchanged, no stack allocated variables
139		movq $0, %rax
140		# popq {%rbx, %r12}
141		movq (%rsp), %rbx
142		movq 8(%rsp), %r12
143		addq $16, %rsp
144		popq %rbp
145		ret
146	
147	.section .rodata
148	# length of .L._prints_str0
149		.int 4
150	.L._prints_str0:
151		.asciz "%.*s"
152	.text
153	_prints:
154		pushq %rbp
155		movq %rsp, %rbp
156		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
157		andq $-16, %rsp
158		movq %rdi, %rdx
159		movl -4(%rdi), %esi
160		leaq .L._prints_str0(%rip), %rdi
161		# on x86, al represents the number of SIMD registers used as variadic arguments
162		movb $0, %al
163		call printf@plt
164		movq $0, %rdi
165		call fflush@plt
166		movq %rbp, %rsp
167		popq %rbp
168		ret
169	
170	_exit:
171		pushq %rbp
172		movq %rsp, %rbp
173		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
174		andq $-16, %rsp
175		call exit@plt
176		movq %rbp, %rsp
177		popq %rbp
178		ret
179	
180	.section .rodata
181	# length of .L._errDivZero_str0
182		.int 40
183	.L._errDivZero_str0:
184		.asciz "fatal error: division or modulo by zero\n"
185	.text
186	_errDivZero:
187		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
188		andq $-16, %rsp
189		leaq .L._errDivZero_str0(%rip), %rdi
190		call _prints
191		movb $-1, %dil
192		call exit@plt
193	
194	.section .rodata
195	# length of .L._errOverflow_str0
196		.int 52
197	.L._errOverflow_str0:
198		.asciz "fatal error: integer overflow or underflow occurred\n"
199	.text
200	_errOverflow:
201		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
202		andq $-16, %rsp
203		leaq .L._errOverflow_str0(%rip), %rdi
204		call _prints
205		movb $-1, %dil
206		call exit@plt
===========================================================
-- Finished

