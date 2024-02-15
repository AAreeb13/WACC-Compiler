./valid/expressions/longSplitExpr.wacc
calling the reference compiler on ./valid/expressions/longSplitExpr.wacc
-- Test: longSplitExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions with several variables

# Output:

# Exit:
# 153

begin

  int a = 1 + 2 ;
  int b = 3 + 4 ;
  int c = 5 + 6 ;
  int d = 7 + 8 ;
  int e = 9 + 10 ;
  int f = 11 + 12 ;
  int g = 13 + 14 ;
  int h = 15 + 16 ;
  int i = 17 ;
  exit a + b + c + d + e + f + g + h + i

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longSplitExpr.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12, %r13, %r14, %r15}
6		subq $40, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %r13, 16(%rsp)
10		movq %r14, 24(%rsp)
11		movq %r15, 32(%rsp)
12		movq %rsp, %rbp
13		# Stack pointer unchanged, no stack allocated variables
14		movl $1, %eax
15		addl $2, %eax
16		jo _errOverflow
17		movslq %eax, %rax
18		pushq %rax
19		popq %rax
20		movq %rax, %rax
21		movq %rax, %r12
22		movl $3, %eax
23		addl $4, %eax
24		jo _errOverflow
25		movslq %eax, %rax
26		pushq %rax
27		popq %rax
28		movq %rax, %rax
29		movq %rax, %r13
30		movl $5, %eax
31		addl $6, %eax
32		jo _errOverflow
33		movslq %eax, %rax
34		pushq %rax
35		popq %rax
36		movq %rax, %rax
37		movq %rax, %r14
38		movl $7, %eax
39		addl $8, %eax
40		jo _errOverflow
41		movslq %eax, %rax
42		pushq %rax
43		popq %rax
44		movq %rax, %rax
45		movq %rax, %r15
46		movl $9, %eax
47		addl $10, %eax
48		jo _errOverflow
49		movslq %eax, %rax
50		pushq %rax
51		popq %rax
52		movq %rax, %rax
53		movq %rax, %rcx
54		movl $11, %eax
55		addl $12, %eax
56		jo _errOverflow
57		movslq %eax, %rax
58		pushq %rax
59		popq %rax
60		movq %rax, %rax
61		movq %rax, %rdx
62		movl $13, %eax
63		addl $14, %eax
64		jo _errOverflow
65		movslq %eax, %rax
66		pushq %rax
67		popq %rax
68		movq %rax, %rax
69		movq %rax, %rsi
70		movl $15, %eax
71		addl $16, %eax
72		jo _errOverflow
73		movslq %eax, %rax
74		pushq %rax
75		popq %rax
76		movq %rax, %rax
77		movq %rax, %rdi
78		movq $17, %rax
79		movq %rax, %r8
80		# pushq {%rcx, %rdx, %rsi, %rdi, %r8}
81		subq $40, %rsp
82		movq %rcx, (%rsp)
83		movq %rdx, 8(%rsp)
84		movq %rsi, 16(%rsp)
85		movq %rdi, 24(%rsp)
86		movq %r8, 32(%rsp)
87		# Set up R11 as a temporary second base pointer for the caller saved things
88		movq %rsp, %r11
89		# Stack pointer unchanged, no stack allocated arguments
90		movl %r12d, %eax
91		addl %r13d, %eax
92		jo _errOverflow
93		movslq %eax, %rax
94		pushq %rax
95		popq %rax
96		movl %eax, %eax
97		addl %r14d, %eax
98		jo _errOverflow
99		movslq %eax, %rax
100		pushq %rax
101		popq %rax
102		movl %eax, %eax
103		addl %r15d, %eax
104		jo _errOverflow
105		movslq %eax, %rax
106		pushq %rax
107		popq %rax
108		movl %eax, %eax
109		addl %ecx, %eax
110		jo _errOverflow
111		movslq %eax, %rax
112		pushq %rax
113		popq %rax
114		movl %eax, %eax
115		addl %edx, %eax
116		jo _errOverflow
117		movslq %eax, %rax
118		pushq %rax
119		popq %rax
120		movl %eax, %eax
121		addl %esi, %eax
122		jo _errOverflow
123		movslq %eax, %rax
124		pushq %rax
125		popq %rax
126		movl %eax, %eax
127		addl %edi, %eax
128		jo _errOverflow
129		movslq %eax, %rax
130		pushq %rax
131		popq %rax
132		movl %eax, %eax
133		addl %r8d, %eax
134		jo _errOverflow
135		movslq %eax, %rax
136		pushq %rax
137		popq %rax
138		movq %rax, %rax
139		movq %rax, %rdi
140		# statement primitives do not return results (but will clobber r0/rax)
141		call _exit
142		# popq {%rcx, %rdx, %rsi, %rdi, %r8}
143		movq (%rsp), %rcx
144		movq 8(%rsp), %rdx
145		movq 16(%rsp), %rsi
146		movq 24(%rsp), %rdi
147		movq 32(%rsp), %r8
148		addq $40, %rsp
149		# Stack pointer unchanged, no stack allocated variables
150		movq $0, %rax
151		# popq {%rbx, %r12, %r13, %r14, %r15}
152		movq (%rsp), %rbx
153		movq 8(%rsp), %r12
154		movq 16(%rsp), %r13
155		movq 24(%rsp), %r14
156		movq 32(%rsp), %r15
157		addq $40, %rsp
158		popq %rbp
159		ret
160	
161	.section .rodata
162	# length of .L._prints_str0
163		.int 4
164	.L._prints_str0:
165		.asciz "%.*s"
166	.text
167	_prints:
168		pushq %rbp
169		movq %rsp, %rbp
170		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
171		andq $-16, %rsp
172		movq %rdi, %rdx
173		movl -4(%rdi), %esi
174		leaq .L._prints_str0(%rip), %rdi
175		# on x86, al represents the number of SIMD registers used as variadic arguments
176		movb $0, %al
177		call printf@plt
178		movq $0, %rdi
179		call fflush@plt
180		movq %rbp, %rsp
181		popq %rbp
182		ret
183	
184	_exit:
185		pushq %rbp
186		movq %rsp, %rbp
187		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
188		andq $-16, %rsp
189		call exit@plt
190		movq %rbp, %rsp
191		popq %rbp
192		ret
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

