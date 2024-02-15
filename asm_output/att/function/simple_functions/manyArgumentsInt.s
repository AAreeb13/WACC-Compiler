./valid/function/simple_functions/manyArgumentsInt.wacc
calling the reference compiler on ./valid/function/simple_functions/manyArgumentsInt.wacc
-- Test: manyArgumentsInt.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests a function with more arguments than fit into registers

# Output:
# 23
#

# Program:

begin
  # w, x, y and z are stack allocated on aarch32
  # y and z are stack allocated on x86-64
  int f(int s, int t, int u, int v, int w, int x, int y, int z) is
    int i = u + v ;
    int j = w * x ;
    int k = y - z ;
    return i + j * k
  end

  int r = call f(0, 0, 1, 4, 2, 3, 7, 4) ;
  println r
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
manyArgumentsInt.s contents are:
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
11		subq $8, %rsp
12		movq $0, %rax
13		movq %rax, %rdi
14		movq $0, %rax
15		movq %rax, %rsi
16		movq $1, %rax
17		movq %rax, %rdx
18		movq $4, %rax
19		movq %rax, %rcx
20		movq $2, %rax
21		movq %rax, %r8
22		movq $3, %rax
23		movq %rax, %r9
24		movq $7, %rax
25		movl %eax, 4(%rsp)
26		movq $4, %rax
27		movl %eax, (%rsp)
28		call wacc_f
29		movq %rax, %r11
30		addq $8, %rsp
31		movq %r11, %rax
32		movq %rax, %r12
33		# Stack pointer unchanged, no stack allocated arguments
34		movq %r12, %rax
35		movq %rax, %rdi
36		# statement primitives do not return results (but will clobber r0/rax)
37		call _printi
38		call _println
39		# Stack pointer unchanged, no stack allocated variables
40		movq $0, %rax
41		# popq {%rbx, %r12}
42		movq (%rsp), %rbx
43		movq 8(%rsp), %r12
44		addq $16, %rsp
45		popq %rbp
46		ret
47	
48	wacc_f:
49		pushq %rbp
50		# pushq {%r12, %r13, %r14}
51		subq $24, %rsp
52		movq %r12, (%rsp)
53		movq %r13, 8(%rsp)
54		movq %r14, 16(%rsp)
55		movq %rsp, %rbp
56		# Stack pointer unchanged, no stack allocated variables
57		movl %edx, %eax
58		addl %ecx, %eax
59		jo _errOverflow
60		movslq %eax, %rax
61		pushq %rax
62		popq %rax
63		movq %rax, %rax
64		movq %rax, %r12
65		movl %r8d, %eax
66		imull %r9d, %eax
67		jo _errOverflow
68		movslq %eax, %rax
69		pushq %rax
70		popq %rax
71		movq %rax, %rax
72		movq %rax, %r13
73		movslq 40(%rbp), %rbx
74		movslq 44(%rbp), %rax
75		movl %eax, %eax
76		subl %ebx, %eax
77		jo _errOverflow
78		movslq %eax, %rax
79		pushq %rax
80		popq %rax
81		movq %rax, %rax
82		movq %rax, %r14
83		movl %r13d, %eax
84		imull %r14d, %eax
85		jo _errOverflow
86		movslq %eax, %rax
87		pushq %rax
88		popq %rbx
89		movl %r12d, %eax
90		addl %ebx, %eax
91		jo _errOverflow
92		movslq %eax, %rax
93		pushq %rax
94		popq %rax
95		movq %rax, %rax
96		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
97		movq %rbp, %rsp
98		# popq {%r12, %r13, %r14}
99		movq (%rsp), %r12
100		movq 8(%rsp), %r13
101		movq 16(%rsp), %r14
102		addq $24, %rsp
103		popq %rbp
104		ret
105		# Stack pointer unchanged, no stack allocated variables
106		# 'ere be dragons: this is 100% dead code, functions always end in returns!
107	
108	.section .rodata
109	# length of .L._prints_str0
110		.int 4
111	.L._prints_str0:
112		.asciz "%.*s"
113	.text
114	_prints:
115		pushq %rbp
116		movq %rsp, %rbp
117		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
118		andq $-16, %rsp
119		movq %rdi, %rdx
120		movl -4(%rdi), %esi
121		leaq .L._prints_str0(%rip), %rdi
122		# on x86, al represents the number of SIMD registers used as variadic arguments
123		movb $0, %al
124		call printf@plt
125		movq $0, %rdi
126		call fflush@plt
127		movq %rbp, %rsp
128		popq %rbp
129		ret
130	
131	.section .rodata
132	# length of .L._printi_str0
133		.int 2
134	.L._printi_str0:
135		.asciz "%d"
136	.text
137	_printi:
138		pushq %rbp
139		movq %rsp, %rbp
140		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
141		andq $-16, %rsp
142		movl %edi, %esi
143		leaq .L._printi_str0(%rip), %rdi
144		# on x86, al represents the number of SIMD registers used as variadic arguments
145		movb $0, %al
146		call printf@plt
147		movq $0, %rdi
148		call fflush@plt
149		movq %rbp, %rsp
150		popq %rbp
151		ret
152	
153	.section .rodata
154	# length of .L._println_str0
155		.int 0
156	.L._println_str0:
157		.asciz ""
158	.text
159	_println:
160		pushq %rbp
161		movq %rsp, %rbp
162		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
163		andq $-16, %rsp
164		leaq .L._println_str0(%rip), %rdi
165		call puts@plt
166		movq $0, %rdi
167		call fflush@plt
168		movq %rbp, %rsp
169		popq %rbp
170		ret
171	
172	_exit:
173		pushq %rbp
174		movq %rsp, %rbp
175		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
176		andq $-16, %rsp
177		call exit@plt
178		movq %rbp, %rsp
179		popq %rbp
180		ret
181	
182	.section .rodata
183	# length of .L._errOverflow_str0
184		.int 52
185	.L._errOverflow_str0:
186		.asciz "fatal error: integer overflow or underflow occurred\n"
187	.text
188	_errOverflow:
189		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
190		andq $-16, %rsp
191		leaq .L._errOverflow_str0(%rip), %rdi
192		call _prints
193		movb $-1, %dil
194		call exit@plt
===========================================================
-- Finished

