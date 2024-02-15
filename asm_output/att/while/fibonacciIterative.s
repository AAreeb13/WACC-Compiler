./valid/while/fibonacciIterative.wacc
calling the reference compiler on ./valid/while/fibonacciIterative.wacc
-- Test: fibonacciIterative.wacc

-- Uploaded file: 
---------------------------------------------------------------
# iterative calculation of the first 20 fibonacci numbers

# Output:
# The first 20 fibonacci numbers are:
# 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, ...
#

# Program:

begin
  int i = 0 ;
  int f0 = 0 ;
  int f1 = 1 ;
  int save = 0;
  println "The first 20 fibonacci numbers are:" ;
  while i < 20 do
    print f0 ;
    print ", " ;
    save = f0 ;
    f0 = f1 ;
    f1 = save + f1 ;
    i = i + 1
  done ;
  println "..."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
fibonacciIterative.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 35
4	.L.str0:
5		.asciz "The first 20 fibonacci numbers are:"
6	# length of .L.str1
7		.int 2
8	.L.str1:
9		.asciz ", "
10	# length of .L.str2
11		.int 3
12	.L.str2:
13		.asciz "..."
14	.text
15	main:
16		pushq %rbp
17		# pushq {%rbx, %r12, %r13, %r14, %r15}
18		subq $40, %rsp
19		movq %rbx, (%rsp)
20		movq %r12, 8(%rsp)
21		movq %r13, 16(%rsp)
22		movq %r14, 24(%rsp)
23		movq %r15, 32(%rsp)
24		movq %rsp, %rbp
25		# Stack pointer unchanged, no stack allocated variables
26		movq $0, %rax
27		movq %rax, %r12
28		movq $0, %rax
29		movq %rax, %r13
30		movq $1, %rax
31		movq %rax, %r14
32		movq $0, %rax
33		movq %rax, %r15
34		# Stack pointer unchanged, no stack allocated arguments
35		leaq .L.str0(%rip), %rax
36		pushq %rax
37		popq %rax
38		movq %rax, %rax
39		movq %rax, %rdi
40		# statement primitives do not return results (but will clobber r0/rax)
41		call _prints
42		call _println
43		jmp .L0
44	.L1:
45		# Stack pointer unchanged, no stack allocated arguments
46		movq %r13, %rax
47		movq %rax, %rdi
48		# statement primitives do not return results (but will clobber r0/rax)
49		call _printi
50		# Stack pointer unchanged, no stack allocated arguments
51		leaq .L.str1(%rip), %rax
52		pushq %rax
53		popq %rax
54		movq %rax, %rax
55		movq %rax, %rdi
56		# statement primitives do not return results (but will clobber r0/rax)
57		call _prints
58		movq %r13, %rax
59		movq %rax, %r15
60		movq %r14, %rax
61		movq %rax, %r13
62		movl %r15d, %eax
63		addl %r14d, %eax
64		jo _errOverflow
65		movslq %eax, %rax
66		pushq %rax
67		popq %rax
68		movq %rax, %rax
69		movq %rax, %r14
70		movl %r12d, %eax
71		addl $1, %eax
72		jo _errOverflow
73		movslq %eax, %rax
74		pushq %rax
75		popq %rax
76		movq %rax, %rax
77		movq %rax, %r12
78	.L0:
79		cmpq $20, %r12
80		jl .L1
81		# Stack pointer unchanged, no stack allocated arguments
82		leaq .L.str2(%rip), %rax
83		pushq %rax
84		popq %rax
85		movq %rax, %rax
86		movq %rax, %rdi
87		# statement primitives do not return results (but will clobber r0/rax)
88		call _prints
89		call _println
90		# Stack pointer unchanged, no stack allocated variables
91		movq $0, %rax
92		# popq {%rbx, %r12, %r13, %r14, %r15}
93		movq (%rsp), %rbx
94		movq 8(%rsp), %r12
95		movq 16(%rsp), %r13
96		movq 24(%rsp), %r14
97		movq 32(%rsp), %r15
98		addq $40, %rsp
99		popq %rbp
100		ret
101	
102	.section .rodata
103	# length of .L._prints_str0
104		.int 4
105	.L._prints_str0:
106		.asciz "%.*s"
107	.text
108	_prints:
109		pushq %rbp
110		movq %rsp, %rbp
111		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
112		andq $-16, %rsp
113		movq %rdi, %rdx
114		movl -4(%rdi), %esi
115		leaq .L._prints_str0(%rip), %rdi
116		# on x86, al represents the number of SIMD registers used as variadic arguments
117		movb $0, %al
118		call printf@plt
119		movq $0, %rdi
120		call fflush@plt
121		movq %rbp, %rsp
122		popq %rbp
123		ret
124	
125	.section .rodata
126	# length of .L._printi_str0
127		.int 2
128	.L._printi_str0:
129		.asciz "%d"
130	.text
131	_printi:
132		pushq %rbp
133		movq %rsp, %rbp
134		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
135		andq $-16, %rsp
136		movl %edi, %esi
137		leaq .L._printi_str0(%rip), %rdi
138		# on x86, al represents the number of SIMD registers used as variadic arguments
139		movb $0, %al
140		call printf@plt
141		movq $0, %rdi
142		call fflush@plt
143		movq %rbp, %rsp
144		popq %rbp
145		ret
146	
147	.section .rodata
148	# length of .L._println_str0
149		.int 0
150	.L._println_str0:
151		.asciz ""
152	.text
153	_println:
154		pushq %rbp
155		movq %rsp, %rbp
156		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
157		andq $-16, %rsp
158		leaq .L._println_str0(%rip), %rdi
159		call puts@plt
160		movq $0, %rdi
161		call fflush@plt
162		movq %rbp, %rsp
163		popq %rbp
164		ret
165	
166	_exit:
167		pushq %rbp
168		movq %rsp, %rbp
169		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
170		andq $-16, %rsp
171		call exit@plt
172		movq %rbp, %rsp
173		popq %rbp
174		ret
175	
176	.section .rodata
177	# length of .L._errOverflow_str0
178		.int 52
179	.L._errOverflow_str0:
180		.asciz "fatal error: integer overflow or underflow occurred\n"
181	.text
182	_errOverflow:
183		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
184		andq $-16, %rsp
185		leaq .L._errOverflow_str0(%rip), %rdi
186		call _prints
187		movb $-1, %dil
188		call exit@plt
===========================================================
-- Finished

