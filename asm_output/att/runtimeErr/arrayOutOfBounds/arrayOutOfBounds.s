./valid/runtimeErr/arrayOutOfBounds/arrayOutOfBounds.wacc
calling the reference compiler on ./valid/runtimeErr/arrayOutOfBounds/arrayOutOfBounds.wacc
-- Test: arrayOutOfBounds.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt out of bounds array access (this ought to seg fault or similar)

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin

  int[] b = [1, 2, 3] ;
	int[] a = [43, 2, 18, 1] ;
	println a[5]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayOutOfBounds.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12, %r13}
6		subq $24, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %r13, 16(%rsp)
10		movq %rsp, %rbp
11		# Stack pointer unchanged, no stack allocated variables
12		# 3 element array
13		movl $16, %edi
14		call _malloc
15		movq %rax, %r11
16		# array pointers are shifted forwards by 4 bytes (to account for size)
17		movq %r11, %r11
18		addq $4, %r11
19		movq $3, %rax
20		movl %eax, -4(%r11)
21		movq $1, %rax
22		movl %eax, (%r11)
23		movq $2, %rax
24		movl %eax, 4(%r11)
25		movq $3, %rax
26		movl %eax, 8(%r11)
27		movq %r11, %rax
28		movq %rax, %r12
29		# 4 element array
30		movl $20, %edi
31		call _malloc
32		movq %rax, %r11
33		# array pointers are shifted forwards by 4 bytes (to account for size)
34		movq %r11, %r11
35		addq $4, %r11
36		movq $4, %rax
37		movl %eax, -4(%r11)
38		movq $43, %rax
39		movl %eax, (%r11)
40		movq $2, %rax
41		movl %eax, 4(%r11)
42		movq $18, %rax
43		movl %eax, 8(%r11)
44		movq $1, %rax
45		movl %eax, 12(%r11)
46		movq %r11, %rax
47		movq %rax, %r13
48		# Stack pointer unchanged, no stack allocated arguments
49		movl $5, %r10d
50		movq %r13, %r9
51		call _arrLoad4
52		movl %r9d, %eax
53		movq %rax, %rax
54		movq %rax, %rdi
55		# statement primitives do not return results (but will clobber r0/rax)
56		call _printi
57		call _println
58		# Stack pointer unchanged, no stack allocated variables
59		movq $0, %rax
60		# popq {%rbx, %r12, %r13}
61		movq (%rsp), %rbx
62		movq 8(%rsp), %r12
63		movq 16(%rsp), %r13
64		addq $24, %rsp
65		popq %rbp
66		ret
67	
68	.section .rodata
69	# length of .L._prints_str0
70		.int 4
71	.L._prints_str0:
72		.asciz "%.*s"
73	.text
74	_prints:
75		pushq %rbp
76		movq %rsp, %rbp
77		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
78		andq $-16, %rsp
79		movq %rdi, %rdx
80		movl -4(%rdi), %esi
81		leaq .L._prints_str0(%rip), %rdi
82		# on x86, al represents the number of SIMD registers used as variadic arguments
83		movb $0, %al
84		call printf@plt
85		movq $0, %rdi
86		call fflush@plt
87		movq %rbp, %rsp
88		popq %rbp
89		ret
90	
91	_malloc:
92		pushq %rbp
93		movq %rsp, %rbp
94		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
95		andq $-16, %rsp
96		call malloc@plt
97		cmpq $0, %rax
98		je _errOutOfMemory
99		movq %rbp, %rsp
100		popq %rbp
101		ret
102	
103	.section .rodata
104	# length of .L._printi_str0
105		.int 2
106	.L._printi_str0:
107		.asciz "%d"
108	.text
109	_printi:
110		pushq %rbp
111		movq %rsp, %rbp
112		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
113		andq $-16, %rsp
114		movl %edi, %esi
115		leaq .L._printi_str0(%rip), %rdi
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
126	# length of .L._println_str0
127		.int 0
128	.L._println_str0:
129		.asciz ""
130	.text
131	_println:
132		pushq %rbp
133		movq %rsp, %rbp
134		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
135		andq $-16, %rsp
136		leaq .L._println_str0(%rip), %rdi
137		call puts@plt
138		movq $0, %rdi
139		call fflush@plt
140		movq %rbp, %rsp
141		popq %rbp
142		ret
143	
144	_exit:
145		pushq %rbp
146		movq %rsp, %rbp
147		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
148		andq $-16, %rsp
149		call exit@plt
150		movq %rbp, %rsp
151		popq %rbp
152		ret
153	
154	_arrLoad4:
155		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
156		pushq %rbx
157		cmpl $0, %r10d
158		cmovl %r10d, %esi
159		jl _errOutOfBounds
160		movl -4(%r9), %ebx
161		cmpl %ebx, %r10d
162		cmovge %r10d, %esi
163		jge _errOutOfBounds
164		movslq (%r9,%r10,4), %r9
165		popq %rbx
166		ret
167	
168	.section .rodata
169	# length of .L._errOutOfMemory_str0
170		.int 27
171	.L._errOutOfMemory_str0:
172		.asciz "fatal error: out of memory\n"
173	.text
174	_errOutOfMemory:
175		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
176		andq $-16, %rsp
177		leaq .L._errOutOfMemory_str0(%rip), %rdi
178		call _prints
179		movb $-1, %dil
180		call exit@plt
181	
182	.section .rodata
183	# length of .L._errOutOfBounds_str0
184		.int 42
185	.L._errOutOfBounds_str0:
186		.asciz "fatal error: array index %d out of bounds\n"
187	.text
188	_errOutOfBounds:
189		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
190		andq $-16, %rsp
191		leaq .L._errOutOfBounds_str0(%rip), %rdi
192		# on x86, al represents the number of SIMD registers used as variadic arguments
193		movb $0, %al
194		call printf@plt
195		movq $0, %rdi
196		call fflush@plt
197		movb $-1, %dil
198		call exit@plt
===========================================================
-- Finished

