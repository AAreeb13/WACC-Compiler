./valid/array/lenArrayIndex.wacc
calling the reference compiler on ./valid/array/lenArrayIndex.wacc
-- Test: lenArrayIndex.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Tests that array length works on array indexes
# Thanks to Nixon Enraght-Moony for his help in this test-case

# Output:
# 0

# Program:

begin
  int[] arr = [];
  int[][] arrs = [arr];
  print len arrs[0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
lenArrayIndex.s contents are:
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
12		# 0 element array
13		movl $4, %edi
14		call _malloc
15		movq %rax, %r11
16		# array pointers are shifted forwards by 4 bytes (to account for size)
17		movq %r11, %r11
18		addq $4, %r11
19		movq $0, %rax
20		movl %eax, -4(%r11)
21		movq %r11, %rax
22		movq %rax, %r12
23		# 1 element array
24		movl $12, %edi
25		call _malloc
26		movq %rax, %r11
27		# array pointers are shifted forwards by 4 bytes (to account for size)
28		movq %r11, %r11
29		addq $4, %r11
30		movq $1, %rax
31		movl %eax, -4(%r11)
32		movq %r12, %rax
33		movq %rax, (%r11)
34		movq %r11, %rax
35		movq %rax, %r13
36		# Stack pointer unchanged, no stack allocated arguments
37		movl $0, %r10d
38		movq %r13, %r9
39		call _arrLoad8
40		movq %r9, %rax
41		movq %rax, %rax
42		pushq %rax
43		popq %rax
44		movslq -4(%rax), %rax
45		movq %rax, %rax
46		movq %rax, %rdi
47		# statement primitives do not return results (but will clobber r0/rax)
48		call _printi
49		# Stack pointer unchanged, no stack allocated variables
50		movq $0, %rax
51		# popq {%rbx, %r12, %r13}
52		movq (%rsp), %rbx
53		movq 8(%rsp), %r12
54		movq 16(%rsp), %r13
55		addq $24, %rsp
56		popq %rbp
57		ret
58	
59	.section .rodata
60	# length of .L._prints_str0
61		.int 4
62	.L._prints_str0:
63		.asciz "%.*s"
64	.text
65	_prints:
66		pushq %rbp
67		movq %rsp, %rbp
68		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
69		andq $-16, %rsp
70		movq %rdi, %rdx
71		movl -4(%rdi), %esi
72		leaq .L._prints_str0(%rip), %rdi
73		# on x86, al represents the number of SIMD registers used as variadic arguments
74		movb $0, %al
75		call printf@plt
76		movq $0, %rdi
77		call fflush@plt
78		movq %rbp, %rsp
79		popq %rbp
80		ret
81	
82	_malloc:
83		pushq %rbp
84		movq %rsp, %rbp
85		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
86		andq $-16, %rsp
87		call malloc@plt
88		cmpq $0, %rax
89		je _errOutOfMemory
90		movq %rbp, %rsp
91		popq %rbp
92		ret
93	
94	.section .rodata
95	# length of .L._printi_str0
96		.int 2
97	.L._printi_str0:
98		.asciz "%d"
99	.text
100	_printi:
101		pushq %rbp
102		movq %rsp, %rbp
103		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
104		andq $-16, %rsp
105		movl %edi, %esi
106		leaq .L._printi_str0(%rip), %rdi
107		# on x86, al represents the number of SIMD registers used as variadic arguments
108		movb $0, %al
109		call printf@plt
110		movq $0, %rdi
111		call fflush@plt
112		movq %rbp, %rsp
113		popq %rbp
114		ret
115	
116	_exit:
117		pushq %rbp
118		movq %rsp, %rbp
119		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
120		andq $-16, %rsp
121		call exit@plt
122		movq %rbp, %rsp
123		popq %rbp
124		ret
125	
126	_arrLoad8:
127		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
128		pushq %rbx
129		cmpl $0, %r10d
130		cmovl %r10d, %esi
131		jl _errOutOfBounds
132		movl -4(%r9), %ebx
133		cmpl %ebx, %r10d
134		cmovge %r10d, %esi
135		jge _errOutOfBounds
136		movq (%r9,%r10,8), %r9
137		popq %rbx
138		ret
139	
140	.section .rodata
141	# length of .L._errOutOfMemory_str0
142		.int 27
143	.L._errOutOfMemory_str0:
144		.asciz "fatal error: out of memory\n"
145	.text
146	_errOutOfMemory:
147		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
148		andq $-16, %rsp
149		leaq .L._errOutOfMemory_str0(%rip), %rdi
150		call _prints
151		movb $-1, %dil
152		call exit@plt
153	
154	.section .rodata
155	# length of .L._errOutOfBounds_str0
156		.int 42
157	.L._errOutOfBounds_str0:
158		.asciz "fatal error: array index %d out of bounds\n"
159	.text
160	_errOutOfBounds:
161		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
162		andq $-16, %rsp
163		leaq .L._errOutOfBounds_str0(%rip), %rdi
164		# on x86, al represents the number of SIMD registers used as variadic arguments
165		movb $0, %al
166		call printf@plt
167		movq $0, %rdi
168		call fflush@plt
169		movb $-1, %dil
170		call exit@plt
===========================================================
-- Finished

