./valid/array/arraySimple.wacc
calling the reference compiler on ./valid/array/arraySimple.wacc
-- Test: arraySimple.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple array assignment and lookup

# Output:
# 42
#

# Program:

begin
  int[] a = [0] ;
  a[0] = 42 ;
  println a[0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arraySimple.s contents are:
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
11		# 1 element array
12		movl $8, %edi
13		call _malloc
14		movq %rax, %r11
15		# array pointers are shifted forwards by 4 bytes (to account for size)
16		movq %r11, %r11
17		addq $4, %r11
18		movq $1, %rax
19		movl %eax, -4(%r11)
20		movq $0, %rax
21		movl %eax, (%r11)
22		movq %r11, %rax
23		movq %rax, %r12
24		movl $0, %r10d
25		movq $42, %rax
26		movq %r12, %r9
27		call _arrStore4
28		# Stack pointer unchanged, no stack allocated arguments
29		movl $0, %r10d
30		movq %r12, %r9
31		call _arrLoad4
32		movl %r9d, %eax
33		movq %rax, %rax
34		movq %rax, %rdi
35		# statement primitives do not return results (but will clobber r0/rax)
36		call _printi
37		call _println
38		# Stack pointer unchanged, no stack allocated variables
39		movq $0, %rax
40		# popq {%rbx, %r12}
41		movq (%rsp), %rbx
42		movq 8(%rsp), %r12
43		addq $16, %rsp
44		popq %rbp
45		ret
46	
47	.section .rodata
48	# length of .L._prints_str0
49		.int 4
50	.L._prints_str0:
51		.asciz "%.*s"
52	.text
53	_prints:
54		pushq %rbp
55		movq %rsp, %rbp
56		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
57		andq $-16, %rsp
58		movq %rdi, %rdx
59		movl -4(%rdi), %esi
60		leaq .L._prints_str0(%rip), %rdi
61		# on x86, al represents the number of SIMD registers used as variadic arguments
62		movb $0, %al
63		call printf@plt
64		movq $0, %rdi
65		call fflush@plt
66		movq %rbp, %rsp
67		popq %rbp
68		ret
69	
70	_malloc:
71		pushq %rbp
72		movq %rsp, %rbp
73		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
74		andq $-16, %rsp
75		call malloc@plt
76		cmpq $0, %rax
77		je _errOutOfMemory
78		movq %rbp, %rsp
79		popq %rbp
80		ret
81	
82	.section .rodata
83	# length of .L._printi_str0
84		.int 2
85	.L._printi_str0:
86		.asciz "%d"
87	.text
88	_printi:
89		pushq %rbp
90		movq %rsp, %rbp
91		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
92		andq $-16, %rsp
93		movl %edi, %esi
94		leaq .L._printi_str0(%rip), %rdi
95		# on x86, al represents the number of SIMD registers used as variadic arguments
96		movb $0, %al
97		call printf@plt
98		movq $0, %rdi
99		call fflush@plt
100		movq %rbp, %rsp
101		popq %rbp
102		ret
103	
104	.section .rodata
105	# length of .L._println_str0
106		.int 0
107	.L._println_str0:
108		.asciz ""
109	.text
110	_println:
111		pushq %rbp
112		movq %rsp, %rbp
113		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
114		andq $-16, %rsp
115		leaq .L._println_str0(%rip), %rdi
116		call puts@plt
117		movq $0, %rdi
118		call fflush@plt
119		movq %rbp, %rsp
120		popq %rbp
121		ret
122	
123	_exit:
124		pushq %rbp
125		movq %rsp, %rbp
126		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
127		andq $-16, %rsp
128		call exit@plt
129		movq %rbp, %rsp
130		popq %rbp
131		ret
132	
133	_arrStore4:
134		# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
135		pushq %rbx
136		cmpl $0, %r10d
137		cmovl %r10d, %esi
138		jl _errOutOfBounds
139		movl -4(%r9), %ebx
140		cmpl %ebx, %r10d
141		cmovge %r10d, %esi
142		jge _errOutOfBounds
143		movl %eax, (%r9,%r10,4)
144		popq %rbx
145		ret
146	
147	_arrLoad4:
148		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
149		pushq %rbx
150		cmpl $0, %r10d
151		cmovl %r10d, %esi
152		jl _errOutOfBounds
153		movl -4(%r9), %ebx
154		cmpl %ebx, %r10d
155		cmovge %r10d, %esi
156		jge _errOutOfBounds
157		movslq (%r9,%r10,4), %r9
158		popq %rbx
159		ret
160	
161	.section .rodata
162	# length of .L._errOutOfMemory_str0
163		.int 27
164	.L._errOutOfMemory_str0:
165		.asciz "fatal error: out of memory\n"
166	.text
167	_errOutOfMemory:
168		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
169		andq $-16, %rsp
170		leaq .L._errOutOfMemory_str0(%rip), %rdi
171		call _prints
172		movb $-1, %dil
173		call exit@plt
174	
175	.section .rodata
176	# length of .L._errOutOfBounds_str0
177		.int 42
178	.L._errOutOfBounds_str0:
179		.asciz "fatal error: array index %d out of bounds\n"
180	.text
181	_errOutOfBounds:
182		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
183		andq $-16, %rsp
184		leaq .L._errOutOfBounds_str0(%rip), %rdi
185		# on x86, al represents the number of SIMD registers used as variadic arguments
186		movb $0, %al
187		call printf@plt
188		movq $0, %rdi
189		call fflush@plt
190		movb $-1, %dil
191		call exit@plt
===========================================================
-- Finished

