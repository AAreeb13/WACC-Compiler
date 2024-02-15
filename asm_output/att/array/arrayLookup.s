./valid/array/arrayLookup.wacc
calling the reference compiler on ./valid/array/arrayLookup.wacc
-- Test: arrayLookup.wacc

-- Uploaded file: 
---------------------------------------------------------------
# check first element of array

# Output:
# 43
#

# Program:

begin
  int[] a = [43, 2, 18, 1] ;
  println a[0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayLookup.s contents are:
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
11		# 4 element array
12		movl $20, %edi
13		call _malloc
14		movq %rax, %r11
15		# array pointers are shifted forwards by 4 bytes (to account for size)
16		movq %r11, %r11
17		addq $4, %r11
18		movq $4, %rax
19		movl %eax, -4(%r11)
20		movq $43, %rax
21		movl %eax, (%r11)
22		movq $2, %rax
23		movl %eax, 4(%r11)
24		movq $18, %rax
25		movl %eax, 8(%r11)
26		movq $1, %rax
27		movl %eax, 12(%r11)
28		movq %r11, %rax
29		movq %rax, %r12
30		# Stack pointer unchanged, no stack allocated arguments
31		movl $0, %r10d
32		movq %r12, %r9
33		call _arrLoad4
34		movl %r9d, %eax
35		movq %rax, %rax
36		movq %rax, %rdi
37		# statement primitives do not return results (but will clobber r0/rax)
38		call _printi
39		call _println
40		# Stack pointer unchanged, no stack allocated variables
41		movq $0, %rax
42		# popq {%rbx, %r12}
43		movq (%rsp), %rbx
44		movq 8(%rsp), %r12
45		addq $16, %rsp
46		popq %rbp
47		ret
48	
49	.section .rodata
50	# length of .L._prints_str0
51		.int 4
52	.L._prints_str0:
53		.asciz "%.*s"
54	.text
55	_prints:
56		pushq %rbp
57		movq %rsp, %rbp
58		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
59		andq $-16, %rsp
60		movq %rdi, %rdx
61		movl -4(%rdi), %esi
62		leaq .L._prints_str0(%rip), %rdi
63		# on x86, al represents the number of SIMD registers used as variadic arguments
64		movb $0, %al
65		call printf@plt
66		movq $0, %rdi
67		call fflush@plt
68		movq %rbp, %rsp
69		popq %rbp
70		ret
71	
72	_malloc:
73		pushq %rbp
74		movq %rsp, %rbp
75		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
76		andq $-16, %rsp
77		call malloc@plt
78		cmpq $0, %rax
79		je _errOutOfMemory
80		movq %rbp, %rsp
81		popq %rbp
82		ret
83	
84	.section .rodata
85	# length of .L._printi_str0
86		.int 2
87	.L._printi_str0:
88		.asciz "%d"
89	.text
90	_printi:
91		pushq %rbp
92		movq %rsp, %rbp
93		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
94		andq $-16, %rsp
95		movl %edi, %esi
96		leaq .L._printi_str0(%rip), %rdi
97		# on x86, al represents the number of SIMD registers used as variadic arguments
98		movb $0, %al
99		call printf@plt
100		movq $0, %rdi
101		call fflush@plt
102		movq %rbp, %rsp
103		popq %rbp
104		ret
105	
106	.section .rodata
107	# length of .L._println_str0
108		.int 0
109	.L._println_str0:
110		.asciz ""
111	.text
112	_println:
113		pushq %rbp
114		movq %rsp, %rbp
115		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
116		andq $-16, %rsp
117		leaq .L._println_str0(%rip), %rdi
118		call puts@plt
119		movq $0, %rdi
120		call fflush@plt
121		movq %rbp, %rsp
122		popq %rbp
123		ret
124	
125	_exit:
126		pushq %rbp
127		movq %rsp, %rbp
128		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
129		andq $-16, %rsp
130		call exit@plt
131		movq %rbp, %rsp
132		popq %rbp
133		ret
134	
135	_arrLoad4:
136		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
137		pushq %rbx
138		cmpl $0, %r10d
139		cmovl %r10d, %esi
140		jl _errOutOfBounds
141		movl -4(%r9), %ebx
142		cmpl %ebx, %r10d
143		cmovge %r10d, %esi
144		jge _errOutOfBounds
145		movslq (%r9,%r10,4), %r9
146		popq %rbx
147		ret
148	
149	.section .rodata
150	# length of .L._errOutOfMemory_str0
151		.int 27
152	.L._errOutOfMemory_str0:
153		.asciz "fatal error: out of memory\n"
154	.text
155	_errOutOfMemory:
156		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
157		andq $-16, %rsp
158		leaq .L._errOutOfMemory_str0(%rip), %rdi
159		call _prints
160		movb $-1, %dil
161		call exit@plt
162	
163	.section .rodata
164	# length of .L._errOutOfBounds_str0
165		.int 42
166	.L._errOutOfBounds_str0:
167		.asciz "fatal error: array index %d out of bounds\n"
168	.text
169	_errOutOfBounds:
170		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
171		andq $-16, %rsp
172		leaq .L._errOutOfBounds_str0(%rip), %rdi
173		# on x86, al represents the number of SIMD registers used as variadic arguments
174		movb $0, %al
175		call printf@plt
176		movq $0, %rdi
177		call fflush@plt
178		movb $-1, %dil
179		call exit@plt
===========================================================
-- Finished

