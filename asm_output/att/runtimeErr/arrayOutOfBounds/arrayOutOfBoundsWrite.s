./valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc
calling the reference compiler on ./valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc
-- Test: arrayOutOfBoundsWrite.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt write out of array bounds (gods, this really should not work!)

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin

  int[] b = [1, 2, 3] ;
	int[] a = [43, 2, 18, 1] ;
	a[5] = 100 ;
  println a[5]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayOutOfBoundsWrite.s contents are:
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
48		movl $5, %r10d
49		movq $100, %rax
50		movq %r13, %r9
51		call _arrStore4
52		# Stack pointer unchanged, no stack allocated arguments
53		movl $5, %r10d
54		movq %r13, %r9
55		call _arrLoad4
56		movl %r9d, %eax
57		movq %rax, %rax
58		movq %rax, %rdi
59		# statement primitives do not return results (but will clobber r0/rax)
60		call _printi
61		call _println
62		# Stack pointer unchanged, no stack allocated variables
63		movq $0, %rax
64		# popq {%rbx, %r12, %r13}
65		movq (%rsp), %rbx
66		movq 8(%rsp), %r12
67		movq 16(%rsp), %r13
68		addq $24, %rsp
69		popq %rbp
70		ret
71	
72	.section .rodata
73	# length of .L._prints_str0
74		.int 4
75	.L._prints_str0:
76		.asciz "%.*s"
77	.text
78	_prints:
79		pushq %rbp
80		movq %rsp, %rbp
81		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
82		andq $-16, %rsp
83		movq %rdi, %rdx
84		movl -4(%rdi), %esi
85		leaq .L._prints_str0(%rip), %rdi
86		# on x86, al represents the number of SIMD registers used as variadic arguments
87		movb $0, %al
88		call printf@plt
89		movq $0, %rdi
90		call fflush@plt
91		movq %rbp, %rsp
92		popq %rbp
93		ret
94	
95	_malloc:
96		pushq %rbp
97		movq %rsp, %rbp
98		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
99		andq $-16, %rsp
100		call malloc@plt
101		cmpq $0, %rax
102		je _errOutOfMemory
103		movq %rbp, %rsp
104		popq %rbp
105		ret
106	
107	.section .rodata
108	# length of .L._printi_str0
109		.int 2
110	.L._printi_str0:
111		.asciz "%d"
112	.text
113	_printi:
114		pushq %rbp
115		movq %rsp, %rbp
116		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
117		andq $-16, %rsp
118		movl %edi, %esi
119		leaq .L._printi_str0(%rip), %rdi
120		# on x86, al represents the number of SIMD registers used as variadic arguments
121		movb $0, %al
122		call printf@plt
123		movq $0, %rdi
124		call fflush@plt
125		movq %rbp, %rsp
126		popq %rbp
127		ret
128	
129	.section .rodata
130	# length of .L._println_str0
131		.int 0
132	.L._println_str0:
133		.asciz ""
134	.text
135	_println:
136		pushq %rbp
137		movq %rsp, %rbp
138		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
139		andq $-16, %rsp
140		leaq .L._println_str0(%rip), %rdi
141		call puts@plt
142		movq $0, %rdi
143		call fflush@plt
144		movq %rbp, %rsp
145		popq %rbp
146		ret
147	
148	_exit:
149		pushq %rbp
150		movq %rsp, %rbp
151		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
152		andq $-16, %rsp
153		call exit@plt
154		movq %rbp, %rsp
155		popq %rbp
156		ret
157	
158	_arrStore4:
159		# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
160		pushq %rbx
161		cmpl $0, %r10d
162		cmovl %r10d, %esi
163		jl _errOutOfBounds
164		movl -4(%r9), %ebx
165		cmpl %ebx, %r10d
166		cmovge %r10d, %esi
167		jge _errOutOfBounds
168		movl %eax, (%r9,%r10,4)
169		popq %rbx
170		ret
171	
172	_arrLoad4:
173		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
174		pushq %rbx
175		cmpl $0, %r10d
176		cmovl %r10d, %esi
177		jl _errOutOfBounds
178		movl -4(%r9), %ebx
179		cmpl %ebx, %r10d
180		cmovge %r10d, %esi
181		jge _errOutOfBounds
182		movslq (%r9,%r10,4), %r9
183		popq %rbx
184		ret
185	
186	.section .rodata
187	# length of .L._errOutOfMemory_str0
188		.int 27
189	.L._errOutOfMemory_str0:
190		.asciz "fatal error: out of memory\n"
191	.text
192	_errOutOfMemory:
193		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
194		andq $-16, %rsp
195		leaq .L._errOutOfMemory_str0(%rip), %rdi
196		call _prints
197		movb $-1, %dil
198		call exit@plt
199	
200	.section .rodata
201	# length of .L._errOutOfBounds_str0
202		.int 42
203	.L._errOutOfBounds_str0:
204		.asciz "fatal error: array index %d out of bounds\n"
205	.text
206	_errOutOfBounds:
207		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
208		andq $-16, %rsp
209		leaq .L._errOutOfBounds_str0(%rip), %rdi
210		# on x86, al represents the number of SIMD registers used as variadic arguments
211		movb $0, %al
212		call printf@plt
213		movq $0, %rdi
214		call fflush@plt
215		movb $-1, %dil
216		call exit@plt
===========================================================
-- Finished

