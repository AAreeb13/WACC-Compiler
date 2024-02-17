./valid/array/arrayNested.wacc
calling the reference compiler on ./valid/array/arrayNested.wacc
-- Test: arrayNested.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array declaration and assignment

# Output:
# 3
# 3
#

# Program:

begin
  int[] a = [1,2,3];
  int[] b = [3,4];
  int[][] c = [a,b] ;
  println c[0][2] ;
  println c[1][0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayNested.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12, %r13, %r14}
6		subq $32, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %r13, 16(%rsp)
10		movq %r14, 24(%rsp)
11		movq %rsp, %rbp
12		# Stack pointer unchanged, no stack allocated variables
13		# 3 element array
14		movl $16, %edi
15		call _malloc
16		movq %rax, %r11
17		# array pointers are shifted forwards by 4 bytes (to account for size)
18		movq %r11, %r11
19		addq $4, %r11
20		movq $3, %rax
21		movl %eax, -4(%r11)
22		movq $1, %rax
23		movl %eax, (%r11)
24		movq $2, %rax
25		movl %eax, 4(%r11)
26		movq $3, %rax
27		movl %eax, 8(%r11)
28		movq %r11, %rax
29		movq %rax, %r12
30		# 2 element array
31		movl $12, %edi
32		call _malloc
33		movq %rax, %r11
34		# array pointers are shifted forwards by 4 bytes (to account for size)
35		movq %r11, %r11
36		addq $4, %r11
37		movq $2, %rax
38		movl %eax, -4(%r11)
39		movq $3, %rax
40		movl %eax, (%r11)
41		movq $4, %rax
42		movl %eax, 4(%r11)
43		movq %r11, %rax
44		movq %rax, %r13
45		# 2 element array
46		movl $20, %edi
47		call _malloc
48		movq %rax, %r11
49		# array pointers are shifted forwards by 4 bytes (to account for size)
50		movq %r11, %r11
51		addq $4, %r11
52		movq $2, %rax
53		movl %eax, -4(%r11)
54		movq %r12, %rax
55		movq %rax, (%r11)
56		movq %r13, %rax
57		movq %rax, 8(%r11)
58		movq %r11, %rax
59		movq %rax, %r14
60		# Stack pointer unchanged, no stack allocated arguments
61		movl $0, %r10d
62		movq %r14, %r9
63		call _arrLoad8
64		movq %r9, %rax
65		movq %rax, %rax
66		pushq %rax
67		movl $2, %r10d
68		popq %rax
69		movq %rax, %r9
70		call _arrLoad4
71		movl %r9d, %eax
72		movq %rax, %rax
73		movq %rax, %rdi
74		# statement primitives do not return results (but will clobber r0/rax)
75		call _printi
76		call _println
77		# Stack pointer unchanged, no stack allocated arguments
78		movl $1, %r10d
79		movq %r14, %r9
80		call _arrLoad8
81		movq %r9, %rax
82		movq %rax, %rax
83		pushq %rax
84		movl $0, %r10d
85		popq %rax
86		movq %rax, %r9
87		call _arrLoad4
88		movl %r9d, %eax
89		movq %rax, %rax
90		movq %rax, %rdi
91		# statement primitives do not return results (but will clobber r0/rax)
92		call _printi
93		call _println
94		# Stack pointer unchanged, no stack allocated variables
95		movq $0, %rax
96		# popq {%rbx, %r12, %r13, %r14}
97		movq (%rsp), %rbx
98		movq 8(%rsp), %r12
99		movq 16(%rsp), %r13
100		movq 24(%rsp), %r14
101		addq $32, %rsp
102		popq %rbp
103		ret
104	
105	.section .rodata
106	# length of .L._prints_str0
107		.int 4
108	.L._prints_str0:
109		.asciz "%.*s"
110	.text
111	_prints:
112		pushq %rbp
113		movq %rsp, %rbp
114		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
115		andq $-16, %rsp
116		movq %rdi, %rdx
117		movl -4(%rdi), %esi
118		leaq .L._prints_str0(%rip), %rdi
119		# on x86, al represents the number of SIMD registers used as variadic arguments
120		movb $0, %al
121		call printf@plt
122		movq $0, %rdi
123		call fflush@plt
124		movq %rbp, %rsp
125		popq %rbp
126		ret
127	
128	_malloc:
129		pushq %rbp
130		movq %rsp, %rbp
131		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
132		andq $-16, %rsp
133		call malloc@plt
134		cmpq $0, %rax
135		je _errOutOfMemory
136		movq %rbp, %rsp
137		popq %rbp
138		ret
139	
140	.section .rodata
141	# length of .L._printi_str0
142		.int 2
143	.L._printi_str0:
144		.asciz "%d"
145	.text
146	_printi:
147		pushq %rbp
148		movq %rsp, %rbp
149		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
150		andq $-16, %rsp
151		movl %edi, %esi
152		leaq .L._printi_str0(%rip), %rdi
153		# on x86, al represents the number of SIMD registers used as variadic arguments
154		movb $0, %al
155		call printf@plt
156		movq $0, %rdi
157		call fflush@plt
158		movq %rbp, %rsp
159		popq %rbp
160		ret
161	
162	.section .rodata
163	# length of .L._println_str0
164		.int 0
165	.L._println_str0:
166		.asciz ""
167	.text
168	_println:
169		pushq %rbp
170		movq %rsp, %rbp
171		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
172		andq $-16, %rsp
173		leaq .L._println_str0(%rip), %rdi
174		call puts@plt
175		movq $0, %rdi
176		call fflush@plt
177		movq %rbp, %rsp
178		popq %rbp
179		ret
180	
181	_exit:
182		pushq %rbp
183		movq %rsp, %rbp
184		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
185		andq $-16, %rsp
186		call exit@plt
187		movq %rbp, %rsp
188		popq %rbp
189		ret
190	
191	_arrLoad8:
192		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
193		pushq %rbx
194		cmpl $0, %r10d
195		cmovl %r10d, %esi
196		jl _errOutOfBounds
197		movl -4(%r9), %ebx
198		cmpl %ebx, %r10d
199		cmovge %r10d, %esi
200		jge _errOutOfBounds
201		movq (%r9,%r10,8), %r9
202		popq %rbx
203		ret
204	
205	_arrLoad4:
206		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
207		pushq %rbx
208		cmpl $0, %r10d
209		cmovl %r10d, %esi
210		jl _errOutOfBounds
211		movl -4(%r9), %ebx
212		cmpl %ebx, %r10d
213		cmovge %r10d, %esi
214		jge _errOutOfBounds
215		movslq (%r9,%r10,4), %r9
216		popq %rbx
217		ret
218	
219	.section .rodata
220	# length of .L._errOutOfMemory_str0
221		.int 27
222	.L._errOutOfMemory_str0:
223		.asciz "fatal error: out of memory\n"
224	.text
225	_errOutOfMemory:
226		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
227		andq $-16, %rsp
228		leaq .L._errOutOfMemory_str0(%rip), %rdi
229		call _prints
230		movb $-1, %dil
231		call exit@plt
232	
233	.section .rodata
234	# length of .L._errOutOfBounds_str0
235		.int 42
236	.L._errOutOfBounds_str0:
237		.asciz "fatal error: array index %d out of bounds\n"
238	.text
239	_errOutOfBounds:
240		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
241		andq $-16, %rsp
242		leaq .L._errOutOfBounds_str0(%rip), %rdi
243		# on x86, al represents the number of SIMD registers used as variadic arguments
244		movb $0, %al
245		call printf@plt
246		movq $0, %rdi
247		call fflush@plt
248		movb $-1, %dil
249		call exit@plt
===========================================================
-- Finished

