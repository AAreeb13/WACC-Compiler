./valid/array/modifyString.wacc
calling the reference compiler on ./valid/array/modifyString.wacc
-- Test: modifyString.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create and modify string as array of characters

# Output:
# hello world!
# Hello world!
# Hi!
#

# Program:

begin
  char[] str = ['h','e','l','l','o',' ','w','o','r','l','d','!'] ;
  println str ;
  str[0] = 'H' ;
  println str ;
  str = ['H','i','!'] ;
  println str
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
modifyString.s contents are:
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
11		# 12 element array
12		movl $16, %edi
13		call _malloc
14		movq %rax, %r11
15		# array pointers are shifted forwards by 4 bytes (to account for size)
16		movq %r11, %r11
17		addq $4, %r11
18		movq $12, %rax
19		movl %eax, -4(%r11)
20		movq $104, %rax
21		movb %al, (%r11)
22		movq $101, %rax
23		movb %al, 1(%r11)
24		movq $108, %rax
25		movb %al, 2(%r11)
26		movq $108, %rax
27		movb %al, 3(%r11)
28		movq $111, %rax
29		movb %al, 4(%r11)
30		movq $32, %rax
31		movb %al, 5(%r11)
32		movq $119, %rax
33		movb %al, 6(%r11)
34		movq $111, %rax
35		movb %al, 7(%r11)
36		movq $114, %rax
37		movb %al, 8(%r11)
38		movq $108, %rax
39		movb %al, 9(%r11)
40		movq $100, %rax
41		movb %al, 10(%r11)
42		movq $33, %rax
43		movb %al, 11(%r11)
44		movq %r11, %rax
45		movq %rax, %r12
46		# Stack pointer unchanged, no stack allocated arguments
47		movq %r12, %rax
48		movq %rax, %rdi
49		# statement primitives do not return results (but will clobber r0/rax)
50		call _prints
51		call _println
52		movl $0, %r10d
53		movq $72, %rax
54		movq %r12, %r9
55		call _arrStore1
56		# Stack pointer unchanged, no stack allocated arguments
57		movq %r12, %rax
58		movq %rax, %rdi
59		# statement primitives do not return results (but will clobber r0/rax)
60		call _prints
61		call _println
62		# 3 element array
63		movl $7, %edi
64		call _malloc
65		movq %rax, %r11
66		# array pointers are shifted forwards by 4 bytes (to account for size)
67		movq %r11, %r11
68		addq $4, %r11
69		movq $3, %rax
70		movl %eax, -4(%r11)
71		movq $72, %rax
72		movb %al, (%r11)
73		movq $105, %rax
74		movb %al, 1(%r11)
75		movq $33, %rax
76		movb %al, 2(%r11)
77		movq %r11, %rax
78		movq %rax, %r12
79		# Stack pointer unchanged, no stack allocated arguments
80		movq %r12, %rax
81		movq %rax, %rdi
82		# statement primitives do not return results (but will clobber r0/rax)
83		call _prints
84		call _println
85		# Stack pointer unchanged, no stack allocated variables
86		movq $0, %rax
87		# popq {%rbx, %r12}
88		movq (%rsp), %rbx
89		movq 8(%rsp), %r12
90		addq $16, %rsp
91		popq %rbp
92		ret
93	
94	.section .rodata
95	# length of .L._prints_str0
96		.int 4
97	.L._prints_str0:
98		.asciz "%.*s"
99	.text
100	_prints:
101		pushq %rbp
102		movq %rsp, %rbp
103		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
104		andq $-16, %rsp
105		movq %rdi, %rdx
106		movl -4(%rdi), %esi
107		leaq .L._prints_str0(%rip), %rdi
108		# on x86, al represents the number of SIMD registers used as variadic arguments
109		movb $0, %al
110		call printf@plt
111		movq $0, %rdi
112		call fflush@plt
113		movq %rbp, %rsp
114		popq %rbp
115		ret
116	
117	_malloc:
118		pushq %rbp
119		movq %rsp, %rbp
120		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
121		andq $-16, %rsp
122		call malloc@plt
123		cmpq $0, %rax
124		je _errOutOfMemory
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
158	_arrStore1:
159		# Special calling convention: array ptr passed in R9, index in R10, value to store in RAX
160		pushq %rbx
161		cmpl $0, %r10d
162		cmovl %r10d, %esi
163		jl _errOutOfBounds
164		movl -4(%r9), %ebx
165		cmpl %ebx, %r10d
166		cmovge %r10d, %esi
167		jge _errOutOfBounds
168		movb %al, (%r9,%r10)
169		popq %rbx
170		ret
171	
172	.section .rodata
173	# length of .L._errOutOfMemory_str0
174		.int 27
175	.L._errOutOfMemory_str0:
176		.asciz "fatal error: out of memory\n"
177	.text
178	_errOutOfMemory:
179		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
180		andq $-16, %rsp
181		leaq .L._errOutOfMemory_str0(%rip), %rdi
182		call _prints
183		movb $-1, %dil
184		call exit@plt
185	
186	.section .rodata
187	# length of .L._errOutOfBounds_str0
188		.int 42
189	.L._errOutOfBounds_str0:
190		.asciz "fatal error: array index %d out of bounds\n"
191	.text
192	_errOutOfBounds:
193		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
194		andq $-16, %rsp
195		leaq .L._errOutOfBounds_str0(%rip), %rdi
196		# on x86, al represents the number of SIMD registers used as variadic arguments
197		movb $0, %al
198		call printf@plt
199		movq $0, %rdi
200		call fflush@plt
201		movb $-1, %dil
202		call exit@plt
===========================================================
-- Finished

