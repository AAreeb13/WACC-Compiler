./valid/array/emptyArrayReplace.wacc
calling the reference compiler on ./valid/array/emptyArrayReplace.wacc
-- Test: emptyArrayReplace.wacc

-- Uploaded file: 
---------------------------------------------------------------
# it should be possible to reassign to an array

# Output:
# true
#

# Program:

begin
  int[] x = [] ;
  x = [0] ;
  println true
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayReplace.s contents are:
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
11		# 0 element array
12		movl $4, %edi
13		call _malloc
14		movq %rax, %r11
15		# array pointers are shifted forwards by 4 bytes (to account for size)
16		movq %r11, %r11
17		addq $4, %r11
18		movq $0, %rax
19		movl %eax, -4(%r11)
20		movq %r11, %rax
21		movq %rax, %r12
22		# 1 element array
23		movl $8, %edi
24		call _malloc
25		movq %rax, %r11
26		# array pointers are shifted forwards by 4 bytes (to account for size)
27		movq %r11, %r11
28		addq $4, %r11
29		movq $1, %rax
30		movl %eax, -4(%r11)
31		movq $0, %rax
32		movl %eax, (%r11)
33		movq %r11, %rax
34		movq %rax, %r12
35		# Stack pointer unchanged, no stack allocated arguments
36		movq $1, %rax
37		movq %rax, %rdi
38		# statement primitives do not return results (but will clobber r0/rax)
39		call _printb
40		call _println
41		# Stack pointer unchanged, no stack allocated variables
42		movq $0, %rax
43		# popq {%rbx, %r12}
44		movq (%rsp), %rbx
45		movq 8(%rsp), %r12
46		addq $16, %rsp
47		popq %rbp
48		ret
49	
50	.section .rodata
51	# length of .L._prints_str0
52		.int 4
53	.L._prints_str0:
54		.asciz "%.*s"
55	.text
56	_prints:
57		pushq %rbp
58		movq %rsp, %rbp
59		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
60		andq $-16, %rsp
61		movq %rdi, %rdx
62		movl -4(%rdi), %esi
63		leaq .L._prints_str0(%rip), %rdi
64		# on x86, al represents the number of SIMD registers used as variadic arguments
65		movb $0, %al
66		call printf@plt
67		movq $0, %rdi
68		call fflush@plt
69		movq %rbp, %rsp
70		popq %rbp
71		ret
72	
73	_malloc:
74		pushq %rbp
75		movq %rsp, %rbp
76		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
77		andq $-16, %rsp
78		call malloc@plt
79		cmpq $0, %rax
80		je _errOutOfMemory
81		movq %rbp, %rsp
82		popq %rbp
83		ret
84	
85	.section .rodata
86	# length of .L._printb_str0
87		.int 5
88	.L._printb_str0:
89		.asciz "false"
90	# length of .L._printb_str1
91		.int 4
92	.L._printb_str1:
93		.asciz "true"
94	# length of .L._printb_str2
95		.int 4
96	.L._printb_str2:
97		.asciz "%.*s"
98	.text
99	_printb:
100		pushq %rbp
101		movq %rsp, %rbp
102		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
103		andq $-16, %rsp
104		cmpb $0, %dil
105		jne .L_printb0
106		leaq .L._printb_str0(%rip), %rdx
107		jmp .L_printb1
108	.L_printb0:
109		leaq .L._printb_str1(%rip), %rdx
110	.L_printb1:
111		movl -4(%rdx), %esi
112		leaq .L._printb_str2(%rip), %rdi
113		# on x86, al represents the number of SIMD registers used as variadic arguments
114		movb $0, %al
115		call printf@plt
116		movq $0, %rdi
117		call fflush@plt
118		movq %rbp, %rsp
119		popq %rbp
120		ret
121	
122	.section .rodata
123	# length of .L._println_str0
124		.int 0
125	.L._println_str0:
126		.asciz ""
127	.text
128	_println:
129		pushq %rbp
130		movq %rsp, %rbp
131		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
132		andq $-16, %rsp
133		leaq .L._println_str0(%rip), %rdi
134		call puts@plt
135		movq $0, %rdi
136		call fflush@plt
137		movq %rbp, %rsp
138		popq %rbp
139		ret
140	
141	_exit:
142		pushq %rbp
143		movq %rsp, %rbp
144		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
145		andq $-16, %rsp
146		call exit@plt
147		movq %rbp, %rsp
148		popq %rbp
149		ret
150	
151	.section .rodata
152	# length of .L._errOutOfMemory_str0
153		.int 27
154	.L._errOutOfMemory_str0:
155		.asciz "fatal error: out of memory\n"
156	.text
157	_errOutOfMemory:
158		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
159		andq $-16, %rsp
160		leaq .L._errOutOfMemory_str0(%rip), %rdi
161		call _prints
162		movb $-1, %dil
163		call exit@plt
===========================================================
-- Finished

