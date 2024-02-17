./valid/array/arrayLength.wacc
calling the reference compiler on ./valid/array/arrayLength.wacc
-- Test: arrayLength.wacc

-- Uploaded file: 
---------------------------------------------------------------
# check length of array

# Output:
# 4
#

# Program:

begin
  int[] a = [43, 2, 18, 1] ;
  println len a
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayLength.s contents are:
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
31		movslq -4(%r12), %rax
32		movq %rax, %rax
33		movq %rax, %rdi
34		# statement primitives do not return results (but will clobber r0/rax)
35		call _printi
36		call _println
37		# Stack pointer unchanged, no stack allocated variables
38		movq $0, %rax
39		# popq {%rbx, %r12}
40		movq (%rsp), %rbx
41		movq 8(%rsp), %r12
42		addq $16, %rsp
43		popq %rbp
44		ret
45	
46	.section .rodata
47	# length of .L._prints_str0
48		.int 4
49	.L._prints_str0:
50		.asciz "%.*s"
51	.text
52	_prints:
53		pushq %rbp
54		movq %rsp, %rbp
55		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
56		andq $-16, %rsp
57		movq %rdi, %rdx
58		movl -4(%rdi), %esi
59		leaq .L._prints_str0(%rip), %rdi
60		# on x86, al represents the number of SIMD registers used as variadic arguments
61		movb $0, %al
62		call printf@plt
63		movq $0, %rdi
64		call fflush@plt
65		movq %rbp, %rsp
66		popq %rbp
67		ret
68	
69	_malloc:
70		pushq %rbp
71		movq %rsp, %rbp
72		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
73		andq $-16, %rsp
74		call malloc@plt
75		cmpq $0, %rax
76		je _errOutOfMemory
77		movq %rbp, %rsp
78		popq %rbp
79		ret
80	
81	.section .rodata
82	# length of .L._printi_str0
83		.int 2
84	.L._printi_str0:
85		.asciz "%d"
86	.text
87	_printi:
88		pushq %rbp
89		movq %rsp, %rbp
90		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
91		andq $-16, %rsp
92		movl %edi, %esi
93		leaq .L._printi_str0(%rip), %rdi
94		# on x86, al represents the number of SIMD registers used as variadic arguments
95		movb $0, %al
96		call printf@plt
97		movq $0, %rdi
98		call fflush@plt
99		movq %rbp, %rsp
100		popq %rbp
101		ret
102	
103	.section .rodata
104	# length of .L._println_str0
105		.int 0
106	.L._println_str0:
107		.asciz ""
108	.text
109	_println:
110		pushq %rbp
111		movq %rsp, %rbp
112		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
113		andq $-16, %rsp
114		leaq .L._println_str0(%rip), %rdi
115		call puts@plt
116		movq $0, %rdi
117		call fflush@plt
118		movq %rbp, %rsp
119		popq %rbp
120		ret
121	
122	_exit:
123		pushq %rbp
124		movq %rsp, %rbp
125		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
126		andq $-16, %rsp
127		call exit@plt
128		movq %rbp, %rsp
129		popq %rbp
130		ret
131	
132	.section .rodata
133	# length of .L._errOutOfMemory_str0
134		.int 27
135	.L._errOutOfMemory_str0:
136		.asciz "fatal error: out of memory\n"
137	.text
138	_errOutOfMemory:
139		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
140		andq $-16, %rsp
141		leaq .L._errOutOfMemory_str0(%rip), %rdi
142		call _prints
143		movb $-1, %dil
144		call exit@plt
===========================================================
-- Finished

