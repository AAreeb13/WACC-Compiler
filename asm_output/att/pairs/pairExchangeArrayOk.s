./valid/pairs/pairExchangeArrayOk.wacc
calling the reference compiler on ./valid/pairs/pairExchangeArrayOk.wacc
-- Test: pairExchangeArrayOk.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Assignment is legal when assigning array (even of unknown type) in nested pair extraction
# Thanks to Kristina Zimina for catching this

# Output:

# Program:

begin
  pair(int, int) p = newpair(4, 5);
  pair(pair, int) q = newpair(p, 6);
  fst fst q = []
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
pairExchangeArrayOk.s contents are:
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
12		movl $16, %edi
13		call _malloc
14		movq %rax, %r11
15		movq $4, %rax
16		movq %rax, (%r11)
17		movq $5, %rax
18		movq %rax, 8(%r11)
19		movq %r11, %rax
20		movq %rax, %r12
21		movl $16, %edi
22		call _malloc
23		movq %rax, %r11
24		movq %r12, %rax
25		movq %rax, (%r11)
26		movq $6, %rax
27		movq %rax, 8(%r11)
28		movq %r11, %rax
29		movq %rax, %r13
30		# 0 element array
31		movl $4, %edi
32		call _malloc
33		movq %rax, %r11
34		# array pointers are shifted forwards by 4 bytes (to account for size)
35		movq %r11, %r11
36		addq $4, %r11
37		movq $0, %rax
38		movl %eax, -4(%r11)
39		cmpq $0, %r13
40		je _errNull
41		movq (%r13), %rax
42		movq %rax, %rax
43		pushq %rax
44		popq %rbx
45		cmpq $0, %rbx
46		je _errNull
47		movq %r11, %rax
48		movq %rax, (%rbx)
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
94	_exit:
95		pushq %rbp
96		movq %rsp, %rbp
97		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
98		andq $-16, %rsp
99		call exit@plt
100		movq %rbp, %rsp
101		popq %rbp
102		ret
103	
104	.section .rodata
105	# length of .L._errOutOfMemory_str0
106		.int 27
107	.L._errOutOfMemory_str0:
108		.asciz "fatal error: out of memory\n"
109	.text
110	_errOutOfMemory:
111		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
112		andq $-16, %rsp
113		leaq .L._errOutOfMemory_str0(%rip), %rdi
114		call _prints
115		movb $-1, %dil
116		call exit@plt
117	
118	.section .rodata
119	# length of .L._errNull_str0
120		.int 45
121	.L._errNull_str0:
122		.asciz "fatal error: null pair dereferenced or freed\n"
123	.text
124	_errNull:
125		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
126		andq $-16, %rsp
127		leaq .L._errNull_str0(%rip), %rdi
128		call _prints
129		movb $-1, %dil
130		call exit@plt
===========================================================
-- Finished

