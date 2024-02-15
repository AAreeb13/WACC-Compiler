./valid/pairs/nestedPair.wacc
calling the reference compiler on ./valid/pairs/nestedPair.wacc
-- Test: nestedPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair (int, pair (int, int) )

# Output:

# Program:

begin
  pair(int, int) p = newpair(2, 3) ;
  pair(int, pair) q = newpair(1, p)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
nestedPair.s contents are:
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
15		movq $2, %rax
16		movq %rax, (%r11)
17		movq $3, %rax
18		movq %rax, 8(%r11)
19		movq %r11, %rax
20		movq %rax, %r12
21		movl $16, %edi
22		call _malloc
23		movq %rax, %r11
24		movq $1, %rax
25		movq %rax, (%r11)
26		movq %r12, %rax
27		movq %rax, 8(%r11)
28		movq %r11, %rax
29		movq %rax, %r13
30		# Stack pointer unchanged, no stack allocated variables
31		movq $0, %rax
32		# popq {%rbx, %r12, %r13}
33		movq (%rsp), %rbx
34		movq 8(%rsp), %r12
35		movq 16(%rsp), %r13
36		addq $24, %rsp
37		popq %rbp
38		ret
39	
40	.section .rodata
41	# length of .L._prints_str0
42		.int 4
43	.L._prints_str0:
44		.asciz "%.*s"
45	.text
46	_prints:
47		pushq %rbp
48		movq %rsp, %rbp
49		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
50		andq $-16, %rsp
51		movq %rdi, %rdx
52		movl -4(%rdi), %esi
53		leaq .L._prints_str0(%rip), %rdi
54		# on x86, al represents the number of SIMD registers used as variadic arguments
55		movb $0, %al
56		call printf@plt
57		movq $0, %rdi
58		call fflush@plt
59		movq %rbp, %rsp
60		popq %rbp
61		ret
62	
63	_malloc:
64		pushq %rbp
65		movq %rsp, %rbp
66		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
67		andq $-16, %rsp
68		call malloc@plt
69		cmpq $0, %rax
70		je _errOutOfMemory
71		movq %rbp, %rsp
72		popq %rbp
73		ret
74	
75	_exit:
76		pushq %rbp
77		movq %rsp, %rbp
78		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
79		andq $-16, %rsp
80		call exit@plt
81		movq %rbp, %rsp
82		popq %rbp
83		ret
84	
85	.section .rodata
86	# length of .L._errOutOfMemory_str0
87		.int 27
88	.L._errOutOfMemory_str0:
89		.asciz "fatal error: out of memory\n"
90	.text
91	_errOutOfMemory:
92		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
93		andq $-16, %rsp
94		leaq .L._errOutOfMemory_str0(%rip), %rdi
95		call _prints
96		movb $-1, %dil
97		call exit@plt
===========================================================
-- Finished

