./valid/runtimeErr/nullDereference/readNull1.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/readNull1.wacc
-- Test: readNull1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt dereference of a null pair by reading into an element of it

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	pair(int, int) p = null ;
	read fst p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
readNull1.s contents are:
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
11		movq $0, %rax
12		movq %rax, %r12
13		# Stack pointer unchanged, no stack allocated arguments
14		# load the current value in the destination of the read so it supports defaults
15		cmpq $0, %r12
16		je _errNull
17		movq (%r12), %rax
18		movq %rax, %rax
19		movq %rax, %rdi
20		call _readi
21		movq %rax, %r11
22		cmpq $0, %r12
23		je _errNull
24		movq %r11, %rax
25		movq %rax, (%r12)
26		# Stack pointer unchanged, no stack allocated variables
27		movq $0, %rax
28		# popq {%rbx, %r12}
29		movq (%rsp), %rbx
30		movq 8(%rsp), %r12
31		addq $16, %rsp
32		popq %rbp
33		ret
34	
35	.section .rodata
36	# length of .L._prints_str0
37		.int 4
38	.L._prints_str0:
39		.asciz "%.*s"
40	.text
41	_prints:
42		pushq %rbp
43		movq %rsp, %rbp
44		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
45		andq $-16, %rsp
46		movq %rdi, %rdx
47		movl -4(%rdi), %esi
48		leaq .L._prints_str0(%rip), %rdi
49		# on x86, al represents the number of SIMD registers used as variadic arguments
50		movb $0, %al
51		call printf@plt
52		movq $0, %rdi
53		call fflush@plt
54		movq %rbp, %rsp
55		popq %rbp
56		ret
57	
58	.section .rodata
59	# length of .L._readi_str0
60		.int 2
61	.L._readi_str0:
62		.asciz "%d"
63	.text
64	_readi:
65		pushq %rbp
66		movq %rsp, %rbp
67		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
68		andq $-16, %rsp
69		# RDI contains the "original" value of the destination of the read
70		# allocate space on the stack to store the read: preserve alignment!
71		# the passed default argument should be stored in case of EOF
72		subq $16, %rsp
73		movl %edi, (%rsp)
74		leaq (%rsp), %rsi
75		leaq .L._readi_str0(%rip), %rdi
76		# on x86, al represents the number of SIMD registers used as variadic arguments
77		movb $0, %al
78		call scanf@plt
79		movslq (%rsp), %rax
80		addq $16, %rsp
81		movq %rbp, %rsp
82		popq %rbp
83		ret
84	
85	_exit:
86		pushq %rbp
87		movq %rsp, %rbp
88		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
89		andq $-16, %rsp
90		call exit@plt
91		movq %rbp, %rsp
92		popq %rbp
93		ret
94	
95	.section .rodata
96	# length of .L._errNull_str0
97		.int 45
98	.L._errNull_str0:
99		.asciz "fatal error: null pair dereferenced or freed\n"
100	.text
101	_errNull:
102		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
103		andq $-16, %rsp
104		leaq .L._errNull_str0(%rip), %rdi
105		call _prints
106		movb $-1, %dil
107		call exit@plt
===========================================================
-- Finished

