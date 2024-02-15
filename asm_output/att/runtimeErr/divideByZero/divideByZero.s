./valid/runtimeErr/divideByZero/divideByZero.wacc
calling the reference compiler on ./valid/runtimeErr/divideByZero/divideByZero.wacc
-- Test: divideByZero.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt divide by zero

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	int x = 10 ;
	int y = 0 ;
	print x / y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
divideByZero.s contents are:
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
12		movq $10, %rax
13		movq %rax, %r12
14		movq $0, %rax
15		movq %rax, %r13
16		# Stack pointer unchanged, no stack allocated arguments
17		movl %r12d, %eax
18		cmpl $0, %r13d
19		je _errDivZero
20		# sign extend EAX into EDX
21		cltd
22		idivl %r13d
23		movl %eax, %eax
24		movl %eax, %eax
25		movslq %eax, %rax
26		pushq %rax
27		popq %rax
28		movq %rax, %rax
29		movq %rax, %rdi
30		# statement primitives do not return results (but will clobber r0/rax)
31		call _printi
32		# Stack pointer unchanged, no stack allocated variables
33		movq $0, %rax
34		# popq {%rbx, %r12, %r13}
35		movq (%rsp), %rbx
36		movq 8(%rsp), %r12
37		movq 16(%rsp), %r13
38		addq $24, %rsp
39		popq %rbp
40		ret
41	
42	.section .rodata
43	# length of .L._prints_str0
44		.int 4
45	.L._prints_str0:
46		.asciz "%.*s"
47	.text
48	_prints:
49		pushq %rbp
50		movq %rsp, %rbp
51		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
52		andq $-16, %rsp
53		movq %rdi, %rdx
54		movl -4(%rdi), %esi
55		leaq .L._prints_str0(%rip), %rdi
56		# on x86, al represents the number of SIMD registers used as variadic arguments
57		movb $0, %al
58		call printf@plt
59		movq $0, %rdi
60		call fflush@plt
61		movq %rbp, %rsp
62		popq %rbp
63		ret
64	
65	.section .rodata
66	# length of .L._printi_str0
67		.int 2
68	.L._printi_str0:
69		.asciz "%d"
70	.text
71	_printi:
72		pushq %rbp
73		movq %rsp, %rbp
74		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
75		andq $-16, %rsp
76		movl %edi, %esi
77		leaq .L._printi_str0(%rip), %rdi
78		# on x86, al represents the number of SIMD registers used as variadic arguments
79		movb $0, %al
80		call printf@plt
81		movq $0, %rdi
82		call fflush@plt
83		movq %rbp, %rsp
84		popq %rbp
85		ret
86	
87	_exit:
88		pushq %rbp
89		movq %rsp, %rbp
90		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
91		andq $-16, %rsp
92		call exit@plt
93		movq %rbp, %rsp
94		popq %rbp
95		ret
96	
97	.section .rodata
98	# length of .L._errDivZero_str0
99		.int 40
100	.L._errDivZero_str0:
101		.asciz "fatal error: division or modulo by zero\n"
102	.text
103	_errDivZero:
104		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
105		andq $-16, %rsp
106		leaq .L._errDivZero_str0(%rip), %rdi
107		call _prints
108		movb $-1, %dil
109		call exit@plt
===========================================================
-- Finished

