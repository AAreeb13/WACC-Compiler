./valid/expressions/negDivisorDiv.wacc
calling the reference compiler on ./valid/expressions/negDivisorDiv.wacc
-- Test: negDivisorDiv.wacc

-- Uploaded file: 
---------------------------------------------------------------
# division by a negative number

# Output:
# -2
#

# Program:

begin
  int x = 4 ;
  int y = -2 ;
  println x / y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
negDivisorDiv.s contents are:
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
12		movq $4, %rax
13		movq %rax, %r12
14		movq $-2, %rax
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
32		call _println
33		# Stack pointer unchanged, no stack allocated variables
34		movq $0, %rax
35		# popq {%rbx, %r12, %r13}
36		movq (%rsp), %rbx
37		movq 8(%rsp), %r12
38		movq 16(%rsp), %r13
39		addq $24, %rsp
40		popq %rbp
41		ret
42	
43	.section .rodata
44	# length of .L._prints_str0
45		.int 4
46	.L._prints_str0:
47		.asciz "%.*s"
48	.text
49	_prints:
50		pushq %rbp
51		movq %rsp, %rbp
52		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
53		andq $-16, %rsp
54		movq %rdi, %rdx
55		movl -4(%rdi), %esi
56		leaq .L._prints_str0(%rip), %rdi
57		# on x86, al represents the number of SIMD registers used as variadic arguments
58		movb $0, %al
59		call printf@plt
60		movq $0, %rdi
61		call fflush@plt
62		movq %rbp, %rsp
63		popq %rbp
64		ret
65	
66	.section .rodata
67	# length of .L._printi_str0
68		.int 2
69	.L._printi_str0:
70		.asciz "%d"
71	.text
72	_printi:
73		pushq %rbp
74		movq %rsp, %rbp
75		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
76		andq $-16, %rsp
77		movl %edi, %esi
78		leaq .L._printi_str0(%rip), %rdi
79		# on x86, al represents the number of SIMD registers used as variadic arguments
80		movb $0, %al
81		call printf@plt
82		movq $0, %rdi
83		call fflush@plt
84		movq %rbp, %rsp
85		popq %rbp
86		ret
87	
88	.section .rodata
89	# length of .L._println_str0
90		.int 0
91	.L._println_str0:
92		.asciz ""
93	.text
94	_println:
95		pushq %rbp
96		movq %rsp, %rbp
97		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
98		andq $-16, %rsp
99		leaq .L._println_str0(%rip), %rdi
100		call puts@plt
101		movq $0, %rdi
102		call fflush@plt
103		movq %rbp, %rsp
104		popq %rbp
105		ret
106	
107	_exit:
108		pushq %rbp
109		movq %rsp, %rbp
110		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
111		andq $-16, %rsp
112		call exit@plt
113		movq %rbp, %rsp
114		popq %rbp
115		ret
116	
117	.section .rodata
118	# length of .L._errDivZero_str0
119		.int 40
120	.L._errDivZero_str0:
121		.asciz "fatal error: division or modulo by zero\n"
122	.text
123	_errDivZero:
124		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
125		andq $-16, %rsp
126		leaq .L._errDivZero_str0(%rip), %rdi
127		call _prints
128		movb $-1, %dil
129		call exit@plt
===========================================================
-- Finished

