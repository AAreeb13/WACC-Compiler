./valid/expressions/intCalc.wacc
calling the reference compiler on ./valid/expressions/intCalc.wacc
-- Test: intCalc.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple integer calculation

# Output:
# 72
#

# Program:

begin
  int x = 42 ;
  int y = 30 ;
  int z = x + y ;
  println z
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intCalc.s contents are:
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
13		movq $42, %rax
14		movq %rax, %r12
15		movq $30, %rax
16		movq %rax, %r13
17		movl %r12d, %eax
18		addl %r13d, %eax
19		jo _errOverflow
20		movslq %eax, %rax
21		pushq %rax
22		popq %rax
23		movq %rax, %rax
24		movq %rax, %r14
25		# Stack pointer unchanged, no stack allocated arguments
26		movq %r14, %rax
27		movq %rax, %rdi
28		# statement primitives do not return results (but will clobber r0/rax)
29		call _printi
30		call _println
31		# Stack pointer unchanged, no stack allocated variables
32		movq $0, %rax
33		# popq {%rbx, %r12, %r13, %r14}
34		movq (%rsp), %rbx
35		movq 8(%rsp), %r12
36		movq 16(%rsp), %r13
37		movq 24(%rsp), %r14
38		addq $32, %rsp
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
87	.section .rodata
88	# length of .L._println_str0
89		.int 0
90	.L._println_str0:
91		.asciz ""
92	.text
93	_println:
94		pushq %rbp
95		movq %rsp, %rbp
96		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
97		andq $-16, %rsp
98		leaq .L._println_str0(%rip), %rdi
99		call puts@plt
100		movq $0, %rdi
101		call fflush@plt
102		movq %rbp, %rsp
103		popq %rbp
104		ret
105	
106	_exit:
107		pushq %rbp
108		movq %rsp, %rbp
109		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
110		andq $-16, %rsp
111		call exit@plt
112		movq %rbp, %rsp
113		popq %rbp
114		ret
115	
116	.section .rodata
117	# length of .L._errOverflow_str0
118		.int 52
119	.L._errOverflow_str0:
120		.asciz "fatal error: integer overflow or underflow occurred\n"
121	.text
122	_errOverflow:
123		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
124		andq $-16, %rsp
125		leaq .L._errOverflow_str0(%rip), %rdi
126		call _prints
127		movb $-1, %dil
128		call exit@plt
===========================================================
-- Finished

