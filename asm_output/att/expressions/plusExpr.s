./valid/expressions/plusExpr.wacc
calling the reference compiler on ./valid/expressions/plusExpr.wacc
-- Test: plusExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating addition

# Output:
# 35
#

# Program:

begin
  int x = 15 ;
  int y = 20 ;
  println x + y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
plusExpr.s contents are:
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
12		movq $15, %rax
13		movq %rax, %r12
14		movq $20, %rax
15		movq %rax, %r13
16		# Stack pointer unchanged, no stack allocated arguments
17		movl %r12d, %eax
18		addl %r13d, %eax
19		jo _errOverflow
20		movslq %eax, %rax
21		pushq %rax
22		popq %rax
23		movq %rax, %rax
24		movq %rax, %rdi
25		# statement primitives do not return results (but will clobber r0/rax)
26		call _printi
27		call _println
28		# Stack pointer unchanged, no stack allocated variables
29		movq $0, %rax
30		# popq {%rbx, %r12, %r13}
31		movq (%rsp), %rbx
32		movq 8(%rsp), %r12
33		movq 16(%rsp), %r13
34		addq $24, %rsp
35		popq %rbp
36		ret
37	
38	.section .rodata
39	# length of .L._prints_str0
40		.int 4
41	.L._prints_str0:
42		.asciz "%.*s"
43	.text
44	_prints:
45		pushq %rbp
46		movq %rsp, %rbp
47		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
48		andq $-16, %rsp
49		movq %rdi, %rdx
50		movl -4(%rdi), %esi
51		leaq .L._prints_str0(%rip), %rdi
52		# on x86, al represents the number of SIMD registers used as variadic arguments
53		movb $0, %al
54		call printf@plt
55		movq $0, %rdi
56		call fflush@plt
57		movq %rbp, %rsp
58		popq %rbp
59		ret
60	
61	.section .rodata
62	# length of .L._printi_str0
63		.int 2
64	.L._printi_str0:
65		.asciz "%d"
66	.text
67	_printi:
68		pushq %rbp
69		movq %rsp, %rbp
70		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
71		andq $-16, %rsp
72		movl %edi, %esi
73		leaq .L._printi_str0(%rip), %rdi
74		# on x86, al represents the number of SIMD registers used as variadic arguments
75		movb $0, %al
76		call printf@plt
77		movq $0, %rdi
78		call fflush@plt
79		movq %rbp, %rsp
80		popq %rbp
81		ret
82	
83	.section .rodata
84	# length of .L._println_str0
85		.int 0
86	.L._println_str0:
87		.asciz ""
88	.text
89	_println:
90		pushq %rbp
91		movq %rsp, %rbp
92		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
93		andq $-16, %rsp
94		leaq .L._println_str0(%rip), %rdi
95		call puts@plt
96		movq $0, %rdi
97		call fflush@plt
98		movq %rbp, %rsp
99		popq %rbp
100		ret
101	
102	_exit:
103		pushq %rbp
104		movq %rsp, %rbp
105		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
106		andq $-16, %rsp
107		call exit@plt
108		movq %rbp, %rsp
109		popq %rbp
110		ret
111	
112	.section .rodata
113	# length of .L._errOverflow_str0
114		.int 52
115	.L._errOverflow_str0:
116		.asciz "fatal error: integer overflow or underflow occurred\n"
117	.text
118	_errOverflow:
119		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
120		andq $-16, %rsp
121		leaq .L._errOverflow_str0(%rip), %rdi
122		call _prints
123		movb $-1, %dil
124		call exit@plt
===========================================================
-- Finished

