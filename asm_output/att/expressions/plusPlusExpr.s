./valid/expressions/plusPlusExpr.wacc
calling the reference compiler on ./valid/expressions/plusPlusExpr.wacc
-- Test: plusPlusExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# ++ should be recognised as two separate symbols

# Output:
# 3
#

# Program:

begin
  println 1++2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
plusPlusExpr.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		pushq %rbx
6		movq %rsp, %rbp
7		# Stack pointer unchanged, no stack allocated arguments
8		movl $1, %eax
9		addl $2, %eax
10		jo _errOverflow
11		movslq %eax, %rax
12		pushq %rax
13		popq %rax
14		movq %rax, %rax
15		movq %rax, %rdi
16		# statement primitives do not return results (but will clobber r0/rax)
17		call _printi
18		call _println
19		movq $0, %rax
20		popq %rbx
21		popq %rbp
22		ret
23	
24	.section .rodata
25	# length of .L._prints_str0
26		.int 4
27	.L._prints_str0:
28		.asciz "%.*s"
29	.text
30	_prints:
31		pushq %rbp
32		movq %rsp, %rbp
33		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
34		andq $-16, %rsp
35		movq %rdi, %rdx
36		movl -4(%rdi), %esi
37		leaq .L._prints_str0(%rip), %rdi
38		# on x86, al represents the number of SIMD registers used as variadic arguments
39		movb $0, %al
40		call printf@plt
41		movq $0, %rdi
42		call fflush@plt
43		movq %rbp, %rsp
44		popq %rbp
45		ret
46	
47	.section .rodata
48	# length of .L._printi_str0
49		.int 2
50	.L._printi_str0:
51		.asciz "%d"
52	.text
53	_printi:
54		pushq %rbp
55		movq %rsp, %rbp
56		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
57		andq $-16, %rsp
58		movl %edi, %esi
59		leaq .L._printi_str0(%rip), %rdi
60		# on x86, al represents the number of SIMD registers used as variadic arguments
61		movb $0, %al
62		call printf@plt
63		movq $0, %rdi
64		call fflush@plt
65		movq %rbp, %rsp
66		popq %rbp
67		ret
68	
69	.section .rodata
70	# length of .L._println_str0
71		.int 0
72	.L._println_str0:
73		.asciz ""
74	.text
75	_println:
76		pushq %rbp
77		movq %rsp, %rbp
78		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
79		andq $-16, %rsp
80		leaq .L._println_str0(%rip), %rdi
81		call puts@plt
82		movq $0, %rdi
83		call fflush@plt
84		movq %rbp, %rsp
85		popq %rbp
86		ret
87	
88	_exit:
89		pushq %rbp
90		movq %rsp, %rbp
91		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
92		andq $-16, %rsp
93		call exit@plt
94		movq %rbp, %rsp
95		popq %rbp
96		ret
97	
98	.section .rodata
99	# length of .L._errOverflow_str0
100		.int 52
101	.L._errOverflow_str0:
102		.asciz "fatal error: integer overflow or underflow occurred\n"
103	.text
104	_errOverflow:
105		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
106		andq $-16, %rsp
107		leaq .L._errOverflow_str0(%rip), %rdi
108		call _prints
109		movb $-1, %dil
110		call exit@plt
===========================================================
-- Finished

