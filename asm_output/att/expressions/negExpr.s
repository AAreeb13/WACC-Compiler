./valid/expressions/negExpr.wacc
calling the reference compiler on ./valid/expressions/negExpr.wacc
-- Test: negExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating negation

# Output:
# -42
#

# Program:

begin
	int x = 42 ;
  println -x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
negExpr.s contents are:
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
11		movq $42, %rax
12		movq %rax, %r12
13		# Stack pointer unchanged, no stack allocated arguments
14		movl $0, %eax
15		subl %r12d, %eax
16		jo _errOverflow
17		movslq %eax, %rax
18		pushq %rax
19		popq %rax
20		movq %rax, %rax
21		movq %rax, %rdi
22		# statement primitives do not return results (but will clobber r0/rax)
23		call _printi
24		call _println
25		# Stack pointer unchanged, no stack allocated variables
26		movq $0, %rax
27		# popq {%rbx, %r12}
28		movq (%rsp), %rbx
29		movq 8(%rsp), %r12
30		addq $16, %rsp
31		popq %rbp
32		ret
33	
34	.section .rodata
35	# length of .L._prints_str0
36		.int 4
37	.L._prints_str0:
38		.asciz "%.*s"
39	.text
40	_prints:
41		pushq %rbp
42		movq %rsp, %rbp
43		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
44		andq $-16, %rsp
45		movq %rdi, %rdx
46		movl -4(%rdi), %esi
47		leaq .L._prints_str0(%rip), %rdi
48		# on x86, al represents the number of SIMD registers used as variadic arguments
49		movb $0, %al
50		call printf@plt
51		movq $0, %rdi
52		call fflush@plt
53		movq %rbp, %rsp
54		popq %rbp
55		ret
56	
57	.section .rodata
58	# length of .L._printi_str0
59		.int 2
60	.L._printi_str0:
61		.asciz "%d"
62	.text
63	_printi:
64		pushq %rbp
65		movq %rsp, %rbp
66		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
67		andq $-16, %rsp
68		movl %edi, %esi
69		leaq .L._printi_str0(%rip), %rdi
70		# on x86, al represents the number of SIMD registers used as variadic arguments
71		movb $0, %al
72		call printf@plt
73		movq $0, %rdi
74		call fflush@plt
75		movq %rbp, %rsp
76		popq %rbp
77		ret
78	
79	.section .rodata
80	# length of .L._println_str0
81		.int 0
82	.L._println_str0:
83		.asciz ""
84	.text
85	_println:
86		pushq %rbp
87		movq %rsp, %rbp
88		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
89		andq $-16, %rsp
90		leaq .L._println_str0(%rip), %rdi
91		call puts@plt
92		movq $0, %rdi
93		call fflush@plt
94		movq %rbp, %rsp
95		popq %rbp
96		ret
97	
98	_exit:
99		pushq %rbp
100		movq %rsp, %rbp
101		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
102		andq $-16, %rsp
103		call exit@plt
104		movq %rbp, %rsp
105		popq %rbp
106		ret
107	
108	.section .rodata
109	# length of .L._errOverflow_str0
110		.int 52
111	.L._errOverflow_str0:
112		.asciz "fatal error: integer overflow or underflow occurred\n"
113	.text
114	_errOverflow:
115		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
116		andq $-16, %rsp
117		leaq .L._errOverflow_str0(%rip), %rdi
118		call _prints
119		movb $-1, %dil
120		call exit@plt
===========================================================
-- Finished

