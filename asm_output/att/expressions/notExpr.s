./valid/expressions/notExpr.wacc
calling the reference compiler on ./valid/expressions/notExpr.wacc
-- Test: notExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating not

# Output:
# false
# true
#

# Program:

begin
  bool a = true ;
  bool b = false ;
  println !a ;
  println !b
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
notExpr.s contents are:
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
12		movq $1, %rax
13		movq %rax, %r12
14		movq $0, %rax
15		movq %rax, %r13
16		# Stack pointer unchanged, no stack allocated arguments
17		cmpq $1, %r12
18		setne %al
19		movsbq %al, %rax
20		pushq %rax
21		popq %rax
22		movq %rax, %rax
23		movq %rax, %rdi
24		# statement primitives do not return results (but will clobber r0/rax)
25		call _printb
26		call _println
27		# Stack pointer unchanged, no stack allocated arguments
28		cmpq $1, %r13
29		setne %al
30		movsbq %al, %rax
31		pushq %rax
32		popq %rax
33		movq %rax, %rax
34		movq %rax, %rdi
35		# statement primitives do not return results (but will clobber r0/rax)
36		call _printb
37		call _println
38		# Stack pointer unchanged, no stack allocated variables
39		movq $0, %rax
40		# popq {%rbx, %r12, %r13}
41		movq (%rsp), %rbx
42		movq 8(%rsp), %r12
43		movq 16(%rsp), %r13
44		addq $24, %rsp
45		popq %rbp
46		ret
47	
48	.section .rodata
49	# length of .L._printb_str0
50		.int 5
51	.L._printb_str0:
52		.asciz "false"
53	# length of .L._printb_str1
54		.int 4
55	.L._printb_str1:
56		.asciz "true"
57	# length of .L._printb_str2
58		.int 4
59	.L._printb_str2:
60		.asciz "%.*s"
61	.text
62	_printb:
63		pushq %rbp
64		movq %rsp, %rbp
65		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
66		andq $-16, %rsp
67		cmpb $0, %dil
68		jne .L_printb0
69		leaq .L._printb_str0(%rip), %rdx
70		jmp .L_printb1
71	.L_printb0:
72		leaq .L._printb_str1(%rip), %rdx
73	.L_printb1:
74		movl -4(%rdx), %esi
75		leaq .L._printb_str2(%rip), %rdi
76		# on x86, al represents the number of SIMD registers used as variadic arguments
77		movb $0, %al
78		call printf@plt
79		movq $0, %rdi
80		call fflush@plt
81		movq %rbp, %rsp
82		popq %rbp
83		ret
84	
85	.section .rodata
86	# length of .L._println_str0
87		.int 0
88	.L._println_str0:
89		.asciz ""
90	.text
91	_println:
92		pushq %rbp
93		movq %rsp, %rbp
94		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
95		andq $-16, %rsp
96		leaq .L._println_str0(%rip), %rdi
97		call puts@plt
98		movq $0, %rdi
99		call fflush@plt
100		movq %rbp, %rsp
101		popq %rbp
102		ret
===========================================================
-- Finished

