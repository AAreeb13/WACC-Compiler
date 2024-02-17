./valid/expressions/notequalsExpr.wacc
calling the reference compiler on ./valid/expressions/notequalsExpr.wacc
-- Test: notequalsExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating inequality

# Output:
# true
# true
# false
#

# Program:

begin
  int x = 2 ;
  int y = 4 ;
  int z = 4 ;
  bool b = x != y ;
  println b ;
  println x != y ;
  println y != z
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
notequalsExpr.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12, %r13, %r14, %r15}
6		subq $40, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %r13, 16(%rsp)
10		movq %r14, 24(%rsp)
11		movq %r15, 32(%rsp)
12		movq %rsp, %rbp
13		# Stack pointer unchanged, no stack allocated variables
14		movq $2, %rax
15		movq %rax, %r12
16		movq $4, %rax
17		movq %rax, %r13
18		movq $4, %rax
19		movq %rax, %r14
20		cmpq %r13, %r12
21		setne %al
22		movsbq %al, %rax
23		pushq %rax
24		popq %rax
25		movq %rax, %rax
26		movq %rax, %r15
27		# Stack pointer unchanged, no stack allocated arguments
28		movq %r15, %rax
29		movq %rax, %rdi
30		# statement primitives do not return results (but will clobber r0/rax)
31		call _printb
32		call _println
33		# Stack pointer unchanged, no stack allocated arguments
34		cmpq %r13, %r12
35		setne %al
36		movsbq %al, %rax
37		pushq %rax
38		popq %rax
39		movq %rax, %rax
40		movq %rax, %rdi
41		# statement primitives do not return results (but will clobber r0/rax)
42		call _printb
43		call _println
44		# Stack pointer unchanged, no stack allocated arguments
45		cmpq %r14, %r13
46		setne %al
47		movsbq %al, %rax
48		pushq %rax
49		popq %rax
50		movq %rax, %rax
51		movq %rax, %rdi
52		# statement primitives do not return results (but will clobber r0/rax)
53		call _printb
54		call _println
55		# Stack pointer unchanged, no stack allocated variables
56		movq $0, %rax
57		# popq {%rbx, %r12, %r13, %r14, %r15}
58		movq (%rsp), %rbx
59		movq 8(%rsp), %r12
60		movq 16(%rsp), %r13
61		movq 24(%rsp), %r14
62		movq 32(%rsp), %r15
63		addq $40, %rsp
64		popq %rbp
65		ret
66	
67	.section .rodata
68	# length of .L._printb_str0
69		.int 5
70	.L._printb_str0:
71		.asciz "false"
72	# length of .L._printb_str1
73		.int 4
74	.L._printb_str1:
75		.asciz "true"
76	# length of .L._printb_str2
77		.int 4
78	.L._printb_str2:
79		.asciz "%.*s"
80	.text
81	_printb:
82		pushq %rbp
83		movq %rsp, %rbp
84		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
85		andq $-16, %rsp
86		cmpb $0, %dil
87		jne .L_printb0
88		leaq .L._printb_str0(%rip), %rdx
89		jmp .L_printb1
90	.L_printb0:
91		leaq .L._printb_str1(%rip), %rdx
92	.L_printb1:
93		movl -4(%rdx), %esi
94		leaq .L._printb_str2(%rip), %rdi
95		# on x86, al represents the number of SIMD registers used as variadic arguments
96		movb $0, %al
97		call printf@plt
98		movq $0, %rdi
99		call fflush@plt
100		movq %rbp, %rsp
101		popq %rbp
102		ret
103	
104	.section .rodata
105	# length of .L._println_str0
106		.int 0
107	.L._println_str0:
108		.asciz ""
109	.text
110	_println:
111		pushq %rbp
112		movq %rsp, %rbp
113		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
114		andq $-16, %rsp
115		leaq .L._println_str0(%rip), %rdi
116		call puts@plt
117		movq $0, %rdi
118		call fflush@plt
119		movq %rbp, %rsp
120		popq %rbp
121		ret
===========================================================
-- Finished

