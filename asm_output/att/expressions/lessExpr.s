./valid/expressions/lessExpr.wacc
calling the reference compiler on ./valid/expressions/lessExpr.wacc
-- Test: lessExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating less-than

# Output:
# true
# false
#

# Program:

begin
  int x = 2 ;
  int y = 6 ;
  int z = 4 ;
  println x < y ;
  println y < z
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
lessExpr.s contents are:
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
13		movq $2, %rax
14		movq %rax, %r12
15		movq $6, %rax
16		movq %rax, %r13
17		movq $4, %rax
18		movq %rax, %r14
19		# Stack pointer unchanged, no stack allocated arguments
20		cmpq %r13, %r12
21		setl %al
22		movsbq %al, %rax
23		pushq %rax
24		popq %rax
25		movq %rax, %rax
26		movq %rax, %rdi
27		# statement primitives do not return results (but will clobber r0/rax)
28		call _printb
29		call _println
30		# Stack pointer unchanged, no stack allocated arguments
31		cmpq %r14, %r13
32		setl %al
33		movsbq %al, %rax
34		pushq %rax
35		popq %rax
36		movq %rax, %rax
37		movq %rax, %rdi
38		# statement primitives do not return results (but will clobber r0/rax)
39		call _printb
40		call _println
41		# Stack pointer unchanged, no stack allocated variables
42		movq $0, %rax
43		# popq {%rbx, %r12, %r13, %r14}
44		movq (%rsp), %rbx
45		movq 8(%rsp), %r12
46		movq 16(%rsp), %r13
47		movq 24(%rsp), %r14
48		addq $32, %rsp
49		popq %rbp
50		ret
51	
52	.section .rodata
53	# length of .L._printb_str0
54		.int 5
55	.L._printb_str0:
56		.asciz "false"
57	# length of .L._printb_str1
58		.int 4
59	.L._printb_str1:
60		.asciz "true"
61	# length of .L._printb_str2
62		.int 4
63	.L._printb_str2:
64		.asciz "%.*s"
65	.text
66	_printb:
67		pushq %rbp
68		movq %rsp, %rbp
69		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
70		andq $-16, %rsp
71		cmpb $0, %dil
72		jne .L_printb0
73		leaq .L._printb_str0(%rip), %rdx
74		jmp .L_printb1
75	.L_printb0:
76		leaq .L._printb_str1(%rip), %rdx
77	.L_printb1:
78		movl -4(%rdx), %esi
79		leaq .L._printb_str2(%rip), %rdi
80		# on x86, al represents the number of SIMD registers used as variadic arguments
81		movb $0, %al
82		call printf@plt
83		movq $0, %rdi
84		call fflush@plt
85		movq %rbp, %rsp
86		popq %rbp
87		ret
88	
89	.section .rodata
90	# length of .L._println_str0
91		.int 0
92	.L._println_str0:
93		.asciz ""
94	.text
95	_println:
96		pushq %rbp
97		movq %rsp, %rbp
98		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
99		andq $-16, %rsp
100		leaq .L._println_str0(%rip), %rdi
101		call puts@plt
102		movq $0, %rdi
103		call fflush@plt
104		movq %rbp, %rsp
105		popq %rbp
106		ret
===========================================================
-- Finished

