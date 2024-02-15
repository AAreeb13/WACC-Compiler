./valid/expressions/boolCalc.wacc
calling the reference compiler on ./valid/expressions/boolCalc.wacc
-- Test: boolCalc.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple boolean calculation

# Output:
# false
#

# Program:

begin
  bool b1 = true ;
  bool b2 = false ;
  bool b3 = b1 && b2 ;
  println b3
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
boolCalc.s contents are:
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
13		movq $1, %rax
14		movq %rax, %r12
15		movq $0, %rax
16		movq %rax, %r13
17		cmpq $1, %r12
18		jne .L0
19		cmpq $1, %r13
20	.L0:
21		sete %al
22		movsbq %al, %rax
23		pushq %rax
24		popq %rax
25		movq %rax, %rax
26		movq %rax, %r14
27		# Stack pointer unchanged, no stack allocated arguments
28		movq %r14, %rax
29		movq %rax, %rdi
30		# statement primitives do not return results (but will clobber r0/rax)
31		call _printb
32		call _println
33		# Stack pointer unchanged, no stack allocated variables
34		movq $0, %rax
35		# popq {%rbx, %r12, %r13, %r14}
36		movq (%rsp), %rbx
37		movq 8(%rsp), %r12
38		movq 16(%rsp), %r13
39		movq 24(%rsp), %r14
40		addq $32, %rsp
41		popq %rbp
42		ret
43	
44	.section .rodata
45	# length of .L._printb_str0
46		.int 5
47	.L._printb_str0:
48		.asciz "false"
49	# length of .L._printb_str1
50		.int 4
51	.L._printb_str1:
52		.asciz "true"
53	# length of .L._printb_str2
54		.int 4
55	.L._printb_str2:
56		.asciz "%.*s"
57	.text
58	_printb:
59		pushq %rbp
60		movq %rsp, %rbp
61		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
62		andq $-16, %rsp
63		cmpb $0, %dil
64		jne .L_printb0
65		leaq .L._printb_str0(%rip), %rdx
66		jmp .L_printb1
67	.L_printb0:
68		leaq .L._printb_str1(%rip), %rdx
69	.L_printb1:
70		movl -4(%rdx), %esi
71		leaq .L._printb_str2(%rip), %rdi
72		# on x86, al represents the number of SIMD registers used as variadic arguments
73		movb $0, %al
74		call printf@plt
75		movq $0, %rdi
76		call fflush@plt
77		movq %rbp, %rsp
78		popq %rbp
79		ret
80	
81	.section .rodata
82	# length of .L._println_str0
83		.int 0
84	.L._println_str0:
85		.asciz ""
86	.text
87	_println:
88		pushq %rbp
89		movq %rsp, %rbp
90		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
91		andq $-16, %rsp
92		leaq .L._println_str0(%rip), %rdi
93		call puts@plt
94		movq $0, %rdi
95		call fflush@plt
96		movq %rbp, %rsp
97		popq %rbp
98		ret
===========================================================
-- Finished

