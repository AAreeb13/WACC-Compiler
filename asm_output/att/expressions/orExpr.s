./valid/expressions/orExpr.wacc
calling the reference compiler on ./valid/expressions/orExpr.wacc
-- Test: orExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating or

# Output:
# true
# true
# false
#

# Program:

begin
  bool a = true ;
  bool b = false ;
  println a || b ;
  println a || true ;
  println b || false
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
orExpr.s contents are:
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
18		je .L0
19		cmpq $1, %r13
20	.L0:
21		sete %al
22		movsbq %al, %rax
23		pushq %rax
24		popq %rax
25		movq %rax, %rax
26		movq %rax, %rdi
27		# statement primitives do not return results (but will clobber r0/rax)
28		call _printb
29		call _println
30		# Stack pointer unchanged, no stack allocated arguments
31		cmpq $1, %r12
32		je .L1
33		movq $1, %rax
34		cmpq $1, %rax
35	.L1:
36		sete %al
37		movsbq %al, %rax
38		pushq %rax
39		popq %rax
40		movq %rax, %rax
41		movq %rax, %rdi
42		# statement primitives do not return results (but will clobber r0/rax)
43		call _printb
44		call _println
45		# Stack pointer unchanged, no stack allocated arguments
46		cmpq $1, %r13
47		je .L2
48		movq $0, %rax
49		cmpq $1, %rax
50	.L2:
51		sete %al
52		movsbq %al, %rax
53		pushq %rax
54		popq %rax
55		movq %rax, %rax
56		movq %rax, %rdi
57		# statement primitives do not return results (but will clobber r0/rax)
58		call _printb
59		call _println
60		# Stack pointer unchanged, no stack allocated variables
61		movq $0, %rax
62		# popq {%rbx, %r12, %r13}
63		movq (%rsp), %rbx
64		movq 8(%rsp), %r12
65		movq 16(%rsp), %r13
66		addq $24, %rsp
67		popq %rbp
68		ret
69	
70	.section .rodata
71	# length of .L._printb_str0
72		.int 5
73	.L._printb_str0:
74		.asciz "false"
75	# length of .L._printb_str1
76		.int 4
77	.L._printb_str1:
78		.asciz "true"
79	# length of .L._printb_str2
80		.int 4
81	.L._printb_str2:
82		.asciz "%.*s"
83	.text
84	_printb:
85		pushq %rbp
86		movq %rsp, %rbp
87		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
88		andq $-16, %rsp
89		cmpb $0, %dil
90		jne .L_printb0
91		leaq .L._printb_str0(%rip), %rdx
92		jmp .L_printb1
93	.L_printb0:
94		leaq .L._printb_str1(%rip), %rdx
95	.L_printb1:
96		movl -4(%rdx), %esi
97		leaq .L._printb_str2(%rip), %rdi
98		# on x86, al represents the number of SIMD registers used as variadic arguments
99		movb $0, %al
100		call printf@plt
101		movq $0, %rdi
102		call fflush@plt
103		movq %rbp, %rsp
104		popq %rbp
105		ret
106	
107	.section .rodata
108	# length of .L._println_str0
109		.int 0
110	.L._println_str0:
111		.asciz ""
112	.text
113	_println:
114		pushq %rbp
115		movq %rsp, %rbp
116		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
117		andq $-16, %rsp
118		leaq .L._println_str0(%rip), %rdi
119		call puts@plt
120		movq $0, %rdi
121		call fflush@plt
122		movq %rbp, %rsp
123		popq %rbp
124		ret
===========================================================
-- Finished

