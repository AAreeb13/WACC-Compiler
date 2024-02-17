./valid/expressions/andOverOrExpr.wacc
calling the reference compiler on ./valid/expressions/andOverOrExpr.wacc
-- Test: andOverOrExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating boolean operator precedence

# Output:
# true
# false
#

# Program:

begin
  bool a = false ;
  bool b = false ;
  bool c = true ;
  println a && b || c ;
  println a && (b || c)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
andOverOrExpr.s contents are:
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
13		movq $0, %rax
14		movq %rax, %r12
15		movq $0, %rax
16		movq %rax, %r13
17		movq $1, %rax
18		movq %rax, %r14
19		# Stack pointer unchanged, no stack allocated arguments
20		cmpq $1, %r12
21		jne .L0
22		cmpq $1, %r13
23	.L0:
24		sete %al
25		movsbq %al, %rax
26		pushq %rax
27		popq %rax
28		cmpq $1, %rax
29		je .L1
30		cmpq $1, %r14
31	.L1:
32		sete %al
33		movsbq %al, %rax
34		pushq %rax
35		popq %rax
36		movq %rax, %rax
37		movq %rax, %rdi
38		# statement primitives do not return results (but will clobber r0/rax)
39		call _printb
40		call _println
41		# Stack pointer unchanged, no stack allocated arguments
42		cmpq $1, %r12
43		jne .L2
44		cmpq $1, %r13
45		je .L3
46		cmpq $1, %r14
47	.L3:
48		sete %al
49		movsbq %al, %rax
50		pushq %rax
51		popq %rax
52		cmpq $1, %rax
53	.L2:
54		sete %al
55		movsbq %al, %rax
56		pushq %rax
57		popq %rax
58		movq %rax, %rax
59		movq %rax, %rdi
60		# statement primitives do not return results (but will clobber r0/rax)
61		call _printb
62		call _println
63		# Stack pointer unchanged, no stack allocated variables
64		movq $0, %rax
65		# popq {%rbx, %r12, %r13, %r14}
66		movq (%rsp), %rbx
67		movq 8(%rsp), %r12
68		movq 16(%rsp), %r13
69		movq 24(%rsp), %r14
70		addq $32, %rsp
71		popq %rbp
72		ret
73	
74	.section .rodata
75	# length of .L._printb_str0
76		.int 5
77	.L._printb_str0:
78		.asciz "false"
79	# length of .L._printb_str1
80		.int 4
81	.L._printb_str1:
82		.asciz "true"
83	# length of .L._printb_str2
84		.int 4
85	.L._printb_str2:
86		.asciz "%.*s"
87	.text
88	_printb:
89		pushq %rbp
90		movq %rsp, %rbp
91		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
92		andq $-16, %rsp
93		cmpb $0, %dil
94		jne .L_printb0
95		leaq .L._printb_str0(%rip), %rdx
96		jmp .L_printb1
97	.L_printb0:
98		leaq .L._printb_str1(%rip), %rdx
99	.L_printb1:
100		movl -4(%rdx), %esi
101		leaq .L._printb_str2(%rip), %rdi
102		# on x86, al represents the number of SIMD registers used as variadic arguments
103		movb $0, %al
104		call printf@plt
105		movq $0, %rdi
106		call fflush@plt
107		movq %rbp, %rsp
108		popq %rbp
109		ret
110	
111	.section .rodata
112	# length of .L._println_str0
113		.int 0
114	.L._println_str0:
115		.asciz ""
116	.text
117	_println:
118		pushq %rbp
119		movq %rsp, %rbp
120		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
121		andq $-16, %rsp
122		leaq .L._println_str0(%rip), %rdi
123		call puts@plt
124		movq $0, %rdi
125		call fflush@plt
126		movq %rbp, %rsp
127		popq %rbp
128		ret
===========================================================
-- Finished

