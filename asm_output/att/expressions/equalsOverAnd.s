./valid/expressions/equalsOverAnd.wacc
calling the reference compiler on ./valid/expressions/equalsOverAnd.wacc
-- Test: equalsOverAnd.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating equality and boolean operator precedence

# Output:
# false
# true
#

# Program:

begin
  bool p = false ;
  bool q = true ;
  bool r = false ;
  println p == q && r ;
  println p == (q && r)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
equalsOverAnd.s contents are:
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
15		movq $1, %rax
16		movq %rax, %r13
17		movq $0, %rax
18		movq %rax, %r14
19		# Stack pointer unchanged, no stack allocated arguments
20		cmpq %r13, %r12
21		sete %al
22		movsbq %al, %rax
23		pushq %rax
24		popq %rax
25		cmpq $1, %rax
26		jne .L0
27		cmpq $1, %r14
28	.L0:
29		sete %al
30		movsbq %al, %rax
31		pushq %rax
32		popq %rax
33		movq %rax, %rax
34		movq %rax, %rdi
35		# statement primitives do not return results (but will clobber r0/rax)
36		call _printb
37		call _println
38		# Stack pointer unchanged, no stack allocated arguments
39		cmpq $1, %r13
40		jne .L1
41		cmpq $1, %r14
42	.L1:
43		sete %al
44		movsbq %al, %rax
45		pushq %rax
46		popq %rbx
47		cmpq %rbx, %r12
48		sete %al
49		movsbq %al, %rax
50		pushq %rax
51		popq %rax
52		movq %rax, %rax
53		movq %rax, %rdi
54		# statement primitives do not return results (but will clobber r0/rax)
55		call _printb
56		call _println
57		# Stack pointer unchanged, no stack allocated variables
58		movq $0, %rax
59		# popq {%rbx, %r12, %r13, %r14}
60		movq (%rsp), %rbx
61		movq 8(%rsp), %r12
62		movq 16(%rsp), %r13
63		movq 24(%rsp), %r14
64		addq $32, %rsp
65		popq %rbp
66		ret
67	
68	.section .rodata
69	# length of .L._printb_str0
70		.int 5
71	.L._printb_str0:
72		.asciz "false"
73	# length of .L._printb_str1
74		.int 4
75	.L._printb_str1:
76		.asciz "true"
77	# length of .L._printb_str2
78		.int 4
79	.L._printb_str2:
80		.asciz "%.*s"
81	.text
82	_printb:
83		pushq %rbp
84		movq %rsp, %rbp
85		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
86		andq $-16, %rsp
87		cmpb $0, %dil
88		jne .L_printb0
89		leaq .L._printb_str0(%rip), %rdx
90		jmp .L_printb1
91	.L_printb0:
92		leaq .L._printb_str1(%rip), %rdx
93	.L_printb1:
94		movl -4(%rdx), %esi
95		leaq .L._printb_str2(%rip), %rdi
96		# on x86, al represents the number of SIMD registers used as variadic arguments
97		movb $0, %al
98		call printf@plt
99		movq $0, %rdi
100		call fflush@plt
101		movq %rbp, %rsp
102		popq %rbp
103		ret
104	
105	.section .rodata
106	# length of .L._println_str0
107		.int 0
108	.L._println_str0:
109		.asciz ""
110	.text
111	_println:
112		pushq %rbp
113		movq %rsp, %rbp
114		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
115		andq $-16, %rsp
116		leaq .L._println_str0(%rip), %rdi
117		call puts@plt
118		movq $0, %rdi
119		call fflush@plt
120		movq %rbp, %rsp
121		popq %rbp
122		ret
===========================================================
-- Finished

