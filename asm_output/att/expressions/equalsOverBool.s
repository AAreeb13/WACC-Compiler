./valid/expressions/equalsOverBool.wacc
calling the reference compiler on ./valid/expressions/equalsOverBool.wacc
-- Test: equalsOverBool.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating equality and boolean operator precedence

# Output:
# true
# false
#

# Program:

begin
  bool p = true ;
  bool q = true ;
  bool r = false ;
  bool s = true ;
  println p && q != r || s ;
  println (p && q) != (r || s)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
equalsOverBool.s contents are:
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
14		movq $1, %rax
15		movq %rax, %r12
16		movq $1, %rax
17		movq %rax, %r13
18		movq $0, %rax
19		movq %rax, %r14
20		movq $1, %rax
21		movq %rax, %r15
22		# Stack pointer unchanged, no stack allocated arguments
23		cmpq $1, %r12
24		jne .L0
25		cmpq %r14, %r13
26		setne %al
27		movsbq %al, %rax
28		pushq %rax
29		popq %rax
30		cmpq $1, %rax
31	.L0:
32		sete %al
33		movsbq %al, %rax
34		pushq %rax
35		popq %rax
36		cmpq $1, %rax
37		je .L1
38		cmpq $1, %r15
39	.L1:
40		sete %al
41		movsbq %al, %rax
42		pushq %rax
43		popq %rax
44		movq %rax, %rax
45		movq %rax, %rdi
46		# statement primitives do not return results (but will clobber r0/rax)
47		call _printb
48		call _println
49		# Stack pointer unchanged, no stack allocated arguments
50		cmpq $1, %r12
51		jne .L2
52		cmpq $1, %r13
53	.L2:
54		sete %al
55		movsbq %al, %rax
56		pushq %rax
57		cmpq $1, %r14
58		je .L3
59		cmpq $1, %r15
60	.L3:
61		sete %al
62		movsbq %al, %rax
63		pushq %rax
64		popq %rbx
65		popq %rax
66		cmpq %rbx, %rax
67		setne %al
68		movsbq %al, %rax
69		pushq %rax
70		popq %rax
71		movq %rax, %rax
72		movq %rax, %rdi
73		# statement primitives do not return results (but will clobber r0/rax)
74		call _printb
75		call _println
76		# Stack pointer unchanged, no stack allocated variables
77		movq $0, %rax
78		# popq {%rbx, %r12, %r13, %r14, %r15}
79		movq (%rsp), %rbx
80		movq 8(%rsp), %r12
81		movq 16(%rsp), %r13
82		movq 24(%rsp), %r14
83		movq 32(%rsp), %r15
84		addq $40, %rsp
85		popq %rbp
86		ret
87	
88	.section .rodata
89	# length of .L._printb_str0
90		.int 5
91	.L._printb_str0:
92		.asciz "false"
93	# length of .L._printb_str1
94		.int 4
95	.L._printb_str1:
96		.asciz "true"
97	# length of .L._printb_str2
98		.int 4
99	.L._printb_str2:
100		.asciz "%.*s"
101	.text
102	_printb:
103		pushq %rbp
104		movq %rsp, %rbp
105		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
106		andq $-16, %rsp
107		cmpb $0, %dil
108		jne .L_printb0
109		leaq .L._printb_str0(%rip), %rdx
110		jmp .L_printb1
111	.L_printb0:
112		leaq .L._printb_str1(%rip), %rdx
113	.L_printb1:
114		movl -4(%rdx), %esi
115		leaq .L._printb_str2(%rip), %rdi
116		# on x86, al represents the number of SIMD registers used as variadic arguments
117		movb $0, %al
118		call printf@plt
119		movq $0, %rdi
120		call fflush@plt
121		movq %rbp, %rsp
122		popq %rbp
123		ret
124	
125	.section .rodata
126	# length of .L._println_str0
127		.int 0
128	.L._println_str0:
129		.asciz ""
130	.text
131	_println:
132		pushq %rbp
133		movq %rsp, %rbp
134		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
135		andq $-16, %rsp
136		leaq .L._println_str0(%rip), %rdi
137		call puts@plt
138		movq $0, %rdi
139		call fflush@plt
140		movq %rbp, %rsp
141		popq %rbp
142		ret
===========================================================
-- Finished

