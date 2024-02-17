./valid/expressions/charComparisonExpr.wacc
calling the reference compiler on ./valid/expressions/charComparisonExpr.wacc
-- Test: charComparisonExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# detailed battery of character comparison tests

# Output:
# false
# true
# true
# true
# false
# false
#

# Program:

begin
  char c1 = 'a' ;
  char c2 = 'z' ;
  println c1 == c2 ;
  println c1 != c2 ;
  println c1 < c2 ;
  println c1 <= c2 ;
  println c1 > c2 ;
  println c1 >= c2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
charComparisonExpr.s contents are:
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
12		movq $97, %rax
13		movq %rax, %r12
14		movq $122, %rax
15		movq %rax, %r13
16		# Stack pointer unchanged, no stack allocated arguments
17		cmpq %r13, %r12
18		sete %al
19		movsbq %al, %rax
20		pushq %rax
21		popq %rax
22		movq %rax, %rax
23		movq %rax, %rdi
24		# statement primitives do not return results (but will clobber r0/rax)
25		call _printb
26		call _println
27		# Stack pointer unchanged, no stack allocated arguments
28		cmpq %r13, %r12
29		setne %al
30		movsbq %al, %rax
31		pushq %rax
32		popq %rax
33		movq %rax, %rax
34		movq %rax, %rdi
35		# statement primitives do not return results (but will clobber r0/rax)
36		call _printb
37		call _println
38		# Stack pointer unchanged, no stack allocated arguments
39		cmpq %r13, %r12
40		setl %al
41		movsbq %al, %rax
42		pushq %rax
43		popq %rax
44		movq %rax, %rax
45		movq %rax, %rdi
46		# statement primitives do not return results (but will clobber r0/rax)
47		call _printb
48		call _println
49		# Stack pointer unchanged, no stack allocated arguments
50		cmpq %r13, %r12
51		setle %al
52		movsbq %al, %rax
53		pushq %rax
54		popq %rax
55		movq %rax, %rax
56		movq %rax, %rdi
57		# statement primitives do not return results (but will clobber r0/rax)
58		call _printb
59		call _println
60		# Stack pointer unchanged, no stack allocated arguments
61		cmpq %r13, %r12
62		setg %al
63		movsbq %al, %rax
64		pushq %rax
65		popq %rax
66		movq %rax, %rax
67		movq %rax, %rdi
68		# statement primitives do not return results (but will clobber r0/rax)
69		call _printb
70		call _println
71		# Stack pointer unchanged, no stack allocated arguments
72		cmpq %r13, %r12
73		setge %al
74		movsbq %al, %rax
75		pushq %rax
76		popq %rax
77		movq %rax, %rax
78		movq %rax, %rdi
79		# statement primitives do not return results (but will clobber r0/rax)
80		call _printb
81		call _println
82		# Stack pointer unchanged, no stack allocated variables
83		movq $0, %rax
84		# popq {%rbx, %r12, %r13}
85		movq (%rsp), %rbx
86		movq 8(%rsp), %r12
87		movq 16(%rsp), %r13
88		addq $24, %rsp
89		popq %rbp
90		ret
91	
92	.section .rodata
93	# length of .L._printb_str0
94		.int 5
95	.L._printb_str0:
96		.asciz "false"
97	# length of .L._printb_str1
98		.int 4
99	.L._printb_str1:
100		.asciz "true"
101	# length of .L._printb_str2
102		.int 4
103	.L._printb_str2:
104		.asciz "%.*s"
105	.text
106	_printb:
107		pushq %rbp
108		movq %rsp, %rbp
109		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
110		andq $-16, %rsp
111		cmpb $0, %dil
112		jne .L_printb0
113		leaq .L._printb_str0(%rip), %rdx
114		jmp .L_printb1
115	.L_printb0:
116		leaq .L._printb_str1(%rip), %rdx
117	.L_printb1:
118		movl -4(%rdx), %esi
119		leaq .L._printb_str2(%rip), %rdi
120		# on x86, al represents the number of SIMD registers used as variadic arguments
121		movb $0, %al
122		call printf@plt
123		movq $0, %rdi
124		call fflush@plt
125		movq %rbp, %rsp
126		popq %rbp
127		ret
128	
129	.section .rodata
130	# length of .L._println_str0
131		.int 0
132	.L._println_str0:
133		.asciz ""
134	.text
135	_println:
136		pushq %rbp
137		movq %rsp, %rbp
138		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
139		andq $-16, %rsp
140		leaq .L._println_str0(%rip), %rdi
141		call puts@plt
142		movq $0, %rdi
143		call fflush@plt
144		movq %rbp, %rsp
145		popq %rbp
146		ret
===========================================================
-- Finished

