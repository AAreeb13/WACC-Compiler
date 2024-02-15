./valid/expressions/stringEqualsExpr.wacc
calling the reference compiler on ./valid/expressions/stringEqualsExpr.wacc
-- Test: stringEqualsExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating string equality

# Output:
# true
# false
# false
#

# Program:

begin
  string s1 = "Hello" ;
  string s2 = "foo" ;
  string s3 = "bar" ;
  bool b = s1 == s1 ;
  println b ;
  println s1 == s2 ;
  println s2 == s3
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
stringEqualsExpr.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 5
4	.L.str0:
5		.asciz "Hello"
6	# length of .L.str1
7		.int 3
8	.L.str1:
9		.asciz "foo"
10	# length of .L.str2
11		.int 3
12	.L.str2:
13		.asciz "bar"
14	.text
15	main:
16		pushq %rbp
17		# pushq {%rbx, %r12, %r13, %r14, %r15}
18		subq $40, %rsp
19		movq %rbx, (%rsp)
20		movq %r12, 8(%rsp)
21		movq %r13, 16(%rsp)
22		movq %r14, 24(%rsp)
23		movq %r15, 32(%rsp)
24		movq %rsp, %rbp
25		# Stack pointer unchanged, no stack allocated variables
26		leaq .L.str0(%rip), %rax
27		pushq %rax
28		popq %rax
29		movq %rax, %rax
30		movq %rax, %r12
31		leaq .L.str1(%rip), %rax
32		pushq %rax
33		popq %rax
34		movq %rax, %rax
35		movq %rax, %r13
36		leaq .L.str2(%rip), %rax
37		pushq %rax
38		popq %rax
39		movq %rax, %rax
40		movq %rax, %r14
41		cmpq %r12, %r12
42		sete %al
43		movsbq %al, %rax
44		pushq %rax
45		popq %rax
46		movq %rax, %rax
47		movq %rax, %r15
48		# Stack pointer unchanged, no stack allocated arguments
49		movq %r15, %rax
50		movq %rax, %rdi
51		# statement primitives do not return results (but will clobber r0/rax)
52		call _printb
53		call _println
54		# Stack pointer unchanged, no stack allocated arguments
55		cmpq %r13, %r12
56		sete %al
57		movsbq %al, %rax
58		pushq %rax
59		popq %rax
60		movq %rax, %rax
61		movq %rax, %rdi
62		# statement primitives do not return results (but will clobber r0/rax)
63		call _printb
64		call _println
65		# Stack pointer unchanged, no stack allocated arguments
66		cmpq %r14, %r13
67		sete %al
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

