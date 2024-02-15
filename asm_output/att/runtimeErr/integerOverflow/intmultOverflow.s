./valid/runtimeErr/integerOverflow/intmultOverflow.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intmultOverflow.wacc
-- Test: intmultOverflow.wacc

-- Uploaded file: 
---------------------------------------------------------------
# integer overflow - generates odd assembly error!

# Output:
# 2147483
# 2147483000
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = 2147483 ;
  println x ;
  x = x * 1000 ;
  println x ;
  x = x * 1000 ;
  println x ;
  x = x * 1000 ; #err here?
  println x 
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intmultOverflow.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12}
6		subq $16, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %rsp, %rbp
10		# Stack pointer unchanged, no stack allocated variables
11		movq $2147483, %rax
12		movq %rax, %r12
13		# Stack pointer unchanged, no stack allocated arguments
14		movq %r12, %rax
15		movq %rax, %rdi
16		# statement primitives do not return results (but will clobber r0/rax)
17		call _printi
18		call _println
19		imull $1000, %r12d, %eax
20		jo _errOverflow
21		movslq %eax, %rax
22		pushq %rax
23		popq %rax
24		movq %rax, %rax
25		movq %rax, %r12
26		# Stack pointer unchanged, no stack allocated arguments
27		movq %r12, %rax
28		movq %rax, %rdi
29		# statement primitives do not return results (but will clobber r0/rax)
30		call _printi
31		call _println
32		imull $1000, %r12d, %eax
33		jo _errOverflow
34		movslq %eax, %rax
35		pushq %rax
36		popq %rax
37		movq %rax, %rax
38		movq %rax, %r12
39		# Stack pointer unchanged, no stack allocated arguments
40		movq %r12, %rax
41		movq %rax, %rdi
42		# statement primitives do not return results (but will clobber r0/rax)
43		call _printi
44		call _println
45		imull $1000, %r12d, %eax
46		jo _errOverflow
47		movslq %eax, %rax
48		pushq %rax
49		popq %rax
50		movq %rax, %rax
51		movq %rax, %r12
52		# Stack pointer unchanged, no stack allocated arguments
53		movq %r12, %rax
54		movq %rax, %rdi
55		# statement primitives do not return results (but will clobber r0/rax)
56		call _printi
57		call _println
58		# Stack pointer unchanged, no stack allocated variables
59		movq $0, %rax
60		# popq {%rbx, %r12}
61		movq (%rsp), %rbx
62		movq 8(%rsp), %r12
63		addq $16, %rsp
64		popq %rbp
65		ret
66	
67	.section .rodata
68	# length of .L._prints_str0
69		.int 4
70	.L._prints_str0:
71		.asciz "%.*s"
72	.text
73	_prints:
74		pushq %rbp
75		movq %rsp, %rbp
76		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
77		andq $-16, %rsp
78		movq %rdi, %rdx
79		movl -4(%rdi), %esi
80		leaq .L._prints_str0(%rip), %rdi
81		# on x86, al represents the number of SIMD registers used as variadic arguments
82		movb $0, %al
83		call printf@plt
84		movq $0, %rdi
85		call fflush@plt
86		movq %rbp, %rsp
87		popq %rbp
88		ret
89	
90	.section .rodata
91	# length of .L._printi_str0
92		.int 2
93	.L._printi_str0:
94		.asciz "%d"
95	.text
96	_printi:
97		pushq %rbp
98		movq %rsp, %rbp
99		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
100		andq $-16, %rsp
101		movl %edi, %esi
102		leaq .L._printi_str0(%rip), %rdi
103		# on x86, al represents the number of SIMD registers used as variadic arguments
104		movb $0, %al
105		call printf@plt
106		movq $0, %rdi
107		call fflush@plt
108		movq %rbp, %rsp
109		popq %rbp
110		ret
111	
112	.section .rodata
113	# length of .L._println_str0
114		.int 0
115	.L._println_str0:
116		.asciz ""
117	.text
118	_println:
119		pushq %rbp
120		movq %rsp, %rbp
121		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
122		andq $-16, %rsp
123		leaq .L._println_str0(%rip), %rdi
124		call puts@plt
125		movq $0, %rdi
126		call fflush@plt
127		movq %rbp, %rsp
128		popq %rbp
129		ret
130	
131	_exit:
132		pushq %rbp
133		movq %rsp, %rbp
134		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
135		andq $-16, %rsp
136		call exit@plt
137		movq %rbp, %rsp
138		popq %rbp
139		ret
140	
141	.section .rodata
142	# length of .L._errOverflow_str0
143		.int 52
144	.L._errOverflow_str0:
145		.asciz "fatal error: integer overflow or underflow occurred\n"
146	.text
147	_errOverflow:
148		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
149		andq $-16, %rsp
150		leaq .L._errOverflow_str0(%rip), %rdi
151		call _prints
152		movb $-1, %dil
153		call exit@plt
===========================================================
-- Finished

