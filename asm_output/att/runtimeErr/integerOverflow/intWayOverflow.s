./valid/runtimeErr/integerOverflow/intWayOverflow.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intWayOverflow.wacc
-- Test: intWayOverflow.wacc

-- Uploaded file: 
---------------------------------------------------------------
# positive overflow

# Output:
# 2000000000
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = 2000000000 ;
  println x ;
  x = x + 2000000000; #err here?
  println x 
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intWayOverflow.s contents are:
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
11		movq $2000000000, %rax
12		movq %rax, %r12
13		# Stack pointer unchanged, no stack allocated arguments
14		movq %r12, %rax
15		movq %rax, %rdi
16		# statement primitives do not return results (but will clobber r0/rax)
17		call _printi
18		call _println
19		movl %r12d, %eax
20		addl $2000000000, %eax
21		jo _errOverflow
22		movslq %eax, %rax
23		pushq %rax
24		popq %rax
25		movq %rax, %rax
26		movq %rax, %r12
27		# Stack pointer unchanged, no stack allocated arguments
28		movq %r12, %rax
29		movq %rax, %rdi
30		# statement primitives do not return results (but will clobber r0/rax)
31		call _printi
32		call _println
33		# Stack pointer unchanged, no stack allocated variables
34		movq $0, %rax
35		# popq {%rbx, %r12}
36		movq (%rsp), %rbx
37		movq 8(%rsp), %r12
38		addq $16, %rsp
39		popq %rbp
40		ret
41	
42	.section .rodata
43	# length of .L._prints_str0
44		.int 4
45	.L._prints_str0:
46		.asciz "%.*s"
47	.text
48	_prints:
49		pushq %rbp
50		movq %rsp, %rbp
51		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
52		andq $-16, %rsp
53		movq %rdi, %rdx
54		movl -4(%rdi), %esi
55		leaq .L._prints_str0(%rip), %rdi
56		# on x86, al represents the number of SIMD registers used as variadic arguments
57		movb $0, %al
58		call printf@plt
59		movq $0, %rdi
60		call fflush@plt
61		movq %rbp, %rsp
62		popq %rbp
63		ret
64	
65	.section .rodata
66	# length of .L._printi_str0
67		.int 2
68	.L._printi_str0:
69		.asciz "%d"
70	.text
71	_printi:
72		pushq %rbp
73		movq %rsp, %rbp
74		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
75		andq $-16, %rsp
76		movl %edi, %esi
77		leaq .L._printi_str0(%rip), %rdi
78		# on x86, al represents the number of SIMD registers used as variadic arguments
79		movb $0, %al
80		call printf@plt
81		movq $0, %rdi
82		call fflush@plt
83		movq %rbp, %rsp
84		popq %rbp
85		ret
86	
87	.section .rodata
88	# length of .L._println_str0
89		.int 0
90	.L._println_str0:
91		.asciz ""
92	.text
93	_println:
94		pushq %rbp
95		movq %rsp, %rbp
96		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
97		andq $-16, %rsp
98		leaq .L._println_str0(%rip), %rdi
99		call puts@plt
100		movq $0, %rdi
101		call fflush@plt
102		movq %rbp, %rsp
103		popq %rbp
104		ret
105	
106	_exit:
107		pushq %rbp
108		movq %rsp, %rbp
109		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
110		andq $-16, %rsp
111		call exit@plt
112		movq %rbp, %rsp
113		popq %rbp
114		ret
115	
116	.section .rodata
117	# length of .L._errOverflow_str0
118		.int 52
119	.L._errOverflow_str0:
120		.asciz "fatal error: integer overflow or underflow occurred\n"
121	.text
122	_errOverflow:
123		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
124		andq $-16, %rsp
125		leaq .L._errOverflow_str0(%rip), %rdi
126		call _prints
127		movb $-1, %dil
128		call exit@plt
===========================================================
-- Finished

