./valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc
-- Test: intnegateOverflow3.wacc

-- Uploaded file: 
---------------------------------------------------------------
# multiplying a negative integer

# Output:
# -20000
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = -20000 ;
  println x ;
  x = x*100000000; #err here? 
  println x 
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intnegateOverflow3.s contents are:
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
11		movq $-20000, %rax
12		movq %rax, %r12
13		# Stack pointer unchanged, no stack allocated arguments
14		movq %r12, %rax
15		movq %rax, %rdi
16		# statement primitives do not return results (but will clobber r0/rax)
17		call _printi
18		call _println
19		imull $100000000, %r12d, %eax
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
32		# Stack pointer unchanged, no stack allocated variables
33		movq $0, %rax
34		# popq {%rbx, %r12}
35		movq (%rsp), %rbx
36		movq 8(%rsp), %r12
37		addq $16, %rsp
38		popq %rbp
39		ret
40	
41	.section .rodata
42	# length of .L._prints_str0
43		.int 4
44	.L._prints_str0:
45		.asciz "%.*s"
46	.text
47	_prints:
48		pushq %rbp
49		movq %rsp, %rbp
50		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
51		andq $-16, %rsp
52		movq %rdi, %rdx
53		movl -4(%rdi), %esi
54		leaq .L._prints_str0(%rip), %rdi
55		# on x86, al represents the number of SIMD registers used as variadic arguments
56		movb $0, %al
57		call printf@plt
58		movq $0, %rdi
59		call fflush@plt
60		movq %rbp, %rsp
61		popq %rbp
62		ret
63	
64	.section .rodata
65	# length of .L._printi_str0
66		.int 2
67	.L._printi_str0:
68		.asciz "%d"
69	.text
70	_printi:
71		pushq %rbp
72		movq %rsp, %rbp
73		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
74		andq $-16, %rsp
75		movl %edi, %esi
76		leaq .L._printi_str0(%rip), %rdi
77		# on x86, al represents the number of SIMD registers used as variadic arguments
78		movb $0, %al
79		call printf@plt
80		movq $0, %rdi
81		call fflush@plt
82		movq %rbp, %rsp
83		popq %rbp
84		ret
85	
86	.section .rodata
87	# length of .L._println_str0
88		.int 0
89	.L._println_str0:
90		.asciz ""
91	.text
92	_println:
93		pushq %rbp
94		movq %rsp, %rbp
95		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
96		andq $-16, %rsp
97		leaq .L._println_str0(%rip), %rdi
98		call puts@plt
99		movq $0, %rdi
100		call fflush@plt
101		movq %rbp, %rsp
102		popq %rbp
103		ret
104	
105	_exit:
106		pushq %rbp
107		movq %rsp, %rbp
108		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
109		andq $-16, %rsp
110		call exit@plt
111		movq %rbp, %rsp
112		popq %rbp
113		ret
114	
115	.section .rodata
116	# length of .L._errOverflow_str0
117		.int 52
118	.L._errOverflow_str0:
119		.asciz "fatal error: integer overflow or underflow occurred\n"
120	.text
121	_errOverflow:
122		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
123		andq $-16, %rsp
124		leaq .L._errOverflow_str0(%rip), %rdi
125		call _prints
126		movb $-1, %dil
127		call exit@plt
===========================================================
-- Finished

