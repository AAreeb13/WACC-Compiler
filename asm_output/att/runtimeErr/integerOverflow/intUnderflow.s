./valid/runtimeErr/integerOverflow/intUnderflow.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intUnderflow.wacc
-- Test: intUnderflow.wacc

-- Uploaded file: 
---------------------------------------------------------------
# integer underflow

# Output:
# -2147483647
# -2147483648
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = -2147483647 ;
  println x ;
  x = x - 1 ;
  println x ;
  x = x - 1 ; #err here?
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intUnderflow.s contents are:
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
11		movq $-2147483647, %rax
12		movq %rax, %r12
13		# Stack pointer unchanged, no stack allocated arguments
14		movq %r12, %rax
15		movq %rax, %rdi
16		# statement primitives do not return results (but will clobber r0/rax)
17		call _printi
18		call _println
19		movl %r12d, %eax
20		subl $1, %eax
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
33		movl %r12d, %eax
34		subl $1, %eax
35		jo _errOverflow
36		movslq %eax, %rax
37		pushq %rax
38		popq %rax
39		movq %rax, %rax
40		movq %rax, %r12
41		# Stack pointer unchanged, no stack allocated arguments
42		movq %r12, %rax
43		movq %rax, %rdi
44		# statement primitives do not return results (but will clobber r0/rax)
45		call _printi
46		call _println
47		# Stack pointer unchanged, no stack allocated variables
48		movq $0, %rax
49		# popq {%rbx, %r12}
50		movq (%rsp), %rbx
51		movq 8(%rsp), %r12
52		addq $16, %rsp
53		popq %rbp
54		ret
55	
56	.section .rodata
57	# length of .L._prints_str0
58		.int 4
59	.L._prints_str0:
60		.asciz "%.*s"
61	.text
62	_prints:
63		pushq %rbp
64		movq %rsp, %rbp
65		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
66		andq $-16, %rsp
67		movq %rdi, %rdx
68		movl -4(%rdi), %esi
69		leaq .L._prints_str0(%rip), %rdi
70		# on x86, al represents the number of SIMD registers used as variadic arguments
71		movb $0, %al
72		call printf@plt
73		movq $0, %rdi
74		call fflush@plt
75		movq %rbp, %rsp
76		popq %rbp
77		ret
78	
79	.section .rodata
80	# length of .L._printi_str0
81		.int 2
82	.L._printi_str0:
83		.asciz "%d"
84	.text
85	_printi:
86		pushq %rbp
87		movq %rsp, %rbp
88		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
89		andq $-16, %rsp
90		movl %edi, %esi
91		leaq .L._printi_str0(%rip), %rdi
92		# on x86, al represents the number of SIMD registers used as variadic arguments
93		movb $0, %al
94		call printf@plt
95		movq $0, %rdi
96		call fflush@plt
97		movq %rbp, %rsp
98		popq %rbp
99		ret
100	
101	.section .rodata
102	# length of .L._println_str0
103		.int 0
104	.L._println_str0:
105		.asciz ""
106	.text
107	_println:
108		pushq %rbp
109		movq %rsp, %rbp
110		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
111		andq $-16, %rsp
112		leaq .L._println_str0(%rip), %rdi
113		call puts@plt
114		movq $0, %rdi
115		call fflush@plt
116		movq %rbp, %rsp
117		popq %rbp
118		ret
119	
120	_exit:
121		pushq %rbp
122		movq %rsp, %rbp
123		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
124		andq $-16, %rsp
125		call exit@plt
126		movq %rbp, %rsp
127		popq %rbp
128		ret
129	
130	.section .rodata
131	# length of .L._errOverflow_str0
132		.int 52
133	.L._errOverflow_str0:
134		.asciz "fatal error: integer overflow or underflow occurred\n"
135	.text
136	_errOverflow:
137		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
138		andq $-16, %rsp
139		leaq .L._errOverflow_str0(%rip), %rdi
140		call _prints
141		movb $-1, %dil
142		call exit@plt
===========================================================
-- Finished

