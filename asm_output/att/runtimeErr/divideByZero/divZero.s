./valid/runtimeErr/divideByZero/divZero.wacc
calling the reference compiler on ./valid/runtimeErr/divideByZero/divZero.wacc
-- Test: divZero.wacc

-- Uploaded file: 
---------------------------------------------------------------
# division by zero

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = 10 / 0 ;
  println "should not reach here"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
divZero.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 21
4	.L.str0:
5		.asciz "should not reach here"
6	.text
7	main:
8		pushq %rbp
9		# pushq {%rbx, %r12}
10		subq $16, %rsp
11		movq %rbx, (%rsp)
12		movq %r12, 8(%rsp)
13		movq %rsp, %rbp
14		# Stack pointer unchanged, no stack allocated variables
15		movq $0, %rbx
16		movl $10, %eax
17		cmpl $0, %ebx
18		je _errDivZero
19		# sign extend EAX into EDX
20		cltd
21		idivl %ebx
22		movl %eax, %eax
23		movl %eax, %eax
24		movslq %eax, %rax
25		pushq %rax
26		popq %rax
27		movq %rax, %rax
28		movq %rax, %r12
29		# Stack pointer unchanged, no stack allocated arguments
30		leaq .L.str0(%rip), %rax
31		pushq %rax
32		popq %rax
33		movq %rax, %rax
34		movq %rax, %rdi
35		# statement primitives do not return results (but will clobber r0/rax)
36		call _prints
37		call _println
38		# Stack pointer unchanged, no stack allocated variables
39		movq $0, %rax
40		# popq {%rbx, %r12}
41		movq (%rsp), %rbx
42		movq 8(%rsp), %r12
43		addq $16, %rsp
44		popq %rbp
45		ret
46	
47	.section .rodata
48	# length of .L._prints_str0
49		.int 4
50	.L._prints_str0:
51		.asciz "%.*s"
52	.text
53	_prints:
54		pushq %rbp
55		movq %rsp, %rbp
56		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
57		andq $-16, %rsp
58		movq %rdi, %rdx
59		movl -4(%rdi), %esi
60		leaq .L._prints_str0(%rip), %rdi
61		# on x86, al represents the number of SIMD registers used as variadic arguments
62		movb $0, %al
63		call printf@plt
64		movq $0, %rdi
65		call fflush@plt
66		movq %rbp, %rsp
67		popq %rbp
68		ret
69	
70	.section .rodata
71	# length of .L._println_str0
72		.int 0
73	.L._println_str0:
74		.asciz ""
75	.text
76	_println:
77		pushq %rbp
78		movq %rsp, %rbp
79		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
80		andq $-16, %rsp
81		leaq .L._println_str0(%rip), %rdi
82		call puts@plt
83		movq $0, %rdi
84		call fflush@plt
85		movq %rbp, %rsp
86		popq %rbp
87		ret
88	
89	_exit:
90		pushq %rbp
91		movq %rsp, %rbp
92		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
93		andq $-16, %rsp
94		call exit@plt
95		movq %rbp, %rsp
96		popq %rbp
97		ret
98	
99	.section .rodata
100	# length of .L._errDivZero_str0
101		.int 40
102	.L._errDivZero_str0:
103		.asciz "fatal error: division or modulo by zero\n"
104	.text
105	_errDivZero:
106		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
107		andq $-16, %rsp
108		leaq .L._errDivZero_str0(%rip), %rdi
109		call _prints
110		movb $-1, %dil
111		call exit@plt
===========================================================
-- Finished

