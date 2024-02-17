./valid/IO/read/read.wacc
calling the reference compiler on ./valid/IO/read/read.wacc
-- Test: read.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple read statement

#Input: 350

# Output:
# input an integer to continue...
#

# Program:

begin
	int x = 10;
  println "input an integer to continue..." ;
	read x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
read.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 31
4	.L.str0:
5		.asciz "input an integer to continue..."
6	.text
7	main:
8		pushq %rbp
9		# pushq {%rbx, %r12}
10		subq $16, %rsp
11		movq %rbx, (%rsp)
12		movq %r12, 8(%rsp)
13		movq %rsp, %rbp
14		# Stack pointer unchanged, no stack allocated variables
15		movq $10, %rax
16		movq %rax, %r12
17		# Stack pointer unchanged, no stack allocated arguments
18		leaq .L.str0(%rip), %rax
19		pushq %rax
20		popq %rax
21		movq %rax, %rax
22		movq %rax, %rdi
23		# statement primitives do not return results (but will clobber r0/rax)
24		call _prints
25		call _println
26		# Stack pointer unchanged, no stack allocated arguments
27		# load the current value in the destination of the read so it supports defaults
28		movq %r12, %rax
29		movq %rax, %rdi
30		call _readi
31		movq %rax, %r11
32		movq %r11, %rax
33		movq %rax, %r12
34		# Stack pointer unchanged, no stack allocated variables
35		movq $0, %rax
36		# popq {%rbx, %r12}
37		movq (%rsp), %rbx
38		movq 8(%rsp), %r12
39		addq $16, %rsp
40		popq %rbp
41		ret
42	
43	.section .rodata
44	# length of .L._prints_str0
45		.int 4
46	.L._prints_str0:
47		.asciz "%.*s"
48	.text
49	_prints:
50		pushq %rbp
51		movq %rsp, %rbp
52		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
53		andq $-16, %rsp
54		movq %rdi, %rdx
55		movl -4(%rdi), %esi
56		leaq .L._prints_str0(%rip), %rdi
57		# on x86, al represents the number of SIMD registers used as variadic arguments
58		movb $0, %al
59		call printf@plt
60		movq $0, %rdi
61		call fflush@plt
62		movq %rbp, %rsp
63		popq %rbp
64		ret
65	
66	.section .rodata
67	# length of .L._readi_str0
68		.int 2
69	.L._readi_str0:
70		.asciz "%d"
71	.text
72	_readi:
73		pushq %rbp
74		movq %rsp, %rbp
75		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
76		andq $-16, %rsp
77		# RDI contains the "original" value of the destination of the read
78		# allocate space on the stack to store the read: preserve alignment!
79		# the passed default argument should be stored in case of EOF
80		subq $16, %rsp
81		movl %edi, (%rsp)
82		leaq (%rsp), %rsi
83		leaq .L._readi_str0(%rip), %rdi
84		# on x86, al represents the number of SIMD registers used as variadic arguments
85		movb $0, %al
86		call scanf@plt
87		movslq (%rsp), %rax
88		addq $16, %rsp
89		movq %rbp, %rsp
90		popq %rbp
91		ret
92	
93	.section .rodata
94	# length of .L._println_str0
95		.int 0
96	.L._println_str0:
97		.asciz ""
98	.text
99	_println:
100		pushq %rbp
101		movq %rsp, %rbp
102		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
103		andq $-16, %rsp
104		leaq .L._println_str0(%rip), %rdi
105		call puts@plt
106		movq $0, %rdi
107		call fflush@plt
108		movq %rbp, %rsp
109		popq %rbp
110		ret
===========================================================
-- Finished

