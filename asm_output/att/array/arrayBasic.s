./valid/array/arrayBasic.wacc
calling the reference compiler on ./valid/array/arrayBasic.wacc
-- Test: arrayBasic.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array declaration and assignment

# Output:

# Program:

begin
  int[] a = [0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayBasic.s contents are:
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
11		# 1 element array
12		movl $8, %edi
13		call _malloc
14		movq %rax, %r11
15		# array pointers are shifted forwards by 4 bytes (to account for size)
16		movq %r11, %r11
17		addq $4, %r11
18		movq $1, %rax
19		movl %eax, -4(%r11)
20		movq $0, %rax
21		movl %eax, (%r11)
22		movq %r11, %rax
23		movq %rax, %r12
24		# Stack pointer unchanged, no stack allocated variables
25		movq $0, %rax
26		# popq {%rbx, %r12}
27		movq (%rsp), %rbx
28		movq 8(%rsp), %r12
29		addq $16, %rsp
30		popq %rbp
31		ret
32	
33	.section .rodata
34	# length of .L._prints_str0
35		.int 4
36	.L._prints_str0:
37		.asciz "%.*s"
38	.text
39	_prints:
40		pushq %rbp
41		movq %rsp, %rbp
42		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
43		andq $-16, %rsp
44		movq %rdi, %rdx
45		movl -4(%rdi), %esi
46		leaq .L._prints_str0(%rip), %rdi
47		# on x86, al represents the number of SIMD registers used as variadic arguments
48		movb $0, %al
49		call printf@plt
50		movq $0, %rdi
51		call fflush@plt
52		movq %rbp, %rsp
53		popq %rbp
54		ret
55	
56	_malloc:
57		pushq %rbp
58		movq %rsp, %rbp
59		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
60		andq $-16, %rsp
61		call malloc@plt
62		cmpq $0, %rax
63		je _errOutOfMemory
64		movq %rbp, %rsp
65		popq %rbp
66		ret
67	
68	_exit:
69		pushq %rbp
70		movq %rsp, %rbp
71		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
72		andq $-16, %rsp
73		call exit@plt
74		movq %rbp, %rsp
75		popq %rbp
76		ret
77	
78	.section .rodata
79	# length of .L._errOutOfMemory_str0
80		.int 27
81	.L._errOutOfMemory_str0:
82		.asciz "fatal error: out of memory\n"
83	.text
84	_errOutOfMemory:
85		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
86		andq $-16, %rsp
87		leaq .L._errOutOfMemory_str0(%rip), %rdi
88		call _prints
89		movb $-1, %dil
90		call exit@plt
===========================================================
-- Finished

