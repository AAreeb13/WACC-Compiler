./valid/array/emptyArrayNextLine.wacc
calling the reference compiler on ./valid/array/emptyArrayNextLine.wacc
-- Test: emptyArrayNextLine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# This should work just fine

# Output:

# Program:

begin
  int[] x = [] ;
  bool y = true
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayNextLine.s contents are:
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
12		# 0 element array
13		movl $4, %edi
14		call _malloc
15		movq %rax, %r11
16		# array pointers are shifted forwards by 4 bytes (to account for size)
17		movq %r11, %r11
18		addq $4, %r11
19		movq $0, %rax
20		movl %eax, -4(%r11)
21		movq %r11, %rax
22		movq %rax, %r12
23		movq $1, %rax
24		movq %rax, %r13
25		# Stack pointer unchanged, no stack allocated variables
26		movq $0, %rax
27		# popq {%rbx, %r12, %r13}
28		movq (%rsp), %rbx
29		movq 8(%rsp), %r12
30		movq 16(%rsp), %r13
31		addq $24, %rsp
32		popq %rbp
33		ret
34	
35	.section .rodata
36	# length of .L._prints_str0
37		.int 4
38	.L._prints_str0:
39		.asciz "%.*s"
40	.text
41	_prints:
42		pushq %rbp
43		movq %rsp, %rbp
44		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
45		andq $-16, %rsp
46		movq %rdi, %rdx
47		movl -4(%rdi), %esi
48		leaq .L._prints_str0(%rip), %rdi
49		# on x86, al represents the number of SIMD registers used as variadic arguments
50		movb $0, %al
51		call printf@plt
52		movq $0, %rdi
53		call fflush@plt
54		movq %rbp, %rsp
55		popq %rbp
56		ret
57	
58	_malloc:
59		pushq %rbp
60		movq %rsp, %rbp
61		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
62		andq $-16, %rsp
63		call malloc@plt
64		cmpq $0, %rax
65		je _errOutOfMemory
66		movq %rbp, %rsp
67		popq %rbp
68		ret
69	
70	_exit:
71		pushq %rbp
72		movq %rsp, %rbp
73		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
74		andq $-16, %rsp
75		call exit@plt
76		movq %rbp, %rsp
77		popq %rbp
78		ret
79	
80	.section .rodata
81	# length of .L._errOutOfMemory_str0
82		.int 27
83	.L._errOutOfMemory_str0:
84		.asciz "fatal error: out of memory\n"
85	.text
86	_errOutOfMemory:
87		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
88		andq $-16, %rsp
89		leaq .L._errOutOfMemory_str0(%rip), %rdi
90		call _prints
91		movb $-1, %dil
92		call exit@plt
===========================================================
-- Finished

