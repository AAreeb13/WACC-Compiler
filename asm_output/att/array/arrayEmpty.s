./valid/array/arrayEmpty.wacc
calling the reference compiler on ./valid/array/arrayEmpty.wacc
-- Test: arrayEmpty.wacc

-- Uploaded file: 
---------------------------------------------------------------
# empty array declaration

# Output:

# Program:

begin
  int[] a = []
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayEmpty.s contents are:
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
11		# 0 element array
12		movl $4, %edi
13		call _malloc
14		movq %rax, %r11
15		# array pointers are shifted forwards by 4 bytes (to account for size)
16		movq %r11, %r11
17		addq $4, %r11
18		movq $0, %rax
19		movl %eax, -4(%r11)
20		movq %r11, %rax
21		movq %rax, %r12
22		# Stack pointer unchanged, no stack allocated variables
23		movq $0, %rax
24		# popq {%rbx, %r12}
25		movq (%rsp), %rbx
26		movq 8(%rsp), %r12
27		addq $16, %rsp
28		popq %rbp
29		ret
30	
31	.section .rodata
32	# length of .L._prints_str0
33		.int 4
34	.L._prints_str0:
35		.asciz "%.*s"
36	.text
37	_prints:
38		pushq %rbp
39		movq %rsp, %rbp
40		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
41		andq $-16, %rsp
42		movq %rdi, %rdx
43		movl -4(%rdi), %esi
44		leaq .L._prints_str0(%rip), %rdi
45		# on x86, al represents the number of SIMD registers used as variadic arguments
46		movb $0, %al
47		call printf@plt
48		movq $0, %rdi
49		call fflush@plt
50		movq %rbp, %rsp
51		popq %rbp
52		ret
53	
54	_malloc:
55		pushq %rbp
56		movq %rsp, %rbp
57		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
58		andq $-16, %rsp
59		call malloc@plt
60		cmpq $0, %rax
61		je _errOutOfMemory
62		movq %rbp, %rsp
63		popq %rbp
64		ret
65	
66	_exit:
67		pushq %rbp
68		movq %rsp, %rbp
69		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
70		andq $-16, %rsp
71		call exit@plt
72		movq %rbp, %rsp
73		popq %rbp
74		ret
75	
76	.section .rodata
77	# length of .L._errOutOfMemory_str0
78		.int 27
79	.L._errOutOfMemory_str0:
80		.asciz "fatal error: out of memory\n"
81	.text
82	_errOutOfMemory:
83		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
84		andq $-16, %rsp
85		leaq .L._errOutOfMemory_str0(%rip), %rdi
86		call _prints
87		movb $-1, %dil
88		call exit@plt
===========================================================
-- Finished

