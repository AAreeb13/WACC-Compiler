./valid/pairs/createPair.wacc
calling the reference compiler on ./valid/pairs/createPair.wacc
-- Test: createPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair (int, int)

# Output:

# Program:

begin
  pair(int, int) p = newpair(10, 3)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
createPair.s contents are:
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
11		movl $16, %edi
12		call _malloc
13		movq %rax, %r11
14		movq $10, %rax
15		movq %rax, (%r11)
16		movq $3, %rax
17		movq %rax, 8(%r11)
18		movq %r11, %rax
19		movq %rax, %r12
20		# Stack pointer unchanged, no stack allocated variables
21		movq $0, %rax
22		# popq {%rbx, %r12}
23		movq (%rsp), %rbx
24		movq 8(%rsp), %r12
25		addq $16, %rsp
26		popq %rbp
27		ret
28	
29	.section .rodata
30	# length of .L._prints_str0
31		.int 4
32	.L._prints_str0:
33		.asciz "%.*s"
34	.text
35	_prints:
36		pushq %rbp
37		movq %rsp, %rbp
38		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
39		andq $-16, %rsp
40		movq %rdi, %rdx
41		movl -4(%rdi), %esi
42		leaq .L._prints_str0(%rip), %rdi
43		# on x86, al represents the number of SIMD registers used as variadic arguments
44		movb $0, %al
45		call printf@plt
46		movq $0, %rdi
47		call fflush@plt
48		movq %rbp, %rsp
49		popq %rbp
50		ret
51	
52	_malloc:
53		pushq %rbp
54		movq %rsp, %rbp
55		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
56		andq $-16, %rsp
57		call malloc@plt
58		cmpq $0, %rax
59		je _errOutOfMemory
60		movq %rbp, %rsp
61		popq %rbp
62		ret
63	
64	_exit:
65		pushq %rbp
66		movq %rsp, %rbp
67		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
68		andq $-16, %rsp
69		call exit@plt
70		movq %rbp, %rsp
71		popq %rbp
72		ret
73	
74	.section .rodata
75	# length of .L._errOutOfMemory_str0
76		.int 27
77	.L._errOutOfMemory_str0:
78		.asciz "fatal error: out of memory\n"
79	.text
80	_errOutOfMemory:
81		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
82		andq $-16, %rsp
83		leaq .L._errOutOfMemory_str0(%rip), %rdi
84		call _prints
85		movb $-1, %dil
86		call exit@plt
===========================================================
-- Finished

