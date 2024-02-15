./valid/pairs/createRefPair.wacc
calling the reference compiler on ./valid/pairs/createRefPair.wacc
-- Test: createRefPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair(int, char) with a second reference to it

# Output:

# Program:

begin
  pair(int, char) p = newpair(10, 'a') ;
  pair(int, char) q = p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
createRefPair.s contents are:
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
12		movl $16, %edi
13		call _malloc
14		movq %rax, %r11
15		movq $10, %rax
16		movq %rax, (%r11)
17		movq $97, %rax
18		movq %rax, 8(%r11)
19		movq %r11, %rax
20		movq %rax, %r12
21		movq %r12, %rax
22		movq %rax, %r13
23		# Stack pointer unchanged, no stack allocated variables
24		movq $0, %rax
25		# popq {%rbx, %r12, %r13}
26		movq (%rsp), %rbx
27		movq 8(%rsp), %r12
28		movq 16(%rsp), %r13
29		addq $24, %rsp
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

