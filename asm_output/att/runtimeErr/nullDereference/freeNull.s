./valid/runtimeErr/nullDereference/freeNull.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/freeNull.wacc
-- Test: freeNull.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Create and free a null pair

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
  pair(pair, pair) a = null ;
  free a
end

---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
freeNull.s contents are:
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
11		movq $0, %rax
12		movq %rax, %r12
13		# Stack pointer unchanged, no stack allocated arguments
14		movq %r12, %rax
15		movq %rax, %rdi
16		# statement primitives do not return results (but will clobber r0/rax)
17		call _freepair
18		# Stack pointer unchanged, no stack allocated variables
19		movq $0, %rax
20		# popq {%rbx, %r12}
21		movq (%rsp), %rbx
22		movq 8(%rsp), %r12
23		addq $16, %rsp
24		popq %rbp
25		ret
26	
27	_freepair:
28		pushq %rbp
29		movq %rsp, %rbp
30		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
31		andq $-16, %rsp
32		cmpq $0, %rdi
33		je _errNull
34		call free@plt
35		movq %rbp, %rsp
36		popq %rbp
37		ret
38	
39	.section .rodata
40	# length of .L._prints_str0
41		.int 4
42	.L._prints_str0:
43		.asciz "%.*s"
44	.text
45	_prints:
46		pushq %rbp
47		movq %rsp, %rbp
48		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
49		andq $-16, %rsp
50		movq %rdi, %rdx
51		movl -4(%rdi), %esi
52		leaq .L._prints_str0(%rip), %rdi
53		# on x86, al represents the number of SIMD registers used as variadic arguments
54		movb $0, %al
55		call printf@plt
56		movq $0, %rdi
57		call fflush@plt
58		movq %rbp, %rsp
59		popq %rbp
60		ret
61	
62	_exit:
63		pushq %rbp
64		movq %rsp, %rbp
65		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
66		andq $-16, %rsp
67		call exit@plt
68		movq %rbp, %rsp
69		popq %rbp
70		ret
71	
72	.section .rodata
73	# length of .L._errNull_str0
74		.int 45
75	.L._errNull_str0:
76		.asciz "fatal error: null pair dereferenced or freed\n"
77	.text
78	_errNull:
79		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
80		andq $-16, %rsp
81		leaq .L._errNull_str0(%rip), %rdi
82		call _prints
83		movb $-1, %dil
84		call exit@plt
===========================================================
-- Finished

