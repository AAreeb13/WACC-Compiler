./valid/runtimeErr/nullDereference/useNull2.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/useNull2.wacc
-- Test: useNull2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt dereference of a null pair by using it

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	pair(int, int) p = null ;
	int x = snd p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
useNull2.s contents are:
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
12		movq $0, %rax
13		movq %rax, %r12
14		cmpq $0, %r12
15		je _errNull
16		movq 8(%r12), %rax
17		movq %rax, %rax
18		movq %rax, %r13
19		# Stack pointer unchanged, no stack allocated variables
20		movq $0, %rax
21		# popq {%rbx, %r12, %r13}
22		movq (%rsp), %rbx
23		movq 8(%rsp), %r12
24		movq 16(%rsp), %r13
25		addq $24, %rsp
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
52	_exit:
53		pushq %rbp
54		movq %rsp, %rbp
55		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
56		andq $-16, %rsp
57		call exit@plt
58		movq %rbp, %rsp
59		popq %rbp
60		ret
61	
62	.section .rodata
63	# length of .L._errNull_str0
64		.int 45
65	.L._errNull_str0:
66		.asciz "fatal error: null pair dereferenced or freed\n"
67	.text
68	_errNull:
69		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
70		andq $-16, %rsp
71		leaq .L._errNull_str0(%rip), %rdi
72		call _prints
73		movb $-1, %dil
74		call exit@plt
===========================================================
-- Finished

