./valid/runtimeErr/nullDereference/setNull1.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/setNull1.wacc
-- Test: setNull1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt dereference of a null pair by setting an element of it

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	pair(int, int) p = null ;
	fst p = 1
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
setNull1.s contents are:
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
13		cmpq $0, %r12
14		je _errNull
15		movq $1, %rax
16		movq %rax, (%r12)
17		# Stack pointer unchanged, no stack allocated variables
18		movq $0, %rax
19		# popq {%rbx, %r12}
20		movq (%rsp), %rbx
21		movq 8(%rsp), %r12
22		addq $16, %rsp
23		popq %rbp
24		ret
25	
26	.section .rodata
27	# length of .L._prints_str0
28		.int 4
29	.L._prints_str0:
30		.asciz "%.*s"
31	.text
32	_prints:
33		pushq %rbp
34		movq %rsp, %rbp
35		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
36		andq $-16, %rsp
37		movq %rdi, %rdx
38		movl -4(%rdi), %esi
39		leaq .L._prints_str0(%rip), %rdi
40		# on x86, al represents the number of SIMD registers used as variadic arguments
41		movb $0, %al
42		call printf@plt
43		movq $0, %rdi
44		call fflush@plt
45		movq %rbp, %rsp
46		popq %rbp
47		ret
48	
49	_exit:
50		pushq %rbp
51		movq %rsp, %rbp
52		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
53		andq $-16, %rsp
54		call exit@plt
55		movq %rbp, %rsp
56		popq %rbp
57		ret
58	
59	.section .rodata
60	# length of .L._errNull_str0
61		.int 45
62	.L._errNull_str0:
63		.asciz "fatal error: null pair dereferenced or freed\n"
64	.text
65	_errNull:
66		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
67		andq $-16, %rsp
68		leaq .L._errNull_str0(%rip), %rdi
69		call _prints
70		movb $-1, %dil
71		call exit@plt
===========================================================
-- Finished

