./valid/sequence/stringAssignment.wacc
calling the reference compiler on ./valid/sequence/stringAssignment.wacc
-- Test: stringAssignment.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple string variable declaration and assignment

# Output:

# Program:

begin
  string s = "foo" ;
  s = "bar"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
stringAssignment.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 3
4	.L.str0:
5		.asciz "foo"
6	# length of .L.str1
7		.int 3
8	.L.str1:
9		.asciz "bar"
10	.text
11	main:
12		pushq %rbp
13		# pushq {%rbx, %r12}
14		subq $16, %rsp
15		movq %rbx, (%rsp)
16		movq %r12, 8(%rsp)
17		movq %rsp, %rbp
18		# Stack pointer unchanged, no stack allocated variables
19		leaq .L.str0(%rip), %rax
20		pushq %rax
21		popq %rax
22		movq %rax, %rax
23		movq %rax, %r12
24		leaq .L.str1(%rip), %rax
25		pushq %rax
26		popq %rax
27		movq %rax, %rax
28		movq %rax, %r12
29		# Stack pointer unchanged, no stack allocated variables
30		movq $0, %rax
31		# popq {%rbx, %r12}
32		movq (%rsp), %rbx
33		movq 8(%rsp), %r12
34		addq $16, %rsp
35		popq %rbp
36		ret
===========================================================
-- Finished

