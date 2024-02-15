./valid/variables/boolDeclaration2.wacc
calling the reference compiler on ./valid/variables/boolDeclaration2.wacc
-- Test: boolDeclaration2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple true boolean variable declaration

# Output:

# Program:

begin
  bool b = true
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
boolDeclaration2.s contents are:
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
11		movq $1, %rax
12		movq %rax, %r12
13		# Stack pointer unchanged, no stack allocated variables
14		movq $0, %rax
15		# popq {%rbx, %r12}
16		movq (%rsp), %rbx
17		movq 8(%rsp), %r12
18		addq $16, %rsp
19		popq %rbp
20		ret
===========================================================
-- Finished

