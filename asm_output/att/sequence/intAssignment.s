./valid/sequence/intAssignment.wacc
calling the reference compiler on ./valid/sequence/intAssignment.wacc
-- Test: intAssignment.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple integer variable declaration and assignment

# Output:

# Exit:
# 20

# Program:

begin
  int x = 10 ;
  x = 20 ;
  exit x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intAssignment.s contents are:
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
11		movq $10, %rax
12		movq %rax, %r12
13		movq $20, %rax
14		movq %rax, %r12
15		# Stack pointer unchanged, no stack allocated arguments
16		movq %r12, %rax
17		movq %rax, %rdi
18		# statement primitives do not return results (but will clobber r0/rax)
19		call _exit
20		# Stack pointer unchanged, no stack allocated variables
21		movq $0, %rax
22		# popq {%rbx, %r12}
23		movq (%rsp), %rbx
24		movq 8(%rsp), %r12
25		addq $16, %rsp
26		popq %rbp
27		ret
28	
29	_exit:
30		pushq %rbp
31		movq %rsp, %rbp
32		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
33		andq $-16, %rsp
34		call exit@plt
35		movq %rbp, %rsp
36		popq %rbp
37		ret
===========================================================
-- Finished

