./valid/basic/skip/commentEoF.wacc
calling the reference compiler on ./valid/basic/skip/commentEoF.wacc
-- Test: commentEoF.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple skip program with comment line ended by EoF not EoL

# Output:

# Program:

begin 
  skip 
end
# I can end a file with a comment
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
commentEoF.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		pushq %rbx
6		movq %rsp, %rbp
7		movq $0, %rax
8		popq %rbx
9		popq %rbp
10		ret
===========================================================
-- Finished

