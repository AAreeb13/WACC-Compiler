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
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		mov fp, sp
8		mov x0, #0
9		// pop {fp, lr}
10		ldp fp, lr, [sp], #16
11		ret
===========================================================
-- Finished

