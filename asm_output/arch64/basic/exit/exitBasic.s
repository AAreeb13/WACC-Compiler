./valid/basic/exit/exitBasic.wacc
calling the reference compiler on ./valid/basic/exit/exitBasic.wacc
-- Test: exitBasic.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic exit statement

# Output:

# Exit:
# 7

# Program:

begin
  exit 7
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
exitBasic.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		mov fp, sp
8		// Stack pointer unchanged, no stack allocated arguments
9		mov x8, #7
10		mov x0, x8
11		// statement primitives do not return results (but will clobber r0/rax)
12		bl exit
13		mov x0, #0
14		// pop {fp, lr}
15		ldp fp, lr, [sp], #16
16		ret
===========================================================
-- Finished

