./valid/sequence/charAssignment.wacc
calling the reference compiler on ./valid/sequence/charAssignment.wacc
-- Test: charAssignment.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple character variable declaration and assignment

# Output:

# Program:

begin
  char c = 'a' ;
  c = 'Z'
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
charAssignment.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19}
8		stp x19, xzr, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		mov x8, #97
12		mov x19, x8
13		mov x8, #90
14		mov x19, x8
15		// Stack pointer unchanged, no stack allocated variables
16		mov x0, #0
17		// pop {x19}
18		ldp x19, xzr, [sp], #16
19		// pop {fp, lr}
20		ldp fp, lr, [sp], #16
21		ret
===========================================================
-- Finished

