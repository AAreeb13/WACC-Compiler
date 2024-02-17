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
11		mov x8, #10
12		mov x19, x8
13		mov x8, #20
14		mov x19, x8
15		// Stack pointer unchanged, no stack allocated arguments
16		mov x8, x19
17		mov x0, x8
18		// statement primitives do not return results (but will clobber r0/rax)
19		bl exit
20		// Stack pointer unchanged, no stack allocated variables
21		mov x0, #0
22		// pop {x19}
23		ldp x19, xzr, [sp], #16
24		// pop {fp, lr}
25		ldp fp, lr, [sp], #16
26		ret
===========================================================
-- Finished

