./valid/variables/longVarNames.wacc
calling the reference compiler on ./valid/variables/longVarNames.wacc
-- Test: longVarNames.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable can have very long names

# Output:

# Exit:
# 5

# Program:

begin
  int this_variable_has_a_crazy_long_name_but_ought_to_still_be_valid = 5 ;
  exit this_variable_has_a_crazy_long_name_but_ought_to_still_be_valid
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longVarNames.s contents are:
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
11		mov x8, #5
12		mov x19, x8
13		// Stack pointer unchanged, no stack allocated arguments
14		mov x8, x19
15		mov x0, x8
16		// statement primitives do not return results (but will clobber r0/rax)
17		bl exit
18		// Stack pointer unchanged, no stack allocated variables
19		mov x0, #0
20		// pop {x19}
21		ldp x19, xzr, [sp], #16
22		// pop {fp, lr}
23		ldp fp, lr, [sp], #16
24		ret
===========================================================
-- Finished

