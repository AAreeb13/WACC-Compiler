./valid/variables/puncCharDeclaration.wacc
calling the reference compiler on ./valid/variables/puncCharDeclaration.wacc
-- Test: puncCharDeclaration.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple punctuation character variable declaration

# Output:

# Program:

begin
  char c = '!'
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
puncCharDeclaration.s contents are:
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
11		mov x8, #33
12		mov x19, x8
13		// Stack pointer unchanged, no stack allocated variables
14		mov x0, #0
15		// pop {x19}
16		ldp x19, xzr, [sp], #16
17		// pop {fp, lr}
18		ldp fp, lr, [sp], #16
19		ret
===========================================================
-- Finished

