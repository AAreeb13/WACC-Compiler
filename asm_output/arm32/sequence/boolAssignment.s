./valid/sequence/boolAssignment.wacc
calling the reference compiler on ./valid/sequence/boolAssignment.wacc
-- Test: boolAssignment.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple boolean variable declaration and assignment

# Output:

# Program:

begin
  bool b = false ;
  b = true
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
boolAssignment.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		mov r8, #0
10		mov r4, r8
11		mov r8, #1
12		mov r4, r8
13		@ Stack pointer unchanged, no stack allocated variables
14		mov r0, #0
15		pop {r4, r8, r10, r12}
16		pop {fp, pc}
===========================================================
-- Finished

