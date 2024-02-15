./valid/variables/negIntDeclaration.wacc
calling the reference compiler on ./valid/variables/negIntDeclaration.wacc
-- Test: negIntDeclaration.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple negative integer variable declaration

# Output:

# Program:

begin
  int x = -1
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
negIntDeclaration.s contents are:
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
9		ldr r8, =-1
10		mov r8, r8
11		mov r4, r8
12		@ Stack pointer unchanged, no stack allocated variables
13		mov r0, #0
14		pop {r4, r8, r10, r12}
15		pop {fp, pc}
===========================================================
-- Finished

