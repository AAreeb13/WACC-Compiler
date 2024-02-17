./valid/variables/stringDeclaration.wacc
calling the reference compiler on ./valid/variables/stringDeclaration.wacc
-- Test: stringDeclaration.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple string variable declaration

# Output:

# Program:

begin
  string s = "Hello World!"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
stringDeclaration.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 12
3	.L.str0:
4		.asciz "Hello World!"
5	.align 4
6	.text
7	.global main
8	main:
9		push {fp, lr}
10		push {r4, r8, r10, r12}
11		mov fp, sp
12		@ Stack pointer unchanged, no stack allocated variables
13		ldr r8, =.L.str0
14		push {r8}
15		pop {r8}
16		mov r8, r8
17		mov r4, r8
18		@ Stack pointer unchanged, no stack allocated variables
19		mov r0, #0
20		pop {r4, r8, r10, r12}
21		pop {fp, pc}
===========================================================
-- Finished

