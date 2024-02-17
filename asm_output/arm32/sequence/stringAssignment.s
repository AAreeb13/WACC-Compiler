./valid/sequence/stringAssignment.wacc
calling the reference compiler on ./valid/sequence/stringAssignment.wacc
-- Test: stringAssignment.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple string variable declaration and assignment

# Output:

# Program:

begin
  string s = "foo" ;
  s = "bar"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
stringAssignment.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 3
3	.L.str0:
4		.asciz "foo"
5	@ length of .L.str1
6		.word 3
7	.L.str1:
8		.asciz "bar"
9	.align 4
10	.text
11	.global main
12	main:
13		push {fp, lr}
14		push {r4, r8, r10, r12}
15		mov fp, sp
16		@ Stack pointer unchanged, no stack allocated variables
17		ldr r8, =.L.str0
18		push {r8}
19		pop {r8}
20		mov r8, r8
21		mov r4, r8
22		ldr r8, =.L.str1
23		push {r8}
24		pop {r8}
25		mov r8, r8
26		mov r4, r8
27		@ Stack pointer unchanged, no stack allocated variables
28		mov r0, #0
29		pop {r4, r8, r10, r12}
30		pop {fp, pc}
===========================================================
-- Finished

