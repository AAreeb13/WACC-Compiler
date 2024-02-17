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
1	// length of .L.str0
2		.word 12
3	.L.str0:
4		.asciz "Hello World!"
5	.align 4
6	.text
7	.global main
8	main:
9		// push {fp, lr}
10		stp fp, lr, [sp, #-16]!
11		// push {x19}
12		stp x19, xzr, [sp, #-16]!
13		mov fp, sp
14		// Stack pointer unchanged, no stack allocated variables
15		adrp x8, .L.str0
16		add x8, x8, :lo12:.L.str0
17		// push {x8}
18		stp x8, xzr, [sp, #-16]!
19		// pop {x8}
20		ldp x8, xzr, [sp], #16
21		mov x8, x8
22		mov x19, x8
23		// Stack pointer unchanged, no stack allocated variables
24		mov x0, #0
25		// pop {x19}
26		ldp x19, xzr, [sp], #16
27		// pop {fp, lr}
28		ldp fp, lr, [sp], #16
29		ret
===========================================================
-- Finished

