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
1	// length of .L.str0
2		.word 3
3	.L.str0:
4		.asciz "foo"
5	// length of .L.str1
6		.word 3
7	.L.str1:
8		.asciz "bar"
9	.align 4
10	.text
11	.global main
12	main:
13		// push {fp, lr}
14		stp fp, lr, [sp, #-16]!
15		// push {x19}
16		stp x19, xzr, [sp, #-16]!
17		mov fp, sp
18		// Stack pointer unchanged, no stack allocated variables
19		adrp x8, .L.str0
20		add x8, x8, :lo12:.L.str0
21		// push {x8}
22		stp x8, xzr, [sp, #-16]!
23		// pop {x8}
24		ldp x8, xzr, [sp], #16
25		mov x8, x8
26		mov x19, x8
27		adrp x8, .L.str1
28		add x8, x8, :lo12:.L.str1
29		// push {x8}
30		stp x8, xzr, [sp, #-16]!
31		// pop {x8}
32		ldp x8, xzr, [sp], #16
33		mov x8, x8
34		mov x19, x8
35		// Stack pointer unchanged, no stack allocated variables
36		mov x0, #0
37		// pop {x19}
38		ldp x19, xzr, [sp], #16
39		// pop {fp, lr}
40		ldp fp, lr, [sp], #16
41		ret
===========================================================
-- Finished

