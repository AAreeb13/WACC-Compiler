./valid/runtimeErr/divideByZero/divideByZero.wacc
calling the reference compiler on ./valid/runtimeErr/divideByZero/divideByZero.wacc
-- Test: divideByZero.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt divide by zero

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	int x = 10 ;
	int y = 0 ;
	print x / y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
divideByZero.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19, x20}
8		stp x19, x20, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		mov x8, #10
12		mov x19, x8
13		mov x8, #0
14		mov x20, x8
15		// Stack pointer unchanged, no stack allocated arguments
16		cbz x20, _errDivZero
17		sdiv w8, w19, w20
18		sxtw x8, w8
19		// push {x8}
20		stp x8, xzr, [sp, #-16]!
21		// pop {x8}
22		ldp x8, xzr, [sp], #16
23		mov x8, x8
24		mov x0, x8
25		// statement primitives do not return results (but will clobber r0/rax)
26		bl _printi
27		// Stack pointer unchanged, no stack allocated variables
28		mov x0, #0
29		// pop {x19, x20}
30		ldp x19, x20, [sp], #16
31		// pop {fp, lr}
32		ldp fp, lr, [sp], #16
33		ret
34	
35	// length of .L._prints_str0
36		.word 4
37	.L._prints_str0:
38		.asciz "%.*s"
39	.align 4
40	_prints:
41		// push {lr}
42		stp lr, xzr, [sp, #-16]!
43		mov x2, x0
44		ldrsw x1, [x0, #-4]
45		adr x0, .L._prints_str0
46		bl printf
47		mov x0, #0
48		bl fflush
49		// pop {lr}
50		ldp lr, xzr, [sp], #16
51		ret
52	
53	// length of .L._printi_str0
54		.word 2
55	.L._printi_str0:
56		.asciz "%d"
57	.align 4
58	_printi:
59		// push {lr}
60		stp lr, xzr, [sp, #-16]!
61		mov x1, x0
62		adr x0, .L._printi_str0
63		bl printf
64		mov x0, #0
65		bl fflush
66		// pop {lr}
67		ldp lr, xzr, [sp], #16
68		ret
69	
70	// length of .L._errDivZero_str0
71		.word 40
72	.L._errDivZero_str0:
73		.asciz "fatal error: division or modulo by zero\n"
74	.align 4
75	_errDivZero:
76		adr x0, .L._errDivZero_str0
77		bl _prints
78		mov w0, #-1
79		bl exit
===========================================================
-- Finished

