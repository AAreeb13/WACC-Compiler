./valid/expressions/modExpr.wacc
calling the reference compiler on ./valid/expressions/modExpr.wacc
-- Test: modExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating modulus

# Output:
# 2
#

# Program:

begin
  int x = 5 ;
  int y = 3 ;
  println x % y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
modExpr.s contents are:
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
11		mov x8, #5
12		mov x19, x8
13		mov x8, #3
14		mov x20, x8
15		// Stack pointer unchanged, no stack allocated arguments
16		cbz x20, _errDivZero
17		sdiv w17, w19, w20
18		msub w8, w17, w20, w19
19		sxtw x8, w8
20		// push {x8}
21		stp x8, xzr, [sp, #-16]!
22		// pop {x8}
23		ldp x8, xzr, [sp], #16
24		mov x8, x8
25		mov x0, x8
26		// statement primitives do not return results (but will clobber r0/rax)
27		bl _printi
28		bl _println
29		// Stack pointer unchanged, no stack allocated variables
30		mov x0, #0
31		// pop {x19, x20}
32		ldp x19, x20, [sp], #16
33		// pop {fp, lr}
34		ldp fp, lr, [sp], #16
35		ret
36	
37	// length of .L._prints_str0
38		.word 4
39	.L._prints_str0:
40		.asciz "%.*s"
41	.align 4
42	_prints:
43		// push {lr}
44		stp lr, xzr, [sp, #-16]!
45		mov x2, x0
46		ldrsw x1, [x0, #-4]
47		adr x0, .L._prints_str0
48		bl printf
49		mov x0, #0
50		bl fflush
51		// pop {lr}
52		ldp lr, xzr, [sp], #16
53		ret
54	
55	// length of .L._printi_str0
56		.word 2
57	.L._printi_str0:
58		.asciz "%d"
59	.align 4
60	_printi:
61		// push {lr}
62		stp lr, xzr, [sp, #-16]!
63		mov x1, x0
64		adr x0, .L._printi_str0
65		bl printf
66		mov x0, #0
67		bl fflush
68		// pop {lr}
69		ldp lr, xzr, [sp], #16
70		ret
71	
72	// length of .L._println_str0
73		.word 0
74	.L._println_str0:
75		.asciz ""
76	.align 4
77	_println:
78		// push {lr}
79		stp lr, xzr, [sp, #-16]!
80		adr x0, .L._println_str0
81		bl puts
82		mov x0, #0
83		bl fflush
84		// pop {lr}
85		ldp lr, xzr, [sp], #16
86		ret
87	
88	// length of .L._errDivZero_str0
89		.word 40
90	.L._errDivZero_str0:
91		.asciz "fatal error: division or modulo by zero\n"
92	.align 4
93	_errDivZero:
94		adr x0, .L._errDivZero_str0
95		bl _prints
96		mov w0, #-1
97		bl exit
===========================================================
-- Finished

