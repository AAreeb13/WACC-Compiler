./valid/expressions/negDividendDiv.wacc
calling the reference compiler on ./valid/expressions/negDividendDiv.wacc
-- Test: negDividendDiv.wacc

-- Uploaded file: 
---------------------------------------------------------------
# division of a negative number

# Output:
# -2
#

# Program:

begin
  int x = -4 ;
  int y = 2 ;
  println x / y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
negDividendDiv.s contents are:
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
11		mov x8, #-4
12		mov x19, x8
13		mov x8, #2
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
27		bl _println
28		// Stack pointer unchanged, no stack allocated variables
29		mov x0, #0
30		// pop {x19, x20}
31		ldp x19, x20, [sp], #16
32		// pop {fp, lr}
33		ldp fp, lr, [sp], #16
34		ret
35	
36	// length of .L._prints_str0
37		.word 4
38	.L._prints_str0:
39		.asciz "%.*s"
40	.align 4
41	_prints:
42		// push {lr}
43		stp lr, xzr, [sp, #-16]!
44		mov x2, x0
45		ldrsw x1, [x0, #-4]
46		adr x0, .L._prints_str0
47		bl printf
48		mov x0, #0
49		bl fflush
50		// pop {lr}
51		ldp lr, xzr, [sp], #16
52		ret
53	
54	// length of .L._printi_str0
55		.word 2
56	.L._printi_str0:
57		.asciz "%d"
58	.align 4
59	_printi:
60		// push {lr}
61		stp lr, xzr, [sp, #-16]!
62		mov x1, x0
63		adr x0, .L._printi_str0
64		bl printf
65		mov x0, #0
66		bl fflush
67		// pop {lr}
68		ldp lr, xzr, [sp], #16
69		ret
70	
71	// length of .L._println_str0
72		.word 0
73	.L._println_str0:
74		.asciz ""
75	.align 4
76	_println:
77		// push {lr}
78		stp lr, xzr, [sp, #-16]!
79		adr x0, .L._println_str0
80		bl puts
81		mov x0, #0
82		bl fflush
83		// pop {lr}
84		ldp lr, xzr, [sp], #16
85		ret
86	
87	// length of .L._errDivZero_str0
88		.word 40
89	.L._errDivZero_str0:
90		.asciz "fatal error: division or modulo by zero\n"
91	.align 4
92	_errDivZero:
93		adr x0, .L._errDivZero_str0
94		bl _prints
95		mov w0, #-1
96		bl exit
===========================================================
-- Finished

