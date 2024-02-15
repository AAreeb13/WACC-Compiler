./valid/expressions/multExpr.wacc
calling the reference compiler on ./valid/expressions/multExpr.wacc
-- Test: multExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating multiplication

# Output:
# 15
#

# Program:

begin
  int x = 5 ;
  int y = 3 ;
  println x * y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
multExpr.s contents are:
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
16		smull x8, w19, w20
17		// take the 31st bit, sign extend it to 64 bits
18		sbfx x9, x8, #31, #1
19		// now take the top 32 bits of the result, shift and sign extend to 64 bits
20		cmp x9, x8, asr #32
21		// if they are not equal then overflow occured
22		b.ne _errOverflow
23		sxtw x8, w8
24		// push {x8}
25		stp x8, xzr, [sp, #-16]!
26		// pop {x8}
27		ldp x8, xzr, [sp], #16
28		mov x8, x8
29		mov x0, x8
30		// statement primitives do not return results (but will clobber r0/rax)
31		bl _printi
32		bl _println
33		// Stack pointer unchanged, no stack allocated variables
34		mov x0, #0
35		// pop {x19, x20}
36		ldp x19, x20, [sp], #16
37		// pop {fp, lr}
38		ldp fp, lr, [sp], #16
39		ret
40	
41	// length of .L._prints_str0
42		.word 4
43	.L._prints_str0:
44		.asciz "%.*s"
45	.align 4
46	_prints:
47		// push {lr}
48		stp lr, xzr, [sp, #-16]!
49		mov x2, x0
50		ldrsw x1, [x0, #-4]
51		adr x0, .L._prints_str0
52		bl printf
53		mov x0, #0
54		bl fflush
55		// pop {lr}
56		ldp lr, xzr, [sp], #16
57		ret
58	
59	// length of .L._printi_str0
60		.word 2
61	.L._printi_str0:
62		.asciz "%d"
63	.align 4
64	_printi:
65		// push {lr}
66		stp lr, xzr, [sp, #-16]!
67		mov x1, x0
68		adr x0, .L._printi_str0
69		bl printf
70		mov x0, #0
71		bl fflush
72		// pop {lr}
73		ldp lr, xzr, [sp], #16
74		ret
75	
76	// length of .L._println_str0
77		.word 0
78	.L._println_str0:
79		.asciz ""
80	.align 4
81	_println:
82		// push {lr}
83		stp lr, xzr, [sp, #-16]!
84		adr x0, .L._println_str0
85		bl puts
86		mov x0, #0
87		bl fflush
88		// pop {lr}
89		ldp lr, xzr, [sp], #16
90		ret
91	
92	// length of .L._errOverflow_str0
93		.word 52
94	.L._errOverflow_str0:
95		.asciz "fatal error: integer overflow or underflow occurred\n"
96	.align 4
97	_errOverflow:
98		adr x0, .L._errOverflow_str0
99		bl _prints
100		mov w0, #-1
101		bl exit
===========================================================
-- Finished

