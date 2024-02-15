./valid/expressions/negExpr.wacc
calling the reference compiler on ./valid/expressions/negExpr.wacc
-- Test: negExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating negation

# Output:
# -42
#

# Program:

begin
	int x = 42 ;
  println -x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
negExpr.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19}
8		stp x19, xzr, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		mov x8, #42
12		mov x19, x8
13		// Stack pointer unchanged, no stack allocated arguments
14		mov w8, #0
15		subs w8, w8, w19
16		b.vs _errOverflow
17		sxtw x8, w8
18		// push {x8}
19		stp x8, xzr, [sp, #-16]!
20		// pop {x8}
21		ldp x8, xzr, [sp], #16
22		mov x8, x8
23		mov x0, x8
24		// statement primitives do not return results (but will clobber r0/rax)
25		bl _printi
26		bl _println
27		// Stack pointer unchanged, no stack allocated variables
28		mov x0, #0
29		// pop {x19}
30		ldp x19, xzr, [sp], #16
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
70	// length of .L._println_str0
71		.word 0
72	.L._println_str0:
73		.asciz ""
74	.align 4
75	_println:
76		// push {lr}
77		stp lr, xzr, [sp, #-16]!
78		adr x0, .L._println_str0
79		bl puts
80		mov x0, #0
81		bl fflush
82		// pop {lr}
83		ldp lr, xzr, [sp], #16
84		ret
85	
86	// length of .L._errOverflow_str0
87		.word 52
88	.L._errOverflow_str0:
89		.asciz "fatal error: integer overflow or underflow occurred\n"
90	.align 4
91	_errOverflow:
92		adr x0, .L._errOverflow_str0
93		bl _prints
94		mov w0, #-1
95		bl exit
===========================================================
-- Finished

