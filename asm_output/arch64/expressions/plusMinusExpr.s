./valid/expressions/plusMinusExpr.wacc
calling the reference compiler on ./valid/expressions/plusMinusExpr.wacc
-- Test: plusMinusExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# +- should be recognised as two separate symbols

# Output:
# -1
#

# Program:

begin
  println 1+-2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
plusMinusExpr.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		mov fp, sp
8		// Stack pointer unchanged, no stack allocated arguments
9		mov x9, #-2
10		mov w8, #1
11		adds w8, w8, w9
12		b.vs _errOverflow
13		sxtw x8, w8
14		// push {x8}
15		stp x8, xzr, [sp, #-16]!
16		// pop {x8}
17		ldp x8, xzr, [sp], #16
18		mov x8, x8
19		mov x0, x8
20		// statement primitives do not return results (but will clobber r0/rax)
21		bl _printi
22		bl _println
23		mov x0, #0
24		// pop {fp, lr}
25		ldp fp, lr, [sp], #16
26		ret
27	
28	// length of .L._prints_str0
29		.word 4
30	.L._prints_str0:
31		.asciz "%.*s"
32	.align 4
33	_prints:
34		// push {lr}
35		stp lr, xzr, [sp, #-16]!
36		mov x2, x0
37		ldrsw x1, [x0, #-4]
38		adr x0, .L._prints_str0
39		bl printf
40		mov x0, #0
41		bl fflush
42		// pop {lr}
43		ldp lr, xzr, [sp], #16
44		ret
45	
46	// length of .L._printi_str0
47		.word 2
48	.L._printi_str0:
49		.asciz "%d"
50	.align 4
51	_printi:
52		// push {lr}
53		stp lr, xzr, [sp, #-16]!
54		mov x1, x0
55		adr x0, .L._printi_str0
56		bl printf
57		mov x0, #0
58		bl fflush
59		// pop {lr}
60		ldp lr, xzr, [sp], #16
61		ret
62	
63	// length of .L._println_str0
64		.word 0
65	.L._println_str0:
66		.asciz ""
67	.align 4
68	_println:
69		// push {lr}
70		stp lr, xzr, [sp, #-16]!
71		adr x0, .L._println_str0
72		bl puts
73		mov x0, #0
74		bl fflush
75		// pop {lr}
76		ldp lr, xzr, [sp], #16
77		ret
78	
79	// length of .L._errOverflow_str0
80		.word 52
81	.L._errOverflow_str0:
82		.asciz "fatal error: integer overflow or underflow occurred\n"
83	.align 4
84	_errOverflow:
85		adr x0, .L._errOverflow_str0
86		bl _prints
87		mov w0, #-1
88		bl exit
===========================================================
-- Finished

