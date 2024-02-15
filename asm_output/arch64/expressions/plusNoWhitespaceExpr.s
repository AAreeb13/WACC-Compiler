./valid/expressions/plusNoWhitespaceExpr.wacc
calling the reference compiler on ./valid/expressions/plusNoWhitespaceExpr.wacc
-- Test: plusNoWhitespaceExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# addition expressions should not be whitespace sensitive

# Output:
# 3
#

# Program:

begin
  println 1+2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
plusNoWhitespaceExpr.s contents are:
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
9		mov w8, #1
10		adds w8, w8, #2
11		b.vs _errOverflow
12		sxtw x8, w8
13		// push {x8}
14		stp x8, xzr, [sp, #-16]!
15		// pop {x8}
16		ldp x8, xzr, [sp], #16
17		mov x8, x8
18		mov x0, x8
19		// statement primitives do not return results (but will clobber r0/rax)
20		bl _printi
21		bl _println
22		mov x0, #0
23		// pop {fp, lr}
24		ldp fp, lr, [sp], #16
25		ret
26	
27	// length of .L._prints_str0
28		.word 4
29	.L._prints_str0:
30		.asciz "%.*s"
31	.align 4
32	_prints:
33		// push {lr}
34		stp lr, xzr, [sp, #-16]!
35		mov x2, x0
36		ldrsw x1, [x0, #-4]
37		adr x0, .L._prints_str0
38		bl printf
39		mov x0, #0
40		bl fflush
41		// pop {lr}
42		ldp lr, xzr, [sp], #16
43		ret
44	
45	// length of .L._printi_str0
46		.word 2
47	.L._printi_str0:
48		.asciz "%d"
49	.align 4
50	_printi:
51		// push {lr}
52		stp lr, xzr, [sp, #-16]!
53		mov x1, x0
54		adr x0, .L._printi_str0
55		bl printf
56		mov x0, #0
57		bl fflush
58		// pop {lr}
59		ldp lr, xzr, [sp], #16
60		ret
61	
62	// length of .L._println_str0
63		.word 0
64	.L._println_str0:
65		.asciz ""
66	.align 4
67	_println:
68		// push {lr}
69		stp lr, xzr, [sp, #-16]!
70		adr x0, .L._println_str0
71		bl puts
72		mov x0, #0
73		bl fflush
74		// pop {lr}
75		ldp lr, xzr, [sp], #16
76		ret
77	
78	// length of .L._errOverflow_str0
79		.word 52
80	.L._errOverflow_str0:
81		.asciz "fatal error: integer overflow or underflow occurred\n"
82	.align 4
83	_errOverflow:
84		adr x0, .L._errOverflow_str0
85		bl _prints
86		mov w0, #-1
87		bl exit
===========================================================
-- Finished

