./valid/expressions/multNoWhitespaceExpr.wacc
calling the reference compiler on ./valid/expressions/multNoWhitespaceExpr.wacc
-- Test: multNoWhitespaceExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# multiplication expressions should not be whitespace sensitive

# Output:
# 2
#

# Program:

begin
  println 1*2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
multNoWhitespaceExpr.s contents are:
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
9		mov x9, #2
10		mov x8, #1
11		smull x8, w8, w9
12		// take the 31st bit, sign extend it to 64 bits
13		sbfx x9, x8, #31, #1
14		// now take the top 32 bits of the result, shift and sign extend to 64 bits
15		cmp x9, x8, asr #32
16		// if they are not equal then overflow occured
17		b.ne _errOverflow
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
28		mov x0, #0
29		// pop {fp, lr}
30		ldp fp, lr, [sp], #16
31		ret
32	
33	// length of .L._prints_str0
34		.word 4
35	.L._prints_str0:
36		.asciz "%.*s"
37	.align 4
38	_prints:
39		// push {lr}
40		stp lr, xzr, [sp, #-16]!
41		mov x2, x0
42		ldrsw x1, [x0, #-4]
43		adr x0, .L._prints_str0
44		bl printf
45		mov x0, #0
46		bl fflush
47		// pop {lr}
48		ldp lr, xzr, [sp], #16
49		ret
50	
51	// length of .L._printi_str0
52		.word 2
53	.L._printi_str0:
54		.asciz "%d"
55	.align 4
56	_printi:
57		// push {lr}
58		stp lr, xzr, [sp, #-16]!
59		mov x1, x0
60		adr x0, .L._printi_str0
61		bl printf
62		mov x0, #0
63		bl fflush
64		// pop {lr}
65		ldp lr, xzr, [sp], #16
66		ret
67	
68	// length of .L._println_str0
69		.word 0
70	.L._println_str0:
71		.asciz ""
72	.align 4
73	_println:
74		// push {lr}
75		stp lr, xzr, [sp, #-16]!
76		adr x0, .L._println_str0
77		bl puts
78		mov x0, #0
79		bl fflush
80		// pop {lr}
81		ldp lr, xzr, [sp], #16
82		ret
83	
84	// length of .L._errOverflow_str0
85		.word 52
86	.L._errOverflow_str0:
87		.asciz "fatal error: integer overflow or underflow occurred\n"
88	.align 4
89	_errOverflow:
90		adr x0, .L._errOverflow_str0
91		bl _prints
92		mov w0, #-1
93		bl exit
===========================================================
-- Finished

