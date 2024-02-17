./valid/expressions/intCalc.wacc
calling the reference compiler on ./valid/expressions/intCalc.wacc
-- Test: intCalc.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple integer calculation

# Output:
# 72
#

# Program:

begin
  int x = 42 ;
  int y = 30 ;
  int z = x + y ;
  println z
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intCalc.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19, x20, x21}
8		stp x19, x20, [sp, #-32]!
9		stp x21, xzr, [sp, #16]
10		mov fp, sp
11		// Stack pointer unchanged, no stack allocated variables
12		mov x8, #42
13		mov x19, x8
14		mov x8, #30
15		mov x20, x8
16		adds w8, w19, w20
17		b.vs _errOverflow
18		sxtw x8, w8
19		// push {x8}
20		stp x8, xzr, [sp, #-16]!
21		// pop {x8}
22		ldp x8, xzr, [sp], #16
23		mov x8, x8
24		mov x21, x8
25		// Stack pointer unchanged, no stack allocated arguments
26		mov x8, x21
27		mov x0, x8
28		// statement primitives do not return results (but will clobber r0/rax)
29		bl _printi
30		bl _println
31		// Stack pointer unchanged, no stack allocated variables
32		mov x0, #0
33		// pop {x19, x20, x21}
34		ldp x21, xzr, [sp, #16]
35		ldp x19, x20, [sp], #32
36		// pop {fp, lr}
37		ldp fp, lr, [sp], #16
38		ret
39	
40	// length of .L._prints_str0
41		.word 4
42	.L._prints_str0:
43		.asciz "%.*s"
44	.align 4
45	_prints:
46		// push {lr}
47		stp lr, xzr, [sp, #-16]!
48		mov x2, x0
49		ldrsw x1, [x0, #-4]
50		adr x0, .L._prints_str0
51		bl printf
52		mov x0, #0
53		bl fflush
54		// pop {lr}
55		ldp lr, xzr, [sp], #16
56		ret
57	
58	// length of .L._printi_str0
59		.word 2
60	.L._printi_str0:
61		.asciz "%d"
62	.align 4
63	_printi:
64		// push {lr}
65		stp lr, xzr, [sp, #-16]!
66		mov x1, x0
67		adr x0, .L._printi_str0
68		bl printf
69		mov x0, #0
70		bl fflush
71		// pop {lr}
72		ldp lr, xzr, [sp], #16
73		ret
74	
75	// length of .L._println_str0
76		.word 0
77	.L._println_str0:
78		.asciz ""
79	.align 4
80	_println:
81		// push {lr}
82		stp lr, xzr, [sp, #-16]!
83		adr x0, .L._println_str0
84		bl puts
85		mov x0, #0
86		bl fflush
87		// pop {lr}
88		ldp lr, xzr, [sp], #16
89		ret
90	
91	// length of .L._errOverflow_str0
92		.word 52
93	.L._errOverflow_str0:
94		.asciz "fatal error: integer overflow or underflow occurred\n"
95	.align 4
96	_errOverflow:
97		adr x0, .L._errOverflow_str0
98		bl _prints
99		mov w0, #-1
100		bl exit
===========================================================
-- Finished

