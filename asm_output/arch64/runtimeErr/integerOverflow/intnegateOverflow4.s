./valid/runtimeErr/integerOverflow/intnegateOverflow4.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intnegateOverflow4.wacc
-- Test: intnegateOverflow4.wacc

-- Uploaded file: 
---------------------------------------------------------------
# negative overflow

# Output:
# -2000000000
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = -2000000000 ;
  println x ;
  x = x - 2000000000; #err here? 
  println x 
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intnegateOverflow4.s contents are:
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
11		mov x8, #0x6c00
12		movk x8, #0x88ca, lsl #16
13		movk x8, #0xffff, lsl #32
14		movk x8, #0xffff, lsl #48
15		mov x8, x8
16		mov x19, x8
17		// Stack pointer unchanged, no stack allocated arguments
18		mov x8, x19
19		mov x0, x8
20		// statement primitives do not return results (but will clobber r0/rax)
21		bl _printi
22		bl _println
23		mov x9, #0x9400
24		movk x9, #0x7735, lsl #16
25		subs w8, w19, w9
26		b.vs _errOverflow
27		sxtw x8, w8
28		// push {x8}
29		stp x8, xzr, [sp, #-16]!
30		// pop {x8}
31		ldp x8, xzr, [sp], #16
32		mov x8, x8
33		mov x19, x8
34		// Stack pointer unchanged, no stack allocated arguments
35		mov x8, x19
36		mov x0, x8
37		// statement primitives do not return results (but will clobber r0/rax)
38		bl _printi
39		bl _println
40		// Stack pointer unchanged, no stack allocated variables
41		mov x0, #0
42		// pop {x19}
43		ldp x19, xzr, [sp], #16
44		// pop {fp, lr}
45		ldp fp, lr, [sp], #16
46		ret
47	
48	// length of .L._prints_str0
49		.word 4
50	.L._prints_str0:
51		.asciz "%.*s"
52	.align 4
53	_prints:
54		// push {lr}
55		stp lr, xzr, [sp, #-16]!
56		mov x2, x0
57		ldrsw x1, [x0, #-4]
58		adr x0, .L._prints_str0
59		bl printf
60		mov x0, #0
61		bl fflush
62		// pop {lr}
63		ldp lr, xzr, [sp], #16
64		ret
65	
66	// length of .L._printi_str0
67		.word 2
68	.L._printi_str0:
69		.asciz "%d"
70	.align 4
71	_printi:
72		// push {lr}
73		stp lr, xzr, [sp, #-16]!
74		mov x1, x0
75		adr x0, .L._printi_str0
76		bl printf
77		mov x0, #0
78		bl fflush
79		// pop {lr}
80		ldp lr, xzr, [sp], #16
81		ret
82	
83	// length of .L._println_str0
84		.word 0
85	.L._println_str0:
86		.asciz ""
87	.align 4
88	_println:
89		// push {lr}
90		stp lr, xzr, [sp, #-16]!
91		adr x0, .L._println_str0
92		bl puts
93		mov x0, #0
94		bl fflush
95		// pop {lr}
96		ldp lr, xzr, [sp], #16
97		ret
98	
99	// length of .L._errOverflow_str0
100		.word 52
101	.L._errOverflow_str0:
102		.asciz "fatal error: integer overflow or underflow occurred\n"
103	.align 4
104	_errOverflow:
105		adr x0, .L._errOverflow_str0
106		bl _prints
107		mov w0, #-1
108		bl exit
===========================================================
-- Finished

