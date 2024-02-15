./valid/runtimeErr/integerOverflow/intWayOverflow.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intWayOverflow.wacc
-- Test: intWayOverflow.wacc

-- Uploaded file: 
---------------------------------------------------------------
# positive overflow

# Output:
# 2000000000
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = 2000000000 ;
  println x ;
  x = x + 2000000000; #err here?
  println x 
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intWayOverflow.s contents are:
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
11		mov x8, #0x9400
12		movk x8, #0x7735, lsl #16
13		mov x8, x8
14		mov x19, x8
15		// Stack pointer unchanged, no stack allocated arguments
16		mov x8, x19
17		mov x0, x8
18		// statement primitives do not return results (but will clobber r0/rax)
19		bl _printi
20		bl _println
21		mov x9, #0x9400
22		movk x9, #0x7735, lsl #16
23		adds w8, w19, w9
24		b.vs _errOverflow
25		sxtw x8, w8
26		// push {x8}
27		stp x8, xzr, [sp, #-16]!
28		// pop {x8}
29		ldp x8, xzr, [sp], #16
30		mov x8, x8
31		mov x19, x8
32		// Stack pointer unchanged, no stack allocated arguments
33		mov x8, x19
34		mov x0, x8
35		// statement primitives do not return results (but will clobber r0/rax)
36		bl _printi
37		bl _println
38		// Stack pointer unchanged, no stack allocated variables
39		mov x0, #0
40		// pop {x19}
41		ldp x19, xzr, [sp], #16
42		// pop {fp, lr}
43		ldp fp, lr, [sp], #16
44		ret
45	
46	// length of .L._prints_str0
47		.word 4
48	.L._prints_str0:
49		.asciz "%.*s"
50	.align 4
51	_prints:
52		// push {lr}
53		stp lr, xzr, [sp, #-16]!
54		mov x2, x0
55		ldrsw x1, [x0, #-4]
56		adr x0, .L._prints_str0
57		bl printf
58		mov x0, #0
59		bl fflush
60		// pop {lr}
61		ldp lr, xzr, [sp], #16
62		ret
63	
64	// length of .L._printi_str0
65		.word 2
66	.L._printi_str0:
67		.asciz "%d"
68	.align 4
69	_printi:
70		// push {lr}
71		stp lr, xzr, [sp, #-16]!
72		mov x1, x0
73		adr x0, .L._printi_str0
74		bl printf
75		mov x0, #0
76		bl fflush
77		// pop {lr}
78		ldp lr, xzr, [sp], #16
79		ret
80	
81	// length of .L._println_str0
82		.word 0
83	.L._println_str0:
84		.asciz ""
85	.align 4
86	_println:
87		// push {lr}
88		stp lr, xzr, [sp, #-16]!
89		adr x0, .L._println_str0
90		bl puts
91		mov x0, #0
92		bl fflush
93		// pop {lr}
94		ldp lr, xzr, [sp], #16
95		ret
96	
97	// length of .L._errOverflow_str0
98		.word 52
99	.L._errOverflow_str0:
100		.asciz "fatal error: integer overflow or underflow occurred\n"
101	.align 4
102	_errOverflow:
103		adr x0, .L._errOverflow_str0
104		bl _prints
105		mov w0, #-1
106		bl exit
===========================================================
-- Finished

