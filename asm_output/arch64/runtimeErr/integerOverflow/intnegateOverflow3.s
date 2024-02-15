./valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc
-- Test: intnegateOverflow3.wacc

-- Uploaded file: 
---------------------------------------------------------------
# multiplying a negative integer

# Output:
# -20000
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = -20000 ;
  println x ;
  x = x*100000000; #err here? 
  println x 
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intnegateOverflow3.s contents are:
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
11		mov x8, #-20000
12		mov x19, x8
13		// Stack pointer unchanged, no stack allocated arguments
14		mov x8, x19
15		mov x0, x8
16		// statement primitives do not return results (but will clobber r0/rax)
17		bl _printi
18		bl _println
19		mov x9, #0xe100
20		movk x9, #0x5f5, lsl #16
21		smull x8, w19, w9
22		// take the 31st bit, sign extend it to 64 bits
23		sbfx x9, x8, #31, #1
24		// now take the top 32 bits of the result, shift and sign extend to 64 bits
25		cmp x9, x8, asr #32
26		// if they are not equal then overflow occured
27		b.ne _errOverflow
28		sxtw x8, w8
29		// push {x8}
30		stp x8, xzr, [sp, #-16]!
31		// pop {x8}
32		ldp x8, xzr, [sp], #16
33		mov x8, x8
34		mov x19, x8
35		// Stack pointer unchanged, no stack allocated arguments
36		mov x8, x19
37		mov x0, x8
38		// statement primitives do not return results (but will clobber r0/rax)
39		bl _printi
40		bl _println
41		// Stack pointer unchanged, no stack allocated variables
42		mov x0, #0
43		// pop {x19}
44		ldp x19, xzr, [sp], #16
45		// pop {fp, lr}
46		ldp fp, lr, [sp], #16
47		ret
48	
49	// length of .L._prints_str0
50		.word 4
51	.L._prints_str0:
52		.asciz "%.*s"
53	.align 4
54	_prints:
55		// push {lr}
56		stp lr, xzr, [sp, #-16]!
57		mov x2, x0
58		ldrsw x1, [x0, #-4]
59		adr x0, .L._prints_str0
60		bl printf
61		mov x0, #0
62		bl fflush
63		// pop {lr}
64		ldp lr, xzr, [sp], #16
65		ret
66	
67	// length of .L._printi_str0
68		.word 2
69	.L._printi_str0:
70		.asciz "%d"
71	.align 4
72	_printi:
73		// push {lr}
74		stp lr, xzr, [sp, #-16]!
75		mov x1, x0
76		adr x0, .L._printi_str0
77		bl printf
78		mov x0, #0
79		bl fflush
80		// pop {lr}
81		ldp lr, xzr, [sp], #16
82		ret
83	
84	// length of .L._println_str0
85		.word 0
86	.L._println_str0:
87		.asciz ""
88	.align 4
89	_println:
90		// push {lr}
91		stp lr, xzr, [sp, #-16]!
92		adr x0, .L._println_str0
93		bl puts
94		mov x0, #0
95		bl fflush
96		// pop {lr}
97		ldp lr, xzr, [sp], #16
98		ret
99	
100	// length of .L._errOverflow_str0
101		.word 52
102	.L._errOverflow_str0:
103		.asciz "fatal error: integer overflow or underflow occurred\n"
104	.align 4
105	_errOverflow:
106		adr x0, .L._errOverflow_str0
107		bl _prints
108		mov w0, #-1
109		bl exit
===========================================================
-- Finished

