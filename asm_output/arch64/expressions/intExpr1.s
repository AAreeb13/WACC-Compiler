./valid/expressions/intExpr1.wacc
calling the reference compiler on ./valid/expressions/intExpr1.wacc
-- Test: intExpr1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating a moderately complex integer expression

# Output:
# Correct
#

# Program:

begin
  int a = ( 10 * 1 + 2 * 15 );
  if a == 40 then
    println "Correct"
  else
    println "Wrong"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intExpr1.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 5
3	.L.str0:
4		.asciz "Wrong"
5	// length of .L.str1
6		.word 7
7	.L.str1:
8		.asciz "Correct"
9	.align 4
10	.text
11	.global main
12	main:
13		// push {fp, lr}
14		stp fp, lr, [sp, #-16]!
15		// push {x19}
16		stp x19, xzr, [sp, #-16]!
17		mov fp, sp
18		// Stack pointer unchanged, no stack allocated variables
19		mov x9, #1
20		mov x8, #10
21		smull x8, w8, w9
22		// take the 31st bit, sign extend it to 64 bits
23		sbfx x9, x8, #31, #1
24		// now take the top 32 bits of the result, shift and sign extend to 64 bits
25		cmp x9, x8, asr #32
26		// if they are not equal then overflow occured
27		b.ne _errOverflow
28		sxtw x8, w8
29		// push {x8}
30		stp x8, xzr, [sp, #-16]!
31		mov x9, #15
32		mov x8, #2
33		smull x8, w8, w9
34		// take the 31st bit, sign extend it to 64 bits
35		sbfx x9, x8, #31, #1
36		// now take the top 32 bits of the result, shift and sign extend to 64 bits
37		cmp x9, x8, asr #32
38		// if they are not equal then overflow occured
39		b.ne _errOverflow
40		sxtw x8, w8
41		// push {x8}
42		stp x8, xzr, [sp, #-16]!
43		// pop {x9}
44		ldp x9, xzr, [sp], #16
45		// pop {x8}
46		ldp x8, xzr, [sp], #16
47		adds w8, w8, w9
48		b.vs _errOverflow
49		sxtw x8, w8
50		// push {x8}
51		stp x8, xzr, [sp, #-16]!
52		// pop {x8}
53		ldp x8, xzr, [sp], #16
54		mov x8, x8
55		mov x19, x8
56		cmp x19, #40
57		b.eq .L0
58		// Stack pointer unchanged, no stack allocated arguments
59		adrp x8, .L.str0
60		add x8, x8, :lo12:.L.str0
61		// push {x8}
62		stp x8, xzr, [sp, #-16]!
63		// pop {x8}
64		ldp x8, xzr, [sp], #16
65		mov x8, x8
66		mov x0, x8
67		// statement primitives do not return results (but will clobber r0/rax)
68		bl _prints
69		bl _println
70		b .L1
71	.L0:
72		// Stack pointer unchanged, no stack allocated arguments
73		adrp x8, .L.str1
74		add x8, x8, :lo12:.L.str1
75		// push {x8}
76		stp x8, xzr, [sp, #-16]!
77		// pop {x8}
78		ldp x8, xzr, [sp], #16
79		mov x8, x8
80		mov x0, x8
81		// statement primitives do not return results (but will clobber r0/rax)
82		bl _prints
83		bl _println
84	.L1:
85		// Stack pointer unchanged, no stack allocated variables
86		mov x0, #0
87		// pop {x19}
88		ldp x19, xzr, [sp], #16
89		// pop {fp, lr}
90		ldp fp, lr, [sp], #16
91		ret
92	
93	// length of .L._prints_str0
94		.word 4
95	.L._prints_str0:
96		.asciz "%.*s"
97	.align 4
98	_prints:
99		// push {lr}
100		stp lr, xzr, [sp, #-16]!
101		mov x2, x0
102		ldrsw x1, [x0, #-4]
103		adr x0, .L._prints_str0
104		bl printf
105		mov x0, #0
106		bl fflush
107		// pop {lr}
108		ldp lr, xzr, [sp], #16
109		ret
110	
111	// length of .L._println_str0
112		.word 0
113	.L._println_str0:
114		.asciz ""
115	.align 4
116	_println:
117		// push {lr}
118		stp lr, xzr, [sp, #-16]!
119		adr x0, .L._println_str0
120		bl puts
121		mov x0, #0
122		bl fflush
123		// pop {lr}
124		ldp lr, xzr, [sp], #16
125		ret
126	
127	// length of .L._errOverflow_str0
128		.word 52
129	.L._errOverflow_str0:
130		.asciz "fatal error: integer overflow or underflow occurred\n"
131	.align 4
132	_errOverflow:
133		adr x0, .L._errOverflow_str0
134		bl _prints
135		mov w0, #-1
136		bl exit
===========================================================
-- Finished

