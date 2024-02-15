./valid/expressions/longExpr3.wacc
calling the reference compiler on ./valid/expressions/longExpr3.wacc
-- Test: longExpr3.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions

# Output:

# Exit:
# 9

begin

  int x = ((((((((((((((((1 - 2) + 3) - 4) + 5) - 6) + 7) - 8) + 9) - 10) + 11) - 12) + 13) - 14) + 15) - 16) + 17);
  exit x

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longExpr3.s contents are:
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
11		mov w8, #1
12		subs w8, w8, #2
13		b.vs _errOverflow
14		sxtw x8, w8
15		// push {x8}
16		stp x8, xzr, [sp, #-16]!
17		// pop {x8}
18		ldp x8, xzr, [sp], #16
19		adds w8, w8, #3
20		b.vs _errOverflow
21		sxtw x8, w8
22		// push {x8}
23		stp x8, xzr, [sp, #-16]!
24		// pop {x8}
25		ldp x8, xzr, [sp], #16
26		subs w8, w8, #4
27		b.vs _errOverflow
28		sxtw x8, w8
29		// push {x8}
30		stp x8, xzr, [sp, #-16]!
31		// pop {x8}
32		ldp x8, xzr, [sp], #16
33		adds w8, w8, #5
34		b.vs _errOverflow
35		sxtw x8, w8
36		// push {x8}
37		stp x8, xzr, [sp, #-16]!
38		// pop {x8}
39		ldp x8, xzr, [sp], #16
40		subs w8, w8, #6
41		b.vs _errOverflow
42		sxtw x8, w8
43		// push {x8}
44		stp x8, xzr, [sp, #-16]!
45		// pop {x8}
46		ldp x8, xzr, [sp], #16
47		adds w8, w8, #7
48		b.vs _errOverflow
49		sxtw x8, w8
50		// push {x8}
51		stp x8, xzr, [sp, #-16]!
52		// pop {x8}
53		ldp x8, xzr, [sp], #16
54		subs w8, w8, #8
55		b.vs _errOverflow
56		sxtw x8, w8
57		// push {x8}
58		stp x8, xzr, [sp, #-16]!
59		// pop {x8}
60		ldp x8, xzr, [sp], #16
61		adds w8, w8, #9
62		b.vs _errOverflow
63		sxtw x8, w8
64		// push {x8}
65		stp x8, xzr, [sp, #-16]!
66		// pop {x8}
67		ldp x8, xzr, [sp], #16
68		subs w8, w8, #10
69		b.vs _errOverflow
70		sxtw x8, w8
71		// push {x8}
72		stp x8, xzr, [sp, #-16]!
73		// pop {x8}
74		ldp x8, xzr, [sp], #16
75		adds w8, w8, #11
76		b.vs _errOverflow
77		sxtw x8, w8
78		// push {x8}
79		stp x8, xzr, [sp, #-16]!
80		// pop {x8}
81		ldp x8, xzr, [sp], #16
82		subs w8, w8, #12
83		b.vs _errOverflow
84		sxtw x8, w8
85		// push {x8}
86		stp x8, xzr, [sp, #-16]!
87		// pop {x8}
88		ldp x8, xzr, [sp], #16
89		adds w8, w8, #13
90		b.vs _errOverflow
91		sxtw x8, w8
92		// push {x8}
93		stp x8, xzr, [sp, #-16]!
94		// pop {x8}
95		ldp x8, xzr, [sp], #16
96		subs w8, w8, #14
97		b.vs _errOverflow
98		sxtw x8, w8
99		// push {x8}
100		stp x8, xzr, [sp, #-16]!
101		// pop {x8}
102		ldp x8, xzr, [sp], #16
103		adds w8, w8, #15
104		b.vs _errOverflow
105		sxtw x8, w8
106		// push {x8}
107		stp x8, xzr, [sp, #-16]!
108		// pop {x8}
109		ldp x8, xzr, [sp], #16
110		subs w8, w8, #16
111		b.vs _errOverflow
112		sxtw x8, w8
113		// push {x8}
114		stp x8, xzr, [sp, #-16]!
115		// pop {x8}
116		ldp x8, xzr, [sp], #16
117		adds w8, w8, #17
118		b.vs _errOverflow
119		sxtw x8, w8
120		// push {x8}
121		stp x8, xzr, [sp, #-16]!
122		// pop {x8}
123		ldp x8, xzr, [sp], #16
124		mov x8, x8
125		mov x19, x8
126		// Stack pointer unchanged, no stack allocated arguments
127		mov x8, x19
128		mov x0, x8
129		// statement primitives do not return results (but will clobber r0/rax)
130		bl exit
131		// Stack pointer unchanged, no stack allocated variables
132		mov x0, #0
133		// pop {x19}
134		ldp x19, xzr, [sp], #16
135		// pop {fp, lr}
136		ldp fp, lr, [sp], #16
137		ret
138	
139	// length of .L._prints_str0
140		.word 4
141	.L._prints_str0:
142		.asciz "%.*s"
143	.align 4
144	_prints:
145		// push {lr}
146		stp lr, xzr, [sp, #-16]!
147		mov x2, x0
148		ldrsw x1, [x0, #-4]
149		adr x0, .L._prints_str0
150		bl printf
151		mov x0, #0
152		bl fflush
153		// pop {lr}
154		ldp lr, xzr, [sp], #16
155		ret
156	
157	// length of .L._errOverflow_str0
158		.word 52
159	.L._errOverflow_str0:
160		.asciz "fatal error: integer overflow or underflow occurred\n"
161	.align 4
162	_errOverflow:
163		adr x0, .L._errOverflow_str0
164		bl _prints
165		mov w0, #-1
166		bl exit
===========================================================
-- Finished

