./valid/expressions/longExpr.wacc
calling the reference compiler on ./valid/expressions/longExpr.wacc
-- Test: longExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions

# Output:

# Exit:
# 153

begin

  int x = 1 + (2 + (3 + (4 + (5 + (6 + (7 + (8 + (9 + (10 + (11 + (12 + (13 + (14 + (15 + (16 + 17)))))))))))))));
  exit x

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longExpr.s contents are:
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
11		mov w8, #16
12		adds w8, w8, #17
13		b.vs _errOverflow
14		sxtw x8, w8
15		// push {x8}
16		stp x8, xzr, [sp, #-16]!
17		// pop {x9}
18		ldp x9, xzr, [sp], #16
19		mov w8, #15
20		adds w8, w8, w9
21		b.vs _errOverflow
22		sxtw x8, w8
23		// push {x8}
24		stp x8, xzr, [sp, #-16]!
25		// pop {x9}
26		ldp x9, xzr, [sp], #16
27		mov w8, #14
28		adds w8, w8, w9
29		b.vs _errOverflow
30		sxtw x8, w8
31		// push {x8}
32		stp x8, xzr, [sp, #-16]!
33		// pop {x9}
34		ldp x9, xzr, [sp], #16
35		mov w8, #13
36		adds w8, w8, w9
37		b.vs _errOverflow
38		sxtw x8, w8
39		// push {x8}
40		stp x8, xzr, [sp, #-16]!
41		// pop {x9}
42		ldp x9, xzr, [sp], #16
43		mov w8, #12
44		adds w8, w8, w9
45		b.vs _errOverflow
46		sxtw x8, w8
47		// push {x8}
48		stp x8, xzr, [sp, #-16]!
49		// pop {x9}
50		ldp x9, xzr, [sp], #16
51		mov w8, #11
52		adds w8, w8, w9
53		b.vs _errOverflow
54		sxtw x8, w8
55		// push {x8}
56		stp x8, xzr, [sp, #-16]!
57		// pop {x9}
58		ldp x9, xzr, [sp], #16
59		mov w8, #10
60		adds w8, w8, w9
61		b.vs _errOverflow
62		sxtw x8, w8
63		// push {x8}
64		stp x8, xzr, [sp, #-16]!
65		// pop {x9}
66		ldp x9, xzr, [sp], #16
67		mov w8, #9
68		adds w8, w8, w9
69		b.vs _errOverflow
70		sxtw x8, w8
71		// push {x8}
72		stp x8, xzr, [sp, #-16]!
73		// pop {x9}
74		ldp x9, xzr, [sp], #16
75		mov w8, #8
76		adds w8, w8, w9
77		b.vs _errOverflow
78		sxtw x8, w8
79		// push {x8}
80		stp x8, xzr, [sp, #-16]!
81		// pop {x9}
82		ldp x9, xzr, [sp], #16
83		mov w8, #7
84		adds w8, w8, w9
85		b.vs _errOverflow
86		sxtw x8, w8
87		// push {x8}
88		stp x8, xzr, [sp, #-16]!
89		// pop {x9}
90		ldp x9, xzr, [sp], #16
91		mov w8, #6
92		adds w8, w8, w9
93		b.vs _errOverflow
94		sxtw x8, w8
95		// push {x8}
96		stp x8, xzr, [sp, #-16]!
97		// pop {x9}
98		ldp x9, xzr, [sp], #16
99		mov w8, #5
100		adds w8, w8, w9
101		b.vs _errOverflow
102		sxtw x8, w8
103		// push {x8}
104		stp x8, xzr, [sp, #-16]!
105		// pop {x9}
106		ldp x9, xzr, [sp], #16
107		mov w8, #4
108		adds w8, w8, w9
109		b.vs _errOverflow
110		sxtw x8, w8
111		// push {x8}
112		stp x8, xzr, [sp, #-16]!
113		// pop {x9}
114		ldp x9, xzr, [sp], #16
115		mov w8, #3
116		adds w8, w8, w9
117		b.vs _errOverflow
118		sxtw x8, w8
119		// push {x8}
120		stp x8, xzr, [sp, #-16]!
121		// pop {x9}
122		ldp x9, xzr, [sp], #16
123		mov w8, #2
124		adds w8, w8, w9
125		b.vs _errOverflow
126		sxtw x8, w8
127		// push {x8}
128		stp x8, xzr, [sp, #-16]!
129		// pop {x9}
130		ldp x9, xzr, [sp], #16
131		mov w8, #1
132		adds w8, w8, w9
133		b.vs _errOverflow
134		sxtw x8, w8
135		// push {x8}
136		stp x8, xzr, [sp, #-16]!
137		// pop {x8}
138		ldp x8, xzr, [sp], #16
139		mov x8, x8
140		mov x19, x8
141		// Stack pointer unchanged, no stack allocated arguments
142		mov x8, x19
143		mov x0, x8
144		// statement primitives do not return results (but will clobber r0/rax)
145		bl exit
146		// Stack pointer unchanged, no stack allocated variables
147		mov x0, #0
148		// pop {x19}
149		ldp x19, xzr, [sp], #16
150		// pop {fp, lr}
151		ldp fp, lr, [sp], #16
152		ret
153	
154	// length of .L._prints_str0
155		.word 4
156	.L._prints_str0:
157		.asciz "%.*s"
158	.align 4
159	_prints:
160		// push {lr}
161		stp lr, xzr, [sp, #-16]!
162		mov x2, x0
163		ldrsw x1, [x0, #-4]
164		adr x0, .L._prints_str0
165		bl printf
166		mov x0, #0
167		bl fflush
168		// pop {lr}
169		ldp lr, xzr, [sp], #16
170		ret
171	
172	// length of .L._errOverflow_str0
173		.word 52
174	.L._errOverflow_str0:
175		.asciz "fatal error: integer overflow or underflow occurred\n"
176	.align 4
177	_errOverflow:
178		adr x0, .L._errOverflow_str0
179		bl _prints
180		mov w0, #-1
181		bl exit
===========================================================
-- Finished

