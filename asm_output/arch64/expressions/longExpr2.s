./valid/expressions/longExpr2.wacc
calling the reference compiler on ./valid/expressions/longExpr2.wacc
-- Test: longExpr2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions

# Output:

# Exit:
# 10

begin

  int x = (2 + 3 + 2 + 1 + 1 + 1) - (1 + 2) * (3 - 4 / 6) / ( 2 * (18 - 17) + (3 * 4 / 4 + 6));
  exit x

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longExpr2.s contents are:
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
11		mov w8, #2
12		adds w8, w8, #3
13		b.vs _errOverflow
14		sxtw x8, w8
15		// push {x8}
16		stp x8, xzr, [sp, #-16]!
17		// pop {x8}
18		ldp x8, xzr, [sp], #16
19		adds w8, w8, #2
20		b.vs _errOverflow
21		sxtw x8, w8
22		// push {x8}
23		stp x8, xzr, [sp, #-16]!
24		// pop {x8}
25		ldp x8, xzr, [sp], #16
26		adds w8, w8, #1
27		b.vs _errOverflow
28		sxtw x8, w8
29		// push {x8}
30		stp x8, xzr, [sp, #-16]!
31		// pop {x8}
32		ldp x8, xzr, [sp], #16
33		adds w8, w8, #1
34		b.vs _errOverflow
35		sxtw x8, w8
36		// push {x8}
37		stp x8, xzr, [sp, #-16]!
38		// pop {x8}
39		ldp x8, xzr, [sp], #16
40		adds w8, w8, #1
41		b.vs _errOverflow
42		sxtw x8, w8
43		// push {x8}
44		stp x8, xzr, [sp, #-16]!
45		mov w8, #1
46		adds w8, w8, #2
47		b.vs _errOverflow
48		sxtw x8, w8
49		// push {x8}
50		stp x8, xzr, [sp, #-16]!
51		mov w9, #6
52		mov w8, #4
53		cbz x9, _errDivZero
54		sdiv w8, w8, w9
55		sxtw x8, w8
56		// push {x8}
57		stp x8, xzr, [sp, #-16]!
58		// pop {x9}
59		ldp x9, xzr, [sp], #16
60		mov w8, #3
61		subs w8, w8, w9
62		b.vs _errOverflow
63		sxtw x8, w8
64		// push {x8}
65		stp x8, xzr, [sp, #-16]!
66		// pop {x9}
67		ldp x9, xzr, [sp], #16
68		// pop {x8}
69		ldp x8, xzr, [sp], #16
70		smull x8, w8, w9
71		// take the 31st bit, sign extend it to 64 bits
72		sbfx x9, x8, #31, #1
73		// now take the top 32 bits of the result, shift and sign extend to 64 bits
74		cmp x9, x8, asr #32
75		// if they are not equal then overflow occured
76		b.ne _errOverflow
77		sxtw x8, w8
78		// push {x8}
79		stp x8, xzr, [sp, #-16]!
80		mov w8, #18
81		subs w8, w8, #17
82		b.vs _errOverflow
83		sxtw x8, w8
84		// push {x8}
85		stp x8, xzr, [sp, #-16]!
86		// pop {x9}
87		ldp x9, xzr, [sp], #16
88		mov x8, #2
89		smull x8, w8, w9
90		// take the 31st bit, sign extend it to 64 bits
91		sbfx x9, x8, #31, #1
92		// now take the top 32 bits of the result, shift and sign extend to 64 bits
93		cmp x9, x8, asr #32
94		// if they are not equal then overflow occured
95		b.ne _errOverflow
96		sxtw x8, w8
97		// push {x8}
98		stp x8, xzr, [sp, #-16]!
99		mov x9, #4
100		mov x8, #3
101		smull x8, w8, w9
102		// take the 31st bit, sign extend it to 64 bits
103		sbfx x9, x8, #31, #1
104		// now take the top 32 bits of the result, shift and sign extend to 64 bits
105		cmp x9, x8, asr #32
106		// if they are not equal then overflow occured
107		b.ne _errOverflow
108		sxtw x8, w8
109		// push {x8}
110		stp x8, xzr, [sp, #-16]!
111		mov w9, #4
112		// pop {x8}
113		ldp x8, xzr, [sp], #16
114		cbz x9, _errDivZero
115		sdiv w8, w8, w9
116		sxtw x8, w8
117		// push {x8}
118		stp x8, xzr, [sp, #-16]!
119		// pop {x8}
120		ldp x8, xzr, [sp], #16
121		adds w8, w8, #6
122		b.vs _errOverflow
123		sxtw x8, w8
124		// push {x8}
125		stp x8, xzr, [sp, #-16]!
126		// pop {x9}
127		ldp x9, xzr, [sp], #16
128		// pop {x8}
129		ldp x8, xzr, [sp], #16
130		adds w8, w8, w9
131		b.vs _errOverflow
132		sxtw x8, w8
133		// push {x8}
134		stp x8, xzr, [sp, #-16]!
135		// pop {x9}
136		ldp x9, xzr, [sp], #16
137		// pop {x8}
138		ldp x8, xzr, [sp], #16
139		cbz x9, _errDivZero
140		sdiv w8, w8, w9
141		sxtw x8, w8
142		// push {x8}
143		stp x8, xzr, [sp, #-16]!
144		// pop {x9}
145		ldp x9, xzr, [sp], #16
146		// pop {x8}
147		ldp x8, xzr, [sp], #16
148		subs w8, w8, w9
149		b.vs _errOverflow
150		sxtw x8, w8
151		// push {x8}
152		stp x8, xzr, [sp, #-16]!
153		// pop {x8}
154		ldp x8, xzr, [sp], #16
155		mov x8, x8
156		mov x19, x8
157		// Stack pointer unchanged, no stack allocated arguments
158		mov x8, x19
159		mov x0, x8
160		// statement primitives do not return results (but will clobber r0/rax)
161		bl exit
162		// Stack pointer unchanged, no stack allocated variables
163		mov x0, #0
164		// pop {x19}
165		ldp x19, xzr, [sp], #16
166		// pop {fp, lr}
167		ldp fp, lr, [sp], #16
168		ret
169	
170	// length of .L._prints_str0
171		.word 4
172	.L._prints_str0:
173		.asciz "%.*s"
174	.align 4
175	_prints:
176		// push {lr}
177		stp lr, xzr, [sp, #-16]!
178		mov x2, x0
179		ldrsw x1, [x0, #-4]
180		adr x0, .L._prints_str0
181		bl printf
182		mov x0, #0
183		bl fflush
184		// pop {lr}
185		ldp lr, xzr, [sp], #16
186		ret
187	
188	// length of .L._errDivZero_str0
189		.word 40
190	.L._errDivZero_str0:
191		.asciz "fatal error: division or modulo by zero\n"
192	.align 4
193	_errDivZero:
194		adr x0, .L._errDivZero_str0
195		bl _prints
196		mov w0, #-1
197		bl exit
198	
199	// length of .L._errOverflow_str0
200		.word 52
201	.L._errOverflow_str0:
202		.asciz "fatal error: integer overflow or underflow occurred\n"
203	.align 4
204	_errOverflow:
205		adr x0, .L._errOverflow_str0
206		bl _prints
207		mov w0, #-1
208		bl exit
===========================================================
-- Finished

