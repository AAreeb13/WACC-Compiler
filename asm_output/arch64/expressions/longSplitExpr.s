./valid/expressions/longSplitExpr.wacc
calling the reference compiler on ./valid/expressions/longSplitExpr.wacc
-- Test: longSplitExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions with several variables

# Output:

# Exit:
# 153

begin

  int a = 1 + 2 ;
  int b = 3 + 4 ;
  int c = 5 + 6 ;
  int d = 7 + 8 ;
  int e = 9 + 10 ;
  int f = 11 + 12 ;
  int g = 13 + 14 ;
  int h = 15 + 16 ;
  int i = 17 ;
  exit a + b + c + d + e + f + g + h + i

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longSplitExpr.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19, x20, x21, x22, x23, x24, x25, x26, x27}
8		stp x19, x20, [sp, #-80]!
9		stp x21, x22, [sp, #16]
10		stp x23, x24, [sp, #32]
11		stp x25, x26, [sp, #48]
12		stp x27, xzr, [sp, #64]
13		mov fp, sp
14		// Stack pointer unchanged, no stack allocated variables
15		mov w8, #1
16		adds w8, w8, #2
17		b.vs _errOverflow
18		sxtw x8, w8
19		// push {x8}
20		stp x8, xzr, [sp, #-16]!
21		// pop {x8}
22		ldp x8, xzr, [sp], #16
23		mov x8, x8
24		mov x19, x8
25		mov w8, #3
26		adds w8, w8, #4
27		b.vs _errOverflow
28		sxtw x8, w8
29		// push {x8}
30		stp x8, xzr, [sp, #-16]!
31		// pop {x8}
32		ldp x8, xzr, [sp], #16
33		mov x8, x8
34		mov x20, x8
35		mov w8, #5
36		adds w8, w8, #6
37		b.vs _errOverflow
38		sxtw x8, w8
39		// push {x8}
40		stp x8, xzr, [sp, #-16]!
41		// pop {x8}
42		ldp x8, xzr, [sp], #16
43		mov x8, x8
44		mov x21, x8
45		mov w8, #7
46		adds w8, w8, #8
47		b.vs _errOverflow
48		sxtw x8, w8
49		// push {x8}
50		stp x8, xzr, [sp, #-16]!
51		// pop {x8}
52		ldp x8, xzr, [sp], #16
53		mov x8, x8
54		mov x22, x8
55		mov w8, #9
56		adds w8, w8, #10
57		b.vs _errOverflow
58		sxtw x8, w8
59		// push {x8}
60		stp x8, xzr, [sp, #-16]!
61		// pop {x8}
62		ldp x8, xzr, [sp], #16
63		mov x8, x8
64		mov x23, x8
65		mov w8, #11
66		adds w8, w8, #12
67		b.vs _errOverflow
68		sxtw x8, w8
69		// push {x8}
70		stp x8, xzr, [sp, #-16]!
71		// pop {x8}
72		ldp x8, xzr, [sp], #16
73		mov x8, x8
74		mov x24, x8
75		mov w8, #13
76		adds w8, w8, #14
77		b.vs _errOverflow
78		sxtw x8, w8
79		// push {x8}
80		stp x8, xzr, [sp, #-16]!
81		// pop {x8}
82		ldp x8, xzr, [sp], #16
83		mov x8, x8
84		mov x25, x8
85		mov w8, #15
86		adds w8, w8, #16
87		b.vs _errOverflow
88		sxtw x8, w8
89		// push {x8}
90		stp x8, xzr, [sp, #-16]!
91		// pop {x8}
92		ldp x8, xzr, [sp], #16
93		mov x8, x8
94		mov x26, x8
95		mov x8, #17
96		mov x27, x8
97		// Stack pointer unchanged, no stack allocated arguments
98		adds w8, w19, w20
99		b.vs _errOverflow
100		sxtw x8, w8
101		// push {x8}
102		stp x8, xzr, [sp, #-16]!
103		// pop {x8}
104		ldp x8, xzr, [sp], #16
105		adds w8, w8, w21
106		b.vs _errOverflow
107		sxtw x8, w8
108		// push {x8}
109		stp x8, xzr, [sp, #-16]!
110		// pop {x8}
111		ldp x8, xzr, [sp], #16
112		adds w8, w8, w22
113		b.vs _errOverflow
114		sxtw x8, w8
115		// push {x8}
116		stp x8, xzr, [sp, #-16]!
117		// pop {x8}
118		ldp x8, xzr, [sp], #16
119		adds w8, w8, w23
120		b.vs _errOverflow
121		sxtw x8, w8
122		// push {x8}
123		stp x8, xzr, [sp, #-16]!
124		// pop {x8}
125		ldp x8, xzr, [sp], #16
126		adds w8, w8, w24
127		b.vs _errOverflow
128		sxtw x8, w8
129		// push {x8}
130		stp x8, xzr, [sp, #-16]!
131		// pop {x8}
132		ldp x8, xzr, [sp], #16
133		adds w8, w8, w25
134		b.vs _errOverflow
135		sxtw x8, w8
136		// push {x8}
137		stp x8, xzr, [sp, #-16]!
138		// pop {x8}
139		ldp x8, xzr, [sp], #16
140		adds w8, w8, w26
141		b.vs _errOverflow
142		sxtw x8, w8
143		// push {x8}
144		stp x8, xzr, [sp, #-16]!
145		// pop {x8}
146		ldp x8, xzr, [sp], #16
147		adds w8, w8, w27
148		b.vs _errOverflow
149		sxtw x8, w8
150		// push {x8}
151		stp x8, xzr, [sp, #-16]!
152		// pop {x8}
153		ldp x8, xzr, [sp], #16
154		mov x8, x8
155		mov x0, x8
156		// statement primitives do not return results (but will clobber r0/rax)
157		bl exit
158		// Stack pointer unchanged, no stack allocated variables
159		mov x0, #0
160		// pop {x19, x20, x21, x22, x23, x24, x25, x26, x27}
161		ldp x21, x22, [sp, #16]
162		ldp x23, x24, [sp, #32]
163		ldp x25, x26, [sp, #48]
164		ldp x27, xzr, [sp, #64]
165		ldp x19, x20, [sp], #80
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
188	// length of .L._errOverflow_str0
189		.word 52
190	.L._errOverflow_str0:
191		.asciz "fatal error: integer overflow or underflow occurred\n"
192	.align 4
193	_errOverflow:
194		adr x0, .L._errOverflow_str0
195		bl _prints
196		mov w0, #-1
197		bl exit
===========================================================
-- Finished

