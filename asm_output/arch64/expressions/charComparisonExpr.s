./valid/expressions/charComparisonExpr.wacc
calling the reference compiler on ./valid/expressions/charComparisonExpr.wacc
-- Test: charComparisonExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# detailed battery of character comparison tests

# Output:
# false
# true
# true
# true
# false
# false
#

# Program:

begin
  char c1 = 'a' ;
  char c2 = 'z' ;
  println c1 == c2 ;
  println c1 != c2 ;
  println c1 < c2 ;
  println c1 <= c2 ;
  println c1 > c2 ;
  println c1 >= c2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
charComparisonExpr.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19, x20}
8		stp x19, x20, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		mov x8, #97
12		mov x19, x8
13		mov x8, #122
14		mov x20, x8
15		// Stack pointer unchanged, no stack allocated arguments
16		cmp x19, x20
17		cset x8, eq
18		// push {x8}
19		stp x8, xzr, [sp, #-16]!
20		// pop {x8}
21		ldp x8, xzr, [sp], #16
22		mov x8, x8
23		mov x0, x8
24		// statement primitives do not return results (but will clobber r0/rax)
25		bl _printb
26		bl _println
27		// Stack pointer unchanged, no stack allocated arguments
28		cmp x19, x20
29		cset x8, ne
30		// push {x8}
31		stp x8, xzr, [sp, #-16]!
32		// pop {x8}
33		ldp x8, xzr, [sp], #16
34		mov x8, x8
35		mov x0, x8
36		// statement primitives do not return results (but will clobber r0/rax)
37		bl _printb
38		bl _println
39		// Stack pointer unchanged, no stack allocated arguments
40		cmp x19, x20
41		cset x8, lt
42		// push {x8}
43		stp x8, xzr, [sp, #-16]!
44		// pop {x8}
45		ldp x8, xzr, [sp], #16
46		mov x8, x8
47		mov x0, x8
48		// statement primitives do not return results (but will clobber r0/rax)
49		bl _printb
50		bl _println
51		// Stack pointer unchanged, no stack allocated arguments
52		cmp x19, x20
53		cset x8, le
54		// push {x8}
55		stp x8, xzr, [sp, #-16]!
56		// pop {x8}
57		ldp x8, xzr, [sp], #16
58		mov x8, x8
59		mov x0, x8
60		// statement primitives do not return results (but will clobber r0/rax)
61		bl _printb
62		bl _println
63		// Stack pointer unchanged, no stack allocated arguments
64		cmp x19, x20
65		cset x8, gt
66		// push {x8}
67		stp x8, xzr, [sp, #-16]!
68		// pop {x8}
69		ldp x8, xzr, [sp], #16
70		mov x8, x8
71		mov x0, x8
72		// statement primitives do not return results (but will clobber r0/rax)
73		bl _printb
74		bl _println
75		// Stack pointer unchanged, no stack allocated arguments
76		cmp x19, x20
77		cset x8, ge
78		// push {x8}
79		stp x8, xzr, [sp, #-16]!
80		// pop {x8}
81		ldp x8, xzr, [sp], #16
82		mov x8, x8
83		mov x0, x8
84		// statement primitives do not return results (but will clobber r0/rax)
85		bl _printb
86		bl _println
87		// Stack pointer unchanged, no stack allocated variables
88		mov x0, #0
89		// pop {x19, x20}
90		ldp x19, x20, [sp], #16
91		// pop {fp, lr}
92		ldp fp, lr, [sp], #16
93		ret
94	
95	// length of .L._printb_str0
96		.word 5
97	.L._printb_str0:
98		.asciz "false"
99	// length of .L._printb_str1
100		.word 4
101	.L._printb_str1:
102		.asciz "true"
103	// length of .L._printb_str2
104		.word 4
105	.L._printb_str2:
106		.asciz "%.*s"
107	.align 4
108	_printb:
109		// push {lr}
110		stp lr, xzr, [sp, #-16]!
111		cmp w0, #0
112		b.ne .L_printb0
113		adr x2, .L._printb_str0
114		b .L_printb1
115	.L_printb0:
116		adr x2, .L._printb_str1
117	.L_printb1:
118		ldrsw x1, [x2, #-4]
119		adr x0, .L._printb_str2
120		bl printf
121		mov x0, #0
122		bl fflush
123		// pop {lr}
124		ldp lr, xzr, [sp], #16
125		ret
126	
127	// length of .L._println_str0
128		.word 0
129	.L._println_str0:
130		.asciz ""
131	.align 4
132	_println:
133		// push {lr}
134		stp lr, xzr, [sp, #-16]!
135		adr x0, .L._println_str0
136		bl puts
137		mov x0, #0
138		bl fflush
139		// pop {lr}
140		ldp lr, xzr, [sp], #16
141		ret
===========================================================
-- Finished

