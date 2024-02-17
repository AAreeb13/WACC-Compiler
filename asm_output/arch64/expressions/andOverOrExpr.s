./valid/expressions/andOverOrExpr.wacc
calling the reference compiler on ./valid/expressions/andOverOrExpr.wacc
-- Test: andOverOrExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating boolean operator precedence

# Output:
# true
# false
#

# Program:

begin
  bool a = false ;
  bool b = false ;
  bool c = true ;
  println a && b || c ;
  println a && (b || c)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
andOverOrExpr.s contents are:
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
12		mov x8, #0
13		mov x19, x8
14		mov x8, #0
15		mov x20, x8
16		mov x8, #1
17		mov x21, x8
18		// Stack pointer unchanged, no stack allocated arguments
19		cmp x19, #1
20		b.ne .L0
21		cmp x20, #1
22	.L0:
23		cset x8, eq
24		// push {x8}
25		stp x8, xzr, [sp, #-16]!
26		// pop {x8}
27		ldp x8, xzr, [sp], #16
28		cmp x8, #1
29		b.eq .L1
30		cmp x21, #1
31	.L1:
32		cset x8, eq
33		// push {x8}
34		stp x8, xzr, [sp, #-16]!
35		// pop {x8}
36		ldp x8, xzr, [sp], #16
37		mov x8, x8
38		mov x0, x8
39		// statement primitives do not return results (but will clobber r0/rax)
40		bl _printb
41		bl _println
42		// Stack pointer unchanged, no stack allocated arguments
43		cmp x19, #1
44		b.ne .L2
45		cmp x20, #1
46		b.eq .L3
47		cmp x21, #1
48	.L3:
49		cset x8, eq
50		// push {x8}
51		stp x8, xzr, [sp, #-16]!
52		// pop {x8}
53		ldp x8, xzr, [sp], #16
54		cmp x8, #1
55	.L2:
56		cset x8, eq
57		// push {x8}
58		stp x8, xzr, [sp, #-16]!
59		// pop {x8}
60		ldp x8, xzr, [sp], #16
61		mov x8, x8
62		mov x0, x8
63		// statement primitives do not return results (but will clobber r0/rax)
64		bl _printb
65		bl _println
66		// Stack pointer unchanged, no stack allocated variables
67		mov x0, #0
68		// pop {x19, x20, x21}
69		ldp x21, xzr, [sp, #16]
70		ldp x19, x20, [sp], #32
71		// pop {fp, lr}
72		ldp fp, lr, [sp], #16
73		ret
74	
75	// length of .L._printb_str0
76		.word 5
77	.L._printb_str0:
78		.asciz "false"
79	// length of .L._printb_str1
80		.word 4
81	.L._printb_str1:
82		.asciz "true"
83	// length of .L._printb_str2
84		.word 4
85	.L._printb_str2:
86		.asciz "%.*s"
87	.align 4
88	_printb:
89		// push {lr}
90		stp lr, xzr, [sp, #-16]!
91		cmp w0, #0
92		b.ne .L_printb0
93		adr x2, .L._printb_str0
94		b .L_printb1
95	.L_printb0:
96		adr x2, .L._printb_str1
97	.L_printb1:
98		ldrsw x1, [x2, #-4]
99		adr x0, .L._printb_str2
100		bl printf
101		mov x0, #0
102		bl fflush
103		// pop {lr}
104		ldp lr, xzr, [sp], #16
105		ret
106	
107	// length of .L._println_str0
108		.word 0
109	.L._println_str0:
110		.asciz ""
111	.align 4
112	_println:
113		// push {lr}
114		stp lr, xzr, [sp, #-16]!
115		adr x0, .L._println_str0
116		bl puts
117		mov x0, #0
118		bl fflush
119		// pop {lr}
120		ldp lr, xzr, [sp], #16
121		ret
===========================================================
-- Finished

