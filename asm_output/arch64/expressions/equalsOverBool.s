./valid/expressions/equalsOverBool.wacc
calling the reference compiler on ./valid/expressions/equalsOverBool.wacc
-- Test: equalsOverBool.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating equality and boolean operator precedence

# Output:
# true
# false
#

# Program:

begin
  bool p = true ;
  bool q = true ;
  bool r = false ;
  bool s = true ;
  println p && q != r || s ;
  println (p && q) != (r || s)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
equalsOverBool.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19, x20, x21, x22}
8		stp x19, x20, [sp, #-32]!
9		stp x21, x22, [sp, #16]
10		mov fp, sp
11		// Stack pointer unchanged, no stack allocated variables
12		mov x8, #1
13		mov x19, x8
14		mov x8, #1
15		mov x20, x8
16		mov x8, #0
17		mov x21, x8
18		mov x8, #1
19		mov x22, x8
20		// Stack pointer unchanged, no stack allocated arguments
21		cmp x19, #1
22		b.ne .L0
23		cmp x20, x21
24		cset x8, ne
25		// push {x8}
26		stp x8, xzr, [sp, #-16]!
27		// pop {x8}
28		ldp x8, xzr, [sp], #16
29		cmp x8, #1
30	.L0:
31		cset x8, eq
32		// push {x8}
33		stp x8, xzr, [sp, #-16]!
34		// pop {x8}
35		ldp x8, xzr, [sp], #16
36		cmp x8, #1
37		b.eq .L1
38		cmp x22, #1
39	.L1:
40		cset x8, eq
41		// push {x8}
42		stp x8, xzr, [sp, #-16]!
43		// pop {x8}
44		ldp x8, xzr, [sp], #16
45		mov x8, x8
46		mov x0, x8
47		// statement primitives do not return results (but will clobber r0/rax)
48		bl _printb
49		bl _println
50		// Stack pointer unchanged, no stack allocated arguments
51		cmp x19, #1
52		b.ne .L2
53		cmp x20, #1
54	.L2:
55		cset x8, eq
56		// push {x8}
57		stp x8, xzr, [sp, #-16]!
58		cmp x21, #1
59		b.eq .L3
60		cmp x22, #1
61	.L3:
62		cset x8, eq
63		// push {x8}
64		stp x8, xzr, [sp, #-16]!
65		// pop {x9}
66		ldp x9, xzr, [sp], #16
67		// pop {x8}
68		ldp x8, xzr, [sp], #16
69		cmp x8, x9
70		cset x8, ne
71		// push {x8}
72		stp x8, xzr, [sp, #-16]!
73		// pop {x8}
74		ldp x8, xzr, [sp], #16
75		mov x8, x8
76		mov x0, x8
77		// statement primitives do not return results (but will clobber r0/rax)
78		bl _printb
79		bl _println
80		// Stack pointer unchanged, no stack allocated variables
81		mov x0, #0
82		// pop {x19, x20, x21, x22}
83		ldp x21, x22, [sp, #16]
84		ldp x19, x20, [sp], #32
85		// pop {fp, lr}
86		ldp fp, lr, [sp], #16
87		ret
88	
89	// length of .L._printb_str0
90		.word 5
91	.L._printb_str0:
92		.asciz "false"
93	// length of .L._printb_str1
94		.word 4
95	.L._printb_str1:
96		.asciz "true"
97	// length of .L._printb_str2
98		.word 4
99	.L._printb_str2:
100		.asciz "%.*s"
101	.align 4
102	_printb:
103		// push {lr}
104		stp lr, xzr, [sp, #-16]!
105		cmp w0, #0
106		b.ne .L_printb0
107		adr x2, .L._printb_str0
108		b .L_printb1
109	.L_printb0:
110		adr x2, .L._printb_str1
111	.L_printb1:
112		ldrsw x1, [x2, #-4]
113		adr x0, .L._printb_str2
114		bl printf
115		mov x0, #0
116		bl fflush
117		// pop {lr}
118		ldp lr, xzr, [sp], #16
119		ret
120	
121	// length of .L._println_str0
122		.word 0
123	.L._println_str0:
124		.asciz ""
125	.align 4
126	_println:
127		// push {lr}
128		stp lr, xzr, [sp, #-16]!
129		adr x0, .L._println_str0
130		bl puts
131		mov x0, #0
132		bl fflush
133		// pop {lr}
134		ldp lr, xzr, [sp], #16
135		ret
===========================================================
-- Finished

