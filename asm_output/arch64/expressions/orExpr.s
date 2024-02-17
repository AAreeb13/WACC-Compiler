./valid/expressions/orExpr.wacc
calling the reference compiler on ./valid/expressions/orExpr.wacc
-- Test: orExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating or

# Output:
# true
# true
# false
#

# Program:

begin
  bool a = true ;
  bool b = false ;
  println a || b ;
  println a || true ;
  println b || false
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
orExpr.s contents are:
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
11		mov x8, #1
12		mov x19, x8
13		mov x8, #0
14		mov x20, x8
15		// Stack pointer unchanged, no stack allocated arguments
16		cmp x19, #1
17		b.eq .L0
18		cmp x20, #1
19	.L0:
20		cset x8, eq
21		// push {x8}
22		stp x8, xzr, [sp, #-16]!
23		// pop {x8}
24		ldp x8, xzr, [sp], #16
25		mov x8, x8
26		mov x0, x8
27		// statement primitives do not return results (but will clobber r0/rax)
28		bl _printb
29		bl _println
30		// Stack pointer unchanged, no stack allocated arguments
31		cmp x19, #1
32		b.eq .L1
33		mov x8, #1
34		cmp x8, #1
35	.L1:
36		cset x8, eq
37		// push {x8}
38		stp x8, xzr, [sp, #-16]!
39		// pop {x8}
40		ldp x8, xzr, [sp], #16
41		mov x8, x8
42		mov x0, x8
43		// statement primitives do not return results (but will clobber r0/rax)
44		bl _printb
45		bl _println
46		// Stack pointer unchanged, no stack allocated arguments
47		cmp x20, #1
48		b.eq .L2
49		mov x8, #0
50		cmp x8, #1
51	.L2:
52		cset x8, eq
53		// push {x8}
54		stp x8, xzr, [sp, #-16]!
55		// pop {x8}
56		ldp x8, xzr, [sp], #16
57		mov x8, x8
58		mov x0, x8
59		// statement primitives do not return results (but will clobber r0/rax)
60		bl _printb
61		bl _println
62		// Stack pointer unchanged, no stack allocated variables
63		mov x0, #0
64		// pop {x19, x20}
65		ldp x19, x20, [sp], #16
66		// pop {fp, lr}
67		ldp fp, lr, [sp], #16
68		ret
69	
70	// length of .L._printb_str0
71		.word 5
72	.L._printb_str0:
73		.asciz "false"
74	// length of .L._printb_str1
75		.word 4
76	.L._printb_str1:
77		.asciz "true"
78	// length of .L._printb_str2
79		.word 4
80	.L._printb_str2:
81		.asciz "%.*s"
82	.align 4
83	_printb:
84		// push {lr}
85		stp lr, xzr, [sp, #-16]!
86		cmp w0, #0
87		b.ne .L_printb0
88		adr x2, .L._printb_str0
89		b .L_printb1
90	.L_printb0:
91		adr x2, .L._printb_str1
92	.L_printb1:
93		ldrsw x1, [x2, #-4]
94		adr x0, .L._printb_str2
95		bl printf
96		mov x0, #0
97		bl fflush
98		// pop {lr}
99		ldp lr, xzr, [sp], #16
100		ret
101	
102	// length of .L._println_str0
103		.word 0
104	.L._println_str0:
105		.asciz ""
106	.align 4
107	_println:
108		// push {lr}
109		stp lr, xzr, [sp, #-16]!
110		adr x0, .L._println_str0
111		bl puts
112		mov x0, #0
113		bl fflush
114		// pop {lr}
115		ldp lr, xzr, [sp], #16
116		ret
===========================================================
-- Finished

