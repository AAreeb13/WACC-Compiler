./valid/expressions/equalsOverAnd.wacc
calling the reference compiler on ./valid/expressions/equalsOverAnd.wacc
-- Test: equalsOverAnd.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating equality and boolean operator precedence

# Output:
# false
# true
#

# Program:

begin
  bool p = false ;
  bool q = true ;
  bool r = false ;
  println p == q && r ;
  println p == (q && r)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
equalsOverAnd.s contents are:
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
14		mov x8, #1
15		mov x20, x8
16		mov x8, #0
17		mov x21, x8
18		// Stack pointer unchanged, no stack allocated arguments
19		cmp x19, x20
20		cset x8, eq
21		// push {x8}
22		stp x8, xzr, [sp, #-16]!
23		// pop {x8}
24		ldp x8, xzr, [sp], #16
25		cmp x8, #1
26		b.ne .L0
27		cmp x21, #1
28	.L0:
29		cset x8, eq
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
40		cmp x20, #1
41		b.ne .L1
42		cmp x21, #1
43	.L1:
44		cset x8, eq
45		// push {x8}
46		stp x8, xzr, [sp, #-16]!
47		// pop {x9}
48		ldp x9, xzr, [sp], #16
49		cmp x19, x9
50		cset x8, eq
51		// push {x8}
52		stp x8, xzr, [sp, #-16]!
53		// pop {x8}
54		ldp x8, xzr, [sp], #16
55		mov x8, x8
56		mov x0, x8
57		// statement primitives do not return results (but will clobber r0/rax)
58		bl _printb
59		bl _println
60		// Stack pointer unchanged, no stack allocated variables
61		mov x0, #0
62		// pop {x19, x20, x21}
63		ldp x21, xzr, [sp, #16]
64		ldp x19, x20, [sp], #32
65		// pop {fp, lr}
66		ldp fp, lr, [sp], #16
67		ret
68	
69	// length of .L._printb_str0
70		.word 5
71	.L._printb_str0:
72		.asciz "false"
73	// length of .L._printb_str1
74		.word 4
75	.L._printb_str1:
76		.asciz "true"
77	// length of .L._printb_str2
78		.word 4
79	.L._printb_str2:
80		.asciz "%.*s"
81	.align 4
82	_printb:
83		// push {lr}
84		stp lr, xzr, [sp, #-16]!
85		cmp w0, #0
86		b.ne .L_printb0
87		adr x2, .L._printb_str0
88		b .L_printb1
89	.L_printb0:
90		adr x2, .L._printb_str1
91	.L_printb1:
92		ldrsw x1, [x2, #-4]
93		adr x0, .L._printb_str2
94		bl printf
95		mov x0, #0
96		bl fflush
97		// pop {lr}
98		ldp lr, xzr, [sp], #16
99		ret
100	
101	// length of .L._println_str0
102		.word 0
103	.L._println_str0:
104		.asciz ""
105	.align 4
106	_println:
107		// push {lr}
108		stp lr, xzr, [sp, #-16]!
109		adr x0, .L._println_str0
110		bl puts
111		mov x0, #0
112		bl fflush
113		// pop {lr}
114		ldp lr, xzr, [sp], #16
115		ret
===========================================================
-- Finished

