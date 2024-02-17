./valid/expressions/stringEqualsExpr.wacc
calling the reference compiler on ./valid/expressions/stringEqualsExpr.wacc
-- Test: stringEqualsExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating string equality

# Output:
# true
# false
# false
#

# Program:

begin
  string s1 = "Hello" ;
  string s2 = "foo" ;
  string s3 = "bar" ;
  bool b = s1 == s1 ;
  println b ;
  println s1 == s2 ;
  println s2 == s3
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
stringEqualsExpr.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 5
3	.L.str0:
4		.asciz "Hello"
5	// length of .L.str1
6		.word 3
7	.L.str1:
8		.asciz "foo"
9	// length of .L.str2
10		.word 3
11	.L.str2:
12		.asciz "bar"
13	.align 4
14	.text
15	.global main
16	main:
17		// push {fp, lr}
18		stp fp, lr, [sp, #-16]!
19		// push {x19, x20, x21, x22}
20		stp x19, x20, [sp, #-32]!
21		stp x21, x22, [sp, #16]
22		mov fp, sp
23		// Stack pointer unchanged, no stack allocated variables
24		adrp x8, .L.str0
25		add x8, x8, :lo12:.L.str0
26		// push {x8}
27		stp x8, xzr, [sp, #-16]!
28		// pop {x8}
29		ldp x8, xzr, [sp], #16
30		mov x8, x8
31		mov x19, x8
32		adrp x8, .L.str1
33		add x8, x8, :lo12:.L.str1
34		// push {x8}
35		stp x8, xzr, [sp, #-16]!
36		// pop {x8}
37		ldp x8, xzr, [sp], #16
38		mov x8, x8
39		mov x20, x8
40		adrp x8, .L.str2
41		add x8, x8, :lo12:.L.str2
42		// push {x8}
43		stp x8, xzr, [sp, #-16]!
44		// pop {x8}
45		ldp x8, xzr, [sp], #16
46		mov x8, x8
47		mov x21, x8
48		cmp x19, x19
49		cset x8, eq
50		// push {x8}
51		stp x8, xzr, [sp, #-16]!
52		// pop {x8}
53		ldp x8, xzr, [sp], #16
54		mov x8, x8
55		mov x22, x8
56		// Stack pointer unchanged, no stack allocated arguments
57		mov x8, x22
58		mov x0, x8
59		// statement primitives do not return results (but will clobber r0/rax)
60		bl _printb
61		bl _println
62		// Stack pointer unchanged, no stack allocated arguments
63		cmp x19, x20
64		cset x8, eq
65		// push {x8}
66		stp x8, xzr, [sp, #-16]!
67		// pop {x8}
68		ldp x8, xzr, [sp], #16
69		mov x8, x8
70		mov x0, x8
71		// statement primitives do not return results (but will clobber r0/rax)
72		bl _printb
73		bl _println
74		// Stack pointer unchanged, no stack allocated arguments
75		cmp x20, x21
76		cset x8, eq
77		// push {x8}
78		stp x8, xzr, [sp, #-16]!
79		// pop {x8}
80		ldp x8, xzr, [sp], #16
81		mov x8, x8
82		mov x0, x8
83		// statement primitives do not return results (but will clobber r0/rax)
84		bl _printb
85		bl _println
86		// Stack pointer unchanged, no stack allocated variables
87		mov x0, #0
88		// pop {x19, x20, x21, x22}
89		ldp x21, x22, [sp, #16]
90		ldp x19, x20, [sp], #32
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

