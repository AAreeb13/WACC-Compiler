./valid/expressions/equalsExpr.wacc
calling the reference compiler on ./valid/expressions/equalsExpr.wacc
-- Test: equalsExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating equality

# Output:
# false
# false
# true
#

# Program:

begin
  int x = 2 ;
  int y = 4 ;
  int z = 4 ;
  bool b = x == y ;
  println b ;
  println x == y ;
  println y == z
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
equalsExpr.s contents are:
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
12		mov x8, #2
13		mov x19, x8
14		mov x8, #4
15		mov x20, x8
16		mov x8, #4
17		mov x21, x8
18		cmp x19, x20
19		cset x8, eq
20		// push {x8}
21		stp x8, xzr, [sp, #-16]!
22		// pop {x8}
23		ldp x8, xzr, [sp], #16
24		mov x8, x8
25		mov x22, x8
26		// Stack pointer unchanged, no stack allocated arguments
27		mov x8, x22
28		mov x0, x8
29		// statement primitives do not return results (but will clobber r0/rax)
30		bl _printb
31		bl _println
32		// Stack pointer unchanged, no stack allocated arguments
33		cmp x19, x20
34		cset x8, eq
35		// push {x8}
36		stp x8, xzr, [sp, #-16]!
37		// pop {x8}
38		ldp x8, xzr, [sp], #16
39		mov x8, x8
40		mov x0, x8
41		// statement primitives do not return results (but will clobber r0/rax)
42		bl _printb
43		bl _println
44		// Stack pointer unchanged, no stack allocated arguments
45		cmp x20, x21
46		cset x8, eq
47		// push {x8}
48		stp x8, xzr, [sp, #-16]!
49		// pop {x8}
50		ldp x8, xzr, [sp], #16
51		mov x8, x8
52		mov x0, x8
53		// statement primitives do not return results (but will clobber r0/rax)
54		bl _printb
55		bl _println
56		// Stack pointer unchanged, no stack allocated variables
57		mov x0, #0
58		// pop {x19, x20, x21, x22}
59		ldp x21, x22, [sp, #16]
60		ldp x19, x20, [sp], #32
61		// pop {fp, lr}
62		ldp fp, lr, [sp], #16
63		ret
64	
65	// length of .L._printb_str0
66		.word 5
67	.L._printb_str0:
68		.asciz "false"
69	// length of .L._printb_str1
70		.word 4
71	.L._printb_str1:
72		.asciz "true"
73	// length of .L._printb_str2
74		.word 4
75	.L._printb_str2:
76		.asciz "%.*s"
77	.align 4
78	_printb:
79		// push {lr}
80		stp lr, xzr, [sp, #-16]!
81		cmp w0, #0
82		b.ne .L_printb0
83		adr x2, .L._printb_str0
84		b .L_printb1
85	.L_printb0:
86		adr x2, .L._printb_str1
87	.L_printb1:
88		ldrsw x1, [x2, #-4]
89		adr x0, .L._printb_str2
90		bl printf
91		mov x0, #0
92		bl fflush
93		// pop {lr}
94		ldp lr, xzr, [sp], #16
95		ret
96	
97	// length of .L._println_str0
98		.word 0
99	.L._println_str0:
100		.asciz ""
101	.align 4
102	_println:
103		// push {lr}
104		stp lr, xzr, [sp, #-16]!
105		adr x0, .L._println_str0
106		bl puts
107		mov x0, #0
108		bl fflush
109		// pop {lr}
110		ldp lr, xzr, [sp], #16
111		ret
===========================================================
-- Finished

