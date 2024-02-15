./valid/expressions/lessCharExpr.wacc
calling the reference compiler on ./valid/expressions/lessCharExpr.wacc
-- Test: lessCharExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating less-than on characters

# Output:
# true
# false
#

# Program:

begin
  char a = 'a' ;
  char e = 'e' ;
  char c = 'c' ;
  println a < e ;
  println e < c
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
lessCharExpr.s contents are:
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
12		mov x8, #97
13		mov x19, x8
14		mov x8, #101
15		mov x20, x8
16		mov x8, #99
17		mov x21, x8
18		// Stack pointer unchanged, no stack allocated arguments
19		cmp x19, x20
20		cset x8, lt
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
31		cmp x20, x21
32		cset x8, lt
33		// push {x8}
34		stp x8, xzr, [sp, #-16]!
35		// pop {x8}
36		ldp x8, xzr, [sp], #16
37		mov x8, x8
38		mov x0, x8
39		// statement primitives do not return results (but will clobber r0/rax)
40		bl _printb
41		bl _println
42		// Stack pointer unchanged, no stack allocated variables
43		mov x0, #0
44		// pop {x19, x20, x21}
45		ldp x21, xzr, [sp, #16]
46		ldp x19, x20, [sp], #32
47		// pop {fp, lr}
48		ldp fp, lr, [sp], #16
49		ret
50	
51	// length of .L._printb_str0
52		.word 5
53	.L._printb_str0:
54		.asciz "false"
55	// length of .L._printb_str1
56		.word 4
57	.L._printb_str1:
58		.asciz "true"
59	// length of .L._printb_str2
60		.word 4
61	.L._printb_str2:
62		.asciz "%.*s"
63	.align 4
64	_printb:
65		// push {lr}
66		stp lr, xzr, [sp, #-16]!
67		cmp w0, #0
68		b.ne .L_printb0
69		adr x2, .L._printb_str0
70		b .L_printb1
71	.L_printb0:
72		adr x2, .L._printb_str1
73	.L_printb1:
74		ldrsw x1, [x2, #-4]
75		adr x0, .L._printb_str2
76		bl printf
77		mov x0, #0
78		bl fflush
79		// pop {lr}
80		ldp lr, xzr, [sp], #16
81		ret
82	
83	// length of .L._println_str0
84		.word 0
85	.L._println_str0:
86		.asciz ""
87	.align 4
88	_println:
89		// push {lr}
90		stp lr, xzr, [sp, #-16]!
91		adr x0, .L._println_str0
92		bl puts
93		mov x0, #0
94		bl fflush
95		// pop {lr}
96		ldp lr, xzr, [sp], #16
97		ret
===========================================================
-- Finished

