./valid/expressions/notExpr.wacc
calling the reference compiler on ./valid/expressions/notExpr.wacc
-- Test: notExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating not

# Output:
# false
# true
#

# Program:

begin
  bool a = true ;
  bool b = false ;
  println !a ;
  println !b
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
notExpr.s contents are:
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
17		cset x8, ne
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
28		cmp x20, #1
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
39		// Stack pointer unchanged, no stack allocated variables
40		mov x0, #0
41		// pop {x19, x20}
42		ldp x19, x20, [sp], #16
43		// pop {fp, lr}
44		ldp fp, lr, [sp], #16
45		ret
46	
47	// length of .L._printb_str0
48		.word 5
49	.L._printb_str0:
50		.asciz "false"
51	// length of .L._printb_str1
52		.word 4
53	.L._printb_str1:
54		.asciz "true"
55	// length of .L._printb_str2
56		.word 4
57	.L._printb_str2:
58		.asciz "%.*s"
59	.align 4
60	_printb:
61		// push {lr}
62		stp lr, xzr, [sp, #-16]!
63		cmp w0, #0
64		b.ne .L_printb0
65		adr x2, .L._printb_str0
66		b .L_printb1
67	.L_printb0:
68		adr x2, .L._printb_str1
69	.L_printb1:
70		ldrsw x1, [x2, #-4]
71		adr x0, .L._printb_str2
72		bl printf
73		mov x0, #0
74		bl fflush
75		// pop {lr}
76		ldp lr, xzr, [sp], #16
77		ret
78	
79	// length of .L._println_str0
80		.word 0
81	.L._println_str0:
82		.asciz ""
83	.align 4
84	_println:
85		// push {lr}
86		stp lr, xzr, [sp, #-16]!
87		adr x0, .L._println_str0
88		bl puts
89		mov x0, #0
90		bl fflush
91		// pop {lr}
92		ldp lr, xzr, [sp], #16
93		ret
===========================================================
-- Finished

