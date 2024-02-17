./valid/expressions/boolCalc.wacc
calling the reference compiler on ./valid/expressions/boolCalc.wacc
-- Test: boolCalc.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple boolean calculation

# Output:
# false
#

# Program:

begin
  bool b1 = true ;
  bool b2 = false ;
  bool b3 = b1 && b2 ;
  println b3
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
boolCalc.s contents are:
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
12		mov x8, #1
13		mov x19, x8
14		mov x8, #0
15		mov x20, x8
16		cmp x19, #1
17		b.ne .L0
18		cmp x20, #1
19	.L0:
20		cset x8, eq
21		// push {x8}
22		stp x8, xzr, [sp, #-16]!
23		// pop {x8}
24		ldp x8, xzr, [sp], #16
25		mov x8, x8
26		mov x21, x8
27		// Stack pointer unchanged, no stack allocated arguments
28		mov x8, x21
29		mov x0, x8
30		// statement primitives do not return results (but will clobber r0/rax)
31		bl _printb
32		bl _println
33		// Stack pointer unchanged, no stack allocated variables
34		mov x0, #0
35		// pop {x19, x20, x21}
36		ldp x21, xzr, [sp, #16]
37		ldp x19, x20, [sp], #32
38		// pop {fp, lr}
39		ldp fp, lr, [sp], #16
40		ret
41	
42	// length of .L._printb_str0
43		.word 5
44	.L._printb_str0:
45		.asciz "false"
46	// length of .L._printb_str1
47		.word 4
48	.L._printb_str1:
49		.asciz "true"
50	// length of .L._printb_str2
51		.word 4
52	.L._printb_str2:
53		.asciz "%.*s"
54	.align 4
55	_printb:
56		// push {lr}
57		stp lr, xzr, [sp, #-16]!
58		cmp w0, #0
59		b.ne .L_printb0
60		adr x2, .L._printb_str0
61		b .L_printb1
62	.L_printb0:
63		adr x2, .L._printb_str1
64	.L_printb1:
65		ldrsw x1, [x2, #-4]
66		adr x0, .L._printb_str2
67		bl printf
68		mov x0, #0
69		bl fflush
70		// pop {lr}
71		ldp lr, xzr, [sp], #16
72		ret
73	
74	// length of .L._println_str0
75		.word 0
76	.L._println_str0:
77		.asciz ""
78	.align 4
79	_println:
80		// push {lr}
81		stp lr, xzr, [sp, #-16]!
82		adr x0, .L._println_str0
83		bl puts
84		mov x0, #0
85		bl fflush
86		// pop {lr}
87		ldp lr, xzr, [sp], #16
88		ret
===========================================================
-- Finished

