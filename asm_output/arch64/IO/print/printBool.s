./valid/IO/print/printBool.wacc
calling the reference compiler on ./valid/IO/print/printBool.wacc
-- Test: printBool.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic Boolean printing

# Output:
# True is true
# False is false
#

# Program:

begin
  print "True is " ;
  println true ;

  print "False is " ;
  println false
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printBool.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 8
3	.L.str0:
4		.asciz "True is "
5	// length of .L.str1
6		.word 9
7	.L.str1:
8		.asciz "False is "
9	.align 4
10	.text
11	.global main
12	main:
13		// push {fp, lr}
14		stp fp, lr, [sp, #-16]!
15		mov fp, sp
16		// Stack pointer unchanged, no stack allocated arguments
17		adrp x8, .L.str0
18		add x8, x8, :lo12:.L.str0
19		// push {x8}
20		stp x8, xzr, [sp, #-16]!
21		// pop {x8}
22		ldp x8, xzr, [sp], #16
23		mov x8, x8
24		mov x0, x8
25		// statement primitives do not return results (but will clobber r0/rax)
26		bl _prints
27		// Stack pointer unchanged, no stack allocated arguments
28		mov x8, #1
29		mov x0, x8
30		// statement primitives do not return results (but will clobber r0/rax)
31		bl _printb
32		bl _println
33		// Stack pointer unchanged, no stack allocated arguments
34		adrp x8, .L.str1
35		add x8, x8, :lo12:.L.str1
36		// push {x8}
37		stp x8, xzr, [sp, #-16]!
38		// pop {x8}
39		ldp x8, xzr, [sp], #16
40		mov x8, x8
41		mov x0, x8
42		// statement primitives do not return results (but will clobber r0/rax)
43		bl _prints
44		// Stack pointer unchanged, no stack allocated arguments
45		mov x8, #0
46		mov x0, x8
47		// statement primitives do not return results (but will clobber r0/rax)
48		bl _printb
49		bl _println
50		mov x0, #0
51		// pop {fp, lr}
52		ldp fp, lr, [sp], #16
53		ret
54	
55	// length of .L._prints_str0
56		.word 4
57	.L._prints_str0:
58		.asciz "%.*s"
59	.align 4
60	_prints:
61		// push {lr}
62		stp lr, xzr, [sp, #-16]!
63		mov x2, x0
64		ldrsw x1, [x0, #-4]
65		adr x0, .L._prints_str0
66		bl printf
67		mov x0, #0
68		bl fflush
69		// pop {lr}
70		ldp lr, xzr, [sp], #16
71		ret
72	
73	// length of .L._printb_str0
74		.word 5
75	.L._printb_str0:
76		.asciz "false"
77	// length of .L._printb_str1
78		.word 4
79	.L._printb_str1:
80		.asciz "true"
81	// length of .L._printb_str2
82		.word 4
83	.L._printb_str2:
84		.asciz "%.*s"
85	.align 4
86	_printb:
87		// push {lr}
88		stp lr, xzr, [sp, #-16]!
89		cmp w0, #0
90		b.ne .L_printb0
91		adr x2, .L._printb_str0
92		b .L_printb1
93	.L_printb0:
94		adr x2, .L._printb_str1
95	.L_printb1:
96		ldrsw x1, [x2, #-4]
97		adr x0, .L._printb_str2
98		bl printf
99		mov x0, #0
100		bl fflush
101		// pop {lr}
102		ldp lr, xzr, [sp], #16
103		ret
104	
105	// length of .L._println_str0
106		.word 0
107	.L._println_str0:
108		.asciz ""
109	.align 4
110	_println:
111		// push {lr}
112		stp lr, xzr, [sp, #-16]!
113		adr x0, .L._println_str0
114		bl puts
115		mov x0, #0
116		bl fflush
117		// pop {lr}
118		ldp lr, xzr, [sp], #16
119		ret
===========================================================
-- Finished

