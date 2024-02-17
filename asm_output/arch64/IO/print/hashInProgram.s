./valid/IO/print/hashInProgram.wacc
calling the reference compiler on ./valid/IO/print/hashInProgram.wacc
-- Test: hashInProgram.wacc

-- Uploaded file: 
---------------------------------------------------------------
# In-line comments and printing #

# Output:
# We can print the hash character: #
# We can also print # when its in a string.
#

# Program:

begin
  int x = 0 ; # comments can be in-line
  print "We can print the hash character: " ;
  println '#' ;
  println "We can also print # when its in a string."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
hashInProgram.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 33
3	.L.str0:
4		.asciz "We can print the hash character: "
5	// length of .L.str1
6		.word 41
7	.L.str1:
8		.asciz "We can also print # when its in a string."
9	.align 4
10	.text
11	.global main
12	main:
13		// push {fp, lr}
14		stp fp, lr, [sp, #-16]!
15		// push {x19}
16		stp x19, xzr, [sp, #-16]!
17		mov fp, sp
18		// Stack pointer unchanged, no stack allocated variables
19		mov x8, #0
20		mov x19, x8
21		// Stack pointer unchanged, no stack allocated arguments
22		adrp x8, .L.str0
23		add x8, x8, :lo12:.L.str0
24		// push {x8}
25		stp x8, xzr, [sp, #-16]!
26		// pop {x8}
27		ldp x8, xzr, [sp], #16
28		mov x8, x8
29		mov x0, x8
30		// statement primitives do not return results (but will clobber r0/rax)
31		bl _prints
32		// Stack pointer unchanged, no stack allocated arguments
33		mov x8, #35
34		mov x0, x8
35		// statement primitives do not return results (but will clobber r0/rax)
36		bl _printc
37		bl _println
38		// Stack pointer unchanged, no stack allocated arguments
39		adrp x8, .L.str1
40		add x8, x8, :lo12:.L.str1
41		// push {x8}
42		stp x8, xzr, [sp, #-16]!
43		// pop {x8}
44		ldp x8, xzr, [sp], #16
45		mov x8, x8
46		mov x0, x8
47		// statement primitives do not return results (but will clobber r0/rax)
48		bl _prints
49		bl _println
50		// Stack pointer unchanged, no stack allocated variables
51		mov x0, #0
52		// pop {x19}
53		ldp x19, xzr, [sp], #16
54		// pop {fp, lr}
55		ldp fp, lr, [sp], #16
56		ret
57	
58	// length of .L._prints_str0
59		.word 4
60	.L._prints_str0:
61		.asciz "%.*s"
62	.align 4
63	_prints:
64		// push {lr}
65		stp lr, xzr, [sp, #-16]!
66		mov x2, x0
67		ldrsw x1, [x0, #-4]
68		adr x0, .L._prints_str0
69		bl printf
70		mov x0, #0
71		bl fflush
72		// pop {lr}
73		ldp lr, xzr, [sp], #16
74		ret
75	
76	// length of .L._printc_str0
77		.word 2
78	.L._printc_str0:
79		.asciz "%c"
80	.align 4
81	_printc:
82		// push {lr}
83		stp lr, xzr, [sp, #-16]!
84		mov x1, x0
85		adr x0, .L._printc_str0
86		bl printf
87		mov x0, #0
88		bl fflush
89		// pop {lr}
90		ldp lr, xzr, [sp], #16
91		ret
92	
93	// length of .L._println_str0
94		.word 0
95	.L._println_str0:
96		.asciz ""
97	.align 4
98	_println:
99		// push {lr}
100		stp lr, xzr, [sp, #-16]!
101		adr x0, .L._println_str0
102		bl puts
103		mov x0, #0
104		bl fflush
105		// pop {lr}
106		ldp lr, xzr, [sp], #16
107		ret
===========================================================
-- Finished

