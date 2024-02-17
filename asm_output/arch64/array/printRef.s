./valid/array/printRef.wacc
calling the reference compiler on ./valid/array/printRef.wacc
-- Test: printRef.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array (reference) printing

# Output:
# Printing an array variable gives an address, such as #addrs#
#

# Program:

begin
  print "Printing an array variable gives an address, such as " ;
  int[] a = [1,2,3] ;
  println a
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printRef.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 53
3	.L.str0:
4		.asciz "Printing an array variable gives an address, such as "
5	.align 4
6	.text
7	.global main
8	main:
9		// push {fp, lr}
10		stp fp, lr, [sp, #-16]!
11		// push {x19}
12		stp x19, xzr, [sp, #-16]!
13		mov fp, sp
14		// Stack pointer unchanged, no stack allocated variables
15		// Stack pointer unchanged, no stack allocated arguments
16		adrp x8, .L.str0
17		add x8, x8, :lo12:.L.str0
18		// push {x8}
19		stp x8, xzr, [sp, #-16]!
20		// pop {x8}
21		ldp x8, xzr, [sp], #16
22		mov x8, x8
23		mov x0, x8
24		// statement primitives do not return results (but will clobber r0/rax)
25		bl _prints
26		// 3 element array
27		mov w0, #16
28		bl _malloc
29		mov x16, x0
30		// array pointers are shifted forwards by 4 bytes (to account for size)
31		add x16, x16, #4
32		mov x8, #3
33		str w8, [x16, #-4]
34		mov x8, #1
35		str w8, [x16, #0]
36		mov x8, #2
37		str w8, [x16, #4]
38		mov x8, #3
39		str w8, [x16, #8]
40		mov x8, x16
41		mov x19, x8
42		// Stack pointer unchanged, no stack allocated arguments
43		mov x8, x19
44		mov x0, x8
45		// statement primitives do not return results (but will clobber r0/rax)
46		bl _printp
47		bl _println
48		// Stack pointer unchanged, no stack allocated variables
49		mov x0, #0
50		// pop {x19}
51		ldp x19, xzr, [sp], #16
52		// pop {fp, lr}
53		ldp fp, lr, [sp], #16
54		ret
55	
56	// length of .L._prints_str0
57		.word 4
58	.L._prints_str0:
59		.asciz "%.*s"
60	.align 4
61	_prints:
62		// push {lr}
63		stp lr, xzr, [sp, #-16]!
64		mov x2, x0
65		ldrsw x1, [x0, #-4]
66		adr x0, .L._prints_str0
67		bl printf
68		mov x0, #0
69		bl fflush
70		// pop {lr}
71		ldp lr, xzr, [sp], #16
72		ret
73	
74	// length of .L._printp_str0
75		.word 2
76	.L._printp_str0:
77		.asciz "%p"
78	.align 4
79	_printp:
80		// push {lr}
81		stp lr, xzr, [sp, #-16]!
82		mov x1, x0
83		adr x0, .L._printp_str0
84		bl printf
85		mov x0, #0
86		bl fflush
87		// pop {lr}
88		ldp lr, xzr, [sp], #16
89		ret
90	
91	_malloc:
92		// push {lr}
93		stp lr, xzr, [sp, #-16]!
94		bl malloc
95		cbz x0, _errOutOfMemory
96		// pop {lr}
97		ldp lr, xzr, [sp], #16
98		ret
99	
100	// length of .L._println_str0
101		.word 0
102	.L._println_str0:
103		.asciz ""
104	.align 4
105	_println:
106		// push {lr}
107		stp lr, xzr, [sp, #-16]!
108		adr x0, .L._println_str0
109		bl puts
110		mov x0, #0
111		bl fflush
112		// pop {lr}
113		ldp lr, xzr, [sp], #16
114		ret
115	
116	// length of .L._errOutOfMemory_str0
117		.word 27
118	.L._errOutOfMemory_str0:
119		.asciz "fatal error: out of memory\n"
120	.align 4
121	_errOutOfMemory:
122		adr x0, .L._errOutOfMemory_str0
123		bl _prints
124		mov w0, #-1
125		bl exit
===========================================================
-- Finished

