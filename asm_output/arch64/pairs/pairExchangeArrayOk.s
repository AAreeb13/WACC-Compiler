./valid/pairs/pairExchangeArrayOk.wacc
calling the reference compiler on ./valid/pairs/pairExchangeArrayOk.wacc
-- Test: pairExchangeArrayOk.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Assignment is legal when assigning array (even of unknown type) in nested pair extraction
# Thanks to Kristina Zimina for catching this

# Output:

# Program:

begin
  pair(int, int) p = newpair(4, 5);
  pair(pair, int) q = newpair(p, 6);
  fst fst q = []
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
pairExchangeArrayOk.s contents are:
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
11		mov w0, #16
12		bl _malloc
13		mov x16, x0
14		mov x8, #4
15		str x8, [x16, #0]
16		mov x8, #5
17		str x8, [x16, #8]
18		mov x8, x16
19		mov x19, x8
20		mov w0, #16
21		bl _malloc
22		mov x16, x0
23		mov x8, x19
24		str x8, [x16, #0]
25		mov x8, #6
26		str x8, [x16, #8]
27		mov x8, x16
28		mov x20, x8
29		// 0 element array
30		mov w0, #4
31		bl _malloc
32		mov x16, x0
33		// array pointers are shifted forwards by 4 bytes (to account for size)
34		add x16, x16, #4
35		mov x8, #0
36		str w8, [x16, #-4]
37		cbz x20, _errNull
38		ldr x8, [x20, #0]
39		mov x8, x8
40		// push {x8}
41		stp x8, xzr, [sp, #-16]!
42		// pop {x9}
43		ldp x9, xzr, [sp], #16
44		cbz x9, _errNull
45		mov x8, x16
46		str x8, [x9, #0]
47		// Stack pointer unchanged, no stack allocated variables
48		mov x0, #0
49		// pop {x19, x20}
50		ldp x19, x20, [sp], #16
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
73	_malloc:
74		// push {lr}
75		stp lr, xzr, [sp, #-16]!
76		bl malloc
77		cbz x0, _errOutOfMemory
78		// pop {lr}
79		ldp lr, xzr, [sp], #16
80		ret
81	
82	// length of .L._errOutOfMemory_str0
83		.word 27
84	.L._errOutOfMemory_str0:
85		.asciz "fatal error: out of memory\n"
86	.align 4
87	_errOutOfMemory:
88		adr x0, .L._errOutOfMemory_str0
89		bl _prints
90		mov w0, #-1
91		bl exit
92	
93	// length of .L._errNull_str0
94		.word 45
95	.L._errNull_str0:
96		.asciz "fatal error: null pair dereferenced or freed\n"
97	.align 4
98	_errNull:
99		adr x0, .L._errNull_str0
100		bl _prints
101		mov w0, #-1
102		bl exit
===========================================================
-- Finished

