./valid/array/lenArrayIndex.wacc
calling the reference compiler on ./valid/array/lenArrayIndex.wacc
-- Test: lenArrayIndex.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Tests that array length works on array indexes
# Thanks to Nixon Enraght-Moony for his help in this test-case

# Output:
# 0

# Program:

begin
  int[] arr = [];
  int[][] arrs = [arr];
  print len arrs[0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
lenArrayIndex.s contents are:
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
11		// 0 element array
12		mov w0, #4
13		bl _malloc
14		mov x16, x0
15		// array pointers are shifted forwards by 4 bytes (to account for size)
16		add x16, x16, #4
17		mov x8, #0
18		str w8, [x16, #-4]
19		mov x8, x16
20		mov x19, x8
21		// 1 element array
22		mov w0, #12
23		bl _malloc
24		mov x16, x0
25		// array pointers are shifted forwards by 4 bytes (to account for size)
26		add x16, x16, #4
27		mov x8, #1
28		str w8, [x16, #-4]
29		mov x8, x19
30		str x8, [x16, #0]
31		mov x8, x16
32		mov x20, x8
33		// Stack pointer unchanged, no stack allocated arguments
34		mov w17, #0
35		mov x7, x20
36		bl _arrLoad8
37		mov x8, x7
38		mov x8, x8
39		// push {x8}
40		stp x8, xzr, [sp, #-16]!
41		// pop {x8}
42		ldp x8, xzr, [sp], #16
43		ldrsw x8, [x8, #-4]
44		mov x8, x8
45		mov x0, x8
46		// statement primitives do not return results (but will clobber r0/rax)
47		bl _printi
48		// Stack pointer unchanged, no stack allocated variables
49		mov x0, #0
50		// pop {x19, x20}
51		ldp x19, x20, [sp], #16
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
74	_malloc:
75		// push {lr}
76		stp lr, xzr, [sp, #-16]!
77		bl malloc
78		cbz x0, _errOutOfMemory
79		// pop {lr}
80		ldp lr, xzr, [sp], #16
81		ret
82	
83	// length of .L._printi_str0
84		.word 2
85	.L._printi_str0:
86		.asciz "%d"
87	.align 4
88	_printi:
89		// push {lr}
90		stp lr, xzr, [sp, #-16]!
91		mov x1, x0
92		adr x0, .L._printi_str0
93		bl printf
94		mov x0, #0
95		bl fflush
96		// pop {lr}
97		ldp lr, xzr, [sp], #16
98		ret
99	
100	_arrLoad8:
101		// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7
102		// push {lr}
103		stp lr, xzr, [sp, #-16]!
104		sxtw x17, w17
105		cmp w17, #0
106		csel x1, x17, x1, lt
107		b.lt _errOutOfBounds
108		ldrsw lr, [x7, #-4]
109		cmp w17, w30
110		csel x1, x17, x1, ge
111		b.ge _errOutOfBounds
112		ldr x7, [x7, x17, lsl #3]
113		// pop {lr}
114		ldp lr, xzr, [sp], #16
115		ret
116	
117	// length of .L._errOutOfMemory_str0
118		.word 27
119	.L._errOutOfMemory_str0:
120		.asciz "fatal error: out of memory\n"
121	.align 4
122	_errOutOfMemory:
123		adr x0, .L._errOutOfMemory_str0
124		bl _prints
125		mov w0, #-1
126		bl exit
127	
128	// length of .L._errOutOfBounds_str0
129		.word 42
130	.L._errOutOfBounds_str0:
131		.asciz "fatal error: array index %d out of bounds\n"
132	.align 4
133	_errOutOfBounds:
134		adr x0, .L._errOutOfBounds_str0
135		bl printf
136		mov x0, #0
137		bl fflush
138		mov w0, #-1
139		bl exit
===========================================================
-- Finished

