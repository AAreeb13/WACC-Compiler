./valid/array/arrayLookup.wacc
calling the reference compiler on ./valid/array/arrayLookup.wacc
-- Test: arrayLookup.wacc

-- Uploaded file: 
---------------------------------------------------------------
# check first element of array

# Output:
# 43
#

# Program:

begin
  int[] a = [43, 2, 18, 1] ;
  println a[0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayLookup.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19}
8		stp x19, xzr, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		// 4 element array
12		mov w0, #20
13		bl _malloc
14		mov x16, x0
15		// array pointers are shifted forwards by 4 bytes (to account for size)
16		add x16, x16, #4
17		mov x8, #4
18		str w8, [x16, #-4]
19		mov x8, #43
20		str w8, [x16, #0]
21		mov x8, #2
22		str w8, [x16, #4]
23		mov x8, #18
24		str w8, [x16, #8]
25		mov x8, #1
26		str w8, [x16, #12]
27		mov x8, x16
28		mov x19, x8
29		// Stack pointer unchanged, no stack allocated arguments
30		mov w17, #0
31		mov x7, x19
32		bl _arrLoad4
33		mov w8, w7
34		mov x8, x8
35		mov x0, x8
36		// statement primitives do not return results (but will clobber r0/rax)
37		bl _printi
38		bl _println
39		// Stack pointer unchanged, no stack allocated variables
40		mov x0, #0
41		// pop {x19}
42		ldp x19, xzr, [sp], #16
43		// pop {fp, lr}
44		ldp fp, lr, [sp], #16
45		ret
46	
47	// length of .L._prints_str0
48		.word 4
49	.L._prints_str0:
50		.asciz "%.*s"
51	.align 4
52	_prints:
53		// push {lr}
54		stp lr, xzr, [sp, #-16]!
55		mov x2, x0
56		ldrsw x1, [x0, #-4]
57		adr x0, .L._prints_str0
58		bl printf
59		mov x0, #0
60		bl fflush
61		// pop {lr}
62		ldp lr, xzr, [sp], #16
63		ret
64	
65	_malloc:
66		// push {lr}
67		stp lr, xzr, [sp, #-16]!
68		bl malloc
69		cbz x0, _errOutOfMemory
70		// pop {lr}
71		ldp lr, xzr, [sp], #16
72		ret
73	
74	// length of .L._printi_str0
75		.word 2
76	.L._printi_str0:
77		.asciz "%d"
78	.align 4
79	_printi:
80		// push {lr}
81		stp lr, xzr, [sp, #-16]!
82		mov x1, x0
83		adr x0, .L._printi_str0
84		bl printf
85		mov x0, #0
86		bl fflush
87		// pop {lr}
88		ldp lr, xzr, [sp], #16
89		ret
90	
91	// length of .L._println_str0
92		.word 0
93	.L._println_str0:
94		.asciz ""
95	.align 4
96	_println:
97		// push {lr}
98		stp lr, xzr, [sp, #-16]!
99		adr x0, .L._println_str0
100		bl puts
101		mov x0, #0
102		bl fflush
103		// pop {lr}
104		ldp lr, xzr, [sp], #16
105		ret
106	
107	_arrLoad4:
108		// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7
109		// push {lr}
110		stp lr, xzr, [sp, #-16]!
111		sxtw x17, w17
112		cmp w17, #0
113		csel x1, x17, x1, lt
114		b.lt _errOutOfBounds
115		ldrsw lr, [x7, #-4]
116		cmp w17, w30
117		csel x1, x17, x1, ge
118		b.ge _errOutOfBounds
119		ldrsw x7, [x7, x17, lsl #2]
120		// pop {lr}
121		ldp lr, xzr, [sp], #16
122		ret
123	
124	// length of .L._errOutOfMemory_str0
125		.word 27
126	.L._errOutOfMemory_str0:
127		.asciz "fatal error: out of memory\n"
128	.align 4
129	_errOutOfMemory:
130		adr x0, .L._errOutOfMemory_str0
131		bl _prints
132		mov w0, #-1
133		bl exit
134	
135	// length of .L._errOutOfBounds_str0
136		.word 42
137	.L._errOutOfBounds_str0:
138		.asciz "fatal error: array index %d out of bounds\n"
139	.align 4
140	_errOutOfBounds:
141		adr x0, .L._errOutOfBounds_str0
142		bl printf
143		mov x0, #0
144		bl fflush
145		mov w0, #-1
146		bl exit
===========================================================
-- Finished

