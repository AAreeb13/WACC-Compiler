./valid/runtimeErr/arrayOutOfBounds/arrayNegBounds.wacc
calling the reference compiler on ./valid/runtimeErr/arrayOutOfBounds/arrayNegBounds.wacc
-- Test: arrayNegBounds.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt out of bounds array access (this ought to seg fault or similar)

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	int[] a = [43, 2, 18, 1] ;
  int[] b = [1, 2, 3] ;	
  println a[-2]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayNegBounds.s contents are:
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
29		// 3 element array
30		mov w0, #16
31		bl _malloc
32		mov x16, x0
33		// array pointers are shifted forwards by 4 bytes (to account for size)
34		add x16, x16, #4
35		mov x8, #3
36		str w8, [x16, #-4]
37		mov x8, #1
38		str w8, [x16, #0]
39		mov x8, #2
40		str w8, [x16, #4]
41		mov x8, #3
42		str w8, [x16, #8]
43		mov x8, x16
44		mov x20, x8
45		// Stack pointer unchanged, no stack allocated arguments
46		mov w17, #-2
47		mov x7, x19
48		bl _arrLoad4
49		mov w8, w7
50		mov x8, x8
51		mov x0, x8
52		// statement primitives do not return results (but will clobber r0/rax)
53		bl _printi
54		bl _println
55		// Stack pointer unchanged, no stack allocated variables
56		mov x0, #0
57		// pop {x19, x20}
58		ldp x19, x20, [sp], #16
59		// pop {fp, lr}
60		ldp fp, lr, [sp], #16
61		ret
62	
63	// length of .L._prints_str0
64		.word 4
65	.L._prints_str0:
66		.asciz "%.*s"
67	.align 4
68	_prints:
69		// push {lr}
70		stp lr, xzr, [sp, #-16]!
71		mov x2, x0
72		ldrsw x1, [x0, #-4]
73		adr x0, .L._prints_str0
74		bl printf
75		mov x0, #0
76		bl fflush
77		// pop {lr}
78		ldp lr, xzr, [sp], #16
79		ret
80	
81	_malloc:
82		// push {lr}
83		stp lr, xzr, [sp, #-16]!
84		bl malloc
85		cbz x0, _errOutOfMemory
86		// pop {lr}
87		ldp lr, xzr, [sp], #16
88		ret
89	
90	// length of .L._printi_str0
91		.word 2
92	.L._printi_str0:
93		.asciz "%d"
94	.align 4
95	_printi:
96		// push {lr}
97		stp lr, xzr, [sp, #-16]!
98		mov x1, x0
99		adr x0, .L._printi_str0
100		bl printf
101		mov x0, #0
102		bl fflush
103		// pop {lr}
104		ldp lr, xzr, [sp], #16
105		ret
106	
107	// length of .L._println_str0
108		.word 0
109	.L._println_str0:
110		.asciz ""
111	.align 4
112	_println:
113		// push {lr}
114		stp lr, xzr, [sp, #-16]!
115		adr x0, .L._println_str0
116		bl puts
117		mov x0, #0
118		bl fflush
119		// pop {lr}
120		ldp lr, xzr, [sp], #16
121		ret
122	
123	_arrLoad4:
124		// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7
125		// push {lr}
126		stp lr, xzr, [sp, #-16]!
127		sxtw x17, w17
128		cmp w17, #0
129		csel x1, x17, x1, lt
130		b.lt _errOutOfBounds
131		ldrsw lr, [x7, #-4]
132		cmp w17, w30
133		csel x1, x17, x1, ge
134		b.ge _errOutOfBounds
135		ldrsw x7, [x7, x17, lsl #2]
136		// pop {lr}
137		ldp lr, xzr, [sp], #16
138		ret
139	
140	// length of .L._errOutOfMemory_str0
141		.word 27
142	.L._errOutOfMemory_str0:
143		.asciz "fatal error: out of memory\n"
144	.align 4
145	_errOutOfMemory:
146		adr x0, .L._errOutOfMemory_str0
147		bl _prints
148		mov w0, #-1
149		bl exit
150	
151	// length of .L._errOutOfBounds_str0
152		.word 42
153	.L._errOutOfBounds_str0:
154		.asciz "fatal error: array index %d out of bounds\n"
155	.align 4
156	_errOutOfBounds:
157		adr x0, .L._errOutOfBounds_str0
158		bl printf
159		mov x0, #0
160		bl fflush
161		mov w0, #-1
162		bl exit
===========================================================
-- Finished

