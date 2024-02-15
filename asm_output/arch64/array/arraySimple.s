./valid/array/arraySimple.wacc
calling the reference compiler on ./valid/array/arraySimple.wacc
-- Test: arraySimple.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple array assignment and lookup

# Output:
# 42
#

# Program:

begin
  int[] a = [0] ;
  a[0] = 42 ;
  println a[0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arraySimple.s contents are:
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
11		// 1 element array
12		mov w0, #8
13		bl _malloc
14		mov x16, x0
15		// array pointers are shifted forwards by 4 bytes (to account for size)
16		add x16, x16, #4
17		mov x8, #1
18		str w8, [x16, #-4]
19		mov x8, #0
20		str w8, [x16, #0]
21		mov x8, x16
22		mov x19, x8
23		mov w17, #0
24		mov x8, #42
25		mov x7, x19
26		bl _arrStore4
27		// Stack pointer unchanged, no stack allocated arguments
28		mov w17, #0
29		mov x7, x19
30		bl _arrLoad4
31		mov w8, w7
32		mov x8, x8
33		mov x0, x8
34		// statement primitives do not return results (but will clobber r0/rax)
35		bl _printi
36		bl _println
37		// Stack pointer unchanged, no stack allocated variables
38		mov x0, #0
39		// pop {x19}
40		ldp x19, xzr, [sp], #16
41		// pop {fp, lr}
42		ldp fp, lr, [sp], #16
43		ret
44	
45	// length of .L._prints_str0
46		.word 4
47	.L._prints_str0:
48		.asciz "%.*s"
49	.align 4
50	_prints:
51		// push {lr}
52		stp lr, xzr, [sp, #-16]!
53		mov x2, x0
54		ldrsw x1, [x0, #-4]
55		adr x0, .L._prints_str0
56		bl printf
57		mov x0, #0
58		bl fflush
59		// pop {lr}
60		ldp lr, xzr, [sp], #16
61		ret
62	
63	_malloc:
64		// push {lr}
65		stp lr, xzr, [sp, #-16]!
66		bl malloc
67		cbz x0, _errOutOfMemory
68		// pop {lr}
69		ldp lr, xzr, [sp], #16
70		ret
71	
72	// length of .L._printi_str0
73		.word 2
74	.L._printi_str0:
75		.asciz "%d"
76	.align 4
77	_printi:
78		// push {lr}
79		stp lr, xzr, [sp, #-16]!
80		mov x1, x0
81		adr x0, .L._printi_str0
82		bl printf
83		mov x0, #0
84		bl fflush
85		// pop {lr}
86		ldp lr, xzr, [sp], #16
87		ret
88	
89	// length of .L._println_str0
90		.word 0
91	.L._println_str0:
92		.asciz ""
93	.align 4
94	_println:
95		// push {lr}
96		stp lr, xzr, [sp, #-16]!
97		adr x0, .L._println_str0
98		bl puts
99		mov x0, #0
100		bl fflush
101		// pop {lr}
102		ldp lr, xzr, [sp], #16
103		ret
104	
105	_arrStore4:
106		// Special calling convention: array ptr passed in X7, index in X17, value to store in X8, LR (W30) is used as general register
107		// push {lr}
108		stp lr, xzr, [sp, #-16]!
109		sxtw x17, w17
110		cmp w17, #0
111		csel x1, x17, x1, lt
112		b.lt _errOutOfBounds
113		ldrsw lr, [x7, #-4]
114		cmp w17, w30
115		csel x1, x17, x1, ge
116		b.ge _errOutOfBounds
117		str w8, [x7, x17, lsl #2]
118		// pop {lr}
119		ldp lr, xzr, [sp], #16
120		ret
121	
122	_arrLoad4:
123		// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7
124		// push {lr}
125		stp lr, xzr, [sp, #-16]!
126		sxtw x17, w17
127		cmp w17, #0
128		csel x1, x17, x1, lt
129		b.lt _errOutOfBounds
130		ldrsw lr, [x7, #-4]
131		cmp w17, w30
132		csel x1, x17, x1, ge
133		b.ge _errOutOfBounds
134		ldrsw x7, [x7, x17, lsl #2]
135		// pop {lr}
136		ldp lr, xzr, [sp], #16
137		ret
138	
139	// length of .L._errOutOfMemory_str0
140		.word 27
141	.L._errOutOfMemory_str0:
142		.asciz "fatal error: out of memory\n"
143	.align 4
144	_errOutOfMemory:
145		adr x0, .L._errOutOfMemory_str0
146		bl _prints
147		mov w0, #-1
148		bl exit
149	
150	// length of .L._errOutOfBounds_str0
151		.word 42
152	.L._errOutOfBounds_str0:
153		.asciz "fatal error: array index %d out of bounds\n"
154	.align 4
155	_errOutOfBounds:
156		adr x0, .L._errOutOfBounds_str0
157		bl printf
158		mov x0, #0
159		bl fflush
160		mov w0, #-1
161		bl exit
===========================================================
-- Finished

