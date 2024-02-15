./valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc
calling the reference compiler on ./valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc
-- Test: arrayOutOfBoundsWrite.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt write out of array bounds (gods, this really should not work!)

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin

  int[] b = [1, 2, 3] ;
	int[] a = [43, 2, 18, 1] ;
	a[5] = 100 ;
  println a[5]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayOutOfBoundsWrite.s contents are:
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
11		// 3 element array
12		mov w0, #16
13		bl _malloc
14		mov x16, x0
15		// array pointers are shifted forwards by 4 bytes (to account for size)
16		add x16, x16, #4
17		mov x8, #3
18		str w8, [x16, #-4]
19		mov x8, #1
20		str w8, [x16, #0]
21		mov x8, #2
22		str w8, [x16, #4]
23		mov x8, #3
24		str w8, [x16, #8]
25		mov x8, x16
26		mov x19, x8
27		// 4 element array
28		mov w0, #20
29		bl _malloc
30		mov x16, x0
31		// array pointers are shifted forwards by 4 bytes (to account for size)
32		add x16, x16, #4
33		mov x8, #4
34		str w8, [x16, #-4]
35		mov x8, #43
36		str w8, [x16, #0]
37		mov x8, #2
38		str w8, [x16, #4]
39		mov x8, #18
40		str w8, [x16, #8]
41		mov x8, #1
42		str w8, [x16, #12]
43		mov x8, x16
44		mov x20, x8
45		mov w17, #5
46		mov x8, #100
47		mov x7, x20
48		bl _arrStore4
49		// Stack pointer unchanged, no stack allocated arguments
50		mov w17, #5
51		mov x7, x20
52		bl _arrLoad4
53		mov w8, w7
54		mov x8, x8
55		mov x0, x8
56		// statement primitives do not return results (but will clobber r0/rax)
57		bl _printi
58		bl _println
59		// Stack pointer unchanged, no stack allocated variables
60		mov x0, #0
61		// pop {x19, x20}
62		ldp x19, x20, [sp], #16
63		// pop {fp, lr}
64		ldp fp, lr, [sp], #16
65		ret
66	
67	// length of .L._prints_str0
68		.word 4
69	.L._prints_str0:
70		.asciz "%.*s"
71	.align 4
72	_prints:
73		// push {lr}
74		stp lr, xzr, [sp, #-16]!
75		mov x2, x0
76		ldrsw x1, [x0, #-4]
77		adr x0, .L._prints_str0
78		bl printf
79		mov x0, #0
80		bl fflush
81		// pop {lr}
82		ldp lr, xzr, [sp], #16
83		ret
84	
85	_malloc:
86		// push {lr}
87		stp lr, xzr, [sp, #-16]!
88		bl malloc
89		cbz x0, _errOutOfMemory
90		// pop {lr}
91		ldp lr, xzr, [sp], #16
92		ret
93	
94	// length of .L._printi_str0
95		.word 2
96	.L._printi_str0:
97		.asciz "%d"
98	.align 4
99	_printi:
100		// push {lr}
101		stp lr, xzr, [sp, #-16]!
102		mov x1, x0
103		adr x0, .L._printi_str0
104		bl printf
105		mov x0, #0
106		bl fflush
107		// pop {lr}
108		ldp lr, xzr, [sp], #16
109		ret
110	
111	// length of .L._println_str0
112		.word 0
113	.L._println_str0:
114		.asciz ""
115	.align 4
116	_println:
117		// push {lr}
118		stp lr, xzr, [sp, #-16]!
119		adr x0, .L._println_str0
120		bl puts
121		mov x0, #0
122		bl fflush
123		// pop {lr}
124		ldp lr, xzr, [sp], #16
125		ret
126	
127	_arrStore4:
128		// Special calling convention: array ptr passed in X7, index in X17, value to store in X8, LR (W30) is used as general register
129		// push {lr}
130		stp lr, xzr, [sp, #-16]!
131		sxtw x17, w17
132		cmp w17, #0
133		csel x1, x17, x1, lt
134		b.lt _errOutOfBounds
135		ldrsw lr, [x7, #-4]
136		cmp w17, w30
137		csel x1, x17, x1, ge
138		b.ge _errOutOfBounds
139		str w8, [x7, x17, lsl #2]
140		// pop {lr}
141		ldp lr, xzr, [sp], #16
142		ret
143	
144	_arrLoad4:
145		// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7
146		// push {lr}
147		stp lr, xzr, [sp, #-16]!
148		sxtw x17, w17
149		cmp w17, #0
150		csel x1, x17, x1, lt
151		b.lt _errOutOfBounds
152		ldrsw lr, [x7, #-4]
153		cmp w17, w30
154		csel x1, x17, x1, ge
155		b.ge _errOutOfBounds
156		ldrsw x7, [x7, x17, lsl #2]
157		// pop {lr}
158		ldp lr, xzr, [sp], #16
159		ret
160	
161	// length of .L._errOutOfMemory_str0
162		.word 27
163	.L._errOutOfMemory_str0:
164		.asciz "fatal error: out of memory\n"
165	.align 4
166	_errOutOfMemory:
167		adr x0, .L._errOutOfMemory_str0
168		bl _prints
169		mov w0, #-1
170		bl exit
171	
172	// length of .L._errOutOfBounds_str0
173		.word 42
174	.L._errOutOfBounds_str0:
175		.asciz "fatal error: array index %d out of bounds\n"
176	.align 4
177	_errOutOfBounds:
178		adr x0, .L._errOutOfBounds_str0
179		bl printf
180		mov x0, #0
181		bl fflush
182		mov w0, #-1
183		bl exit
===========================================================
-- Finished

