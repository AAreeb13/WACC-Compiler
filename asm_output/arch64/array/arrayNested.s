./valid/array/arrayNested.wacc
calling the reference compiler on ./valid/array/arrayNested.wacc
-- Test: arrayNested.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array declaration and assignment

# Output:
# 3
# 3
#

# Program:

begin
  int[] a = [1,2,3];
  int[] b = [3,4];
  int[][] c = [a,b] ;
  println c[0][2] ;
  println c[1][0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayNested.s contents are:
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
12		// 3 element array
13		mov w0, #16
14		bl _malloc
15		mov x16, x0
16		// array pointers are shifted forwards by 4 bytes (to account for size)
17		add x16, x16, #4
18		mov x8, #3
19		str w8, [x16, #-4]
20		mov x8, #1
21		str w8, [x16, #0]
22		mov x8, #2
23		str w8, [x16, #4]
24		mov x8, #3
25		str w8, [x16, #8]
26		mov x8, x16
27		mov x19, x8
28		// 2 element array
29		mov w0, #12
30		bl _malloc
31		mov x16, x0
32		// array pointers are shifted forwards by 4 bytes (to account for size)
33		add x16, x16, #4
34		mov x8, #2
35		str w8, [x16, #-4]
36		mov x8, #3
37		str w8, [x16, #0]
38		mov x8, #4
39		str w8, [x16, #4]
40		mov x8, x16
41		mov x20, x8
42		// 2 element array
43		mov w0, #20
44		bl _malloc
45		mov x16, x0
46		// array pointers are shifted forwards by 4 bytes (to account for size)
47		add x16, x16, #4
48		mov x8, #2
49		str w8, [x16, #-4]
50		mov x8, x19
51		str x8, [x16, #0]
52		mov x8, x20
53		str x8, [x16, #8]
54		mov x8, x16
55		mov x21, x8
56		// Stack pointer unchanged, no stack allocated arguments
57		mov w17, #0
58		mov x7, x21
59		bl _arrLoad8
60		mov x8, x7
61		mov x8, x8
62		// push {x8}
63		stp x8, xzr, [sp, #-16]!
64		mov w17, #2
65		// pop {x8}
66		ldp x8, xzr, [sp], #16
67		mov x7, x8
68		bl _arrLoad4
69		mov w8, w7
70		mov x8, x8
71		mov x0, x8
72		// statement primitives do not return results (but will clobber r0/rax)
73		bl _printi
74		bl _println
75		// Stack pointer unchanged, no stack allocated arguments
76		mov w17, #1
77		mov x7, x21
78		bl _arrLoad8
79		mov x8, x7
80		mov x8, x8
81		// push {x8}
82		stp x8, xzr, [sp, #-16]!
83		mov w17, #0
84		// pop {x8}
85		ldp x8, xzr, [sp], #16
86		mov x7, x8
87		bl _arrLoad4
88		mov w8, w7
89		mov x8, x8
90		mov x0, x8
91		// statement primitives do not return results (but will clobber r0/rax)
92		bl _printi
93		bl _println
94		// Stack pointer unchanged, no stack allocated variables
95		mov x0, #0
96		// pop {x19, x20, x21}
97		ldp x21, xzr, [sp, #16]
98		ldp x19, x20, [sp], #32
99		// pop {fp, lr}
100		ldp fp, lr, [sp], #16
101		ret
102	
103	// length of .L._prints_str0
104		.word 4
105	.L._prints_str0:
106		.asciz "%.*s"
107	.align 4
108	_prints:
109		// push {lr}
110		stp lr, xzr, [sp, #-16]!
111		mov x2, x0
112		ldrsw x1, [x0, #-4]
113		adr x0, .L._prints_str0
114		bl printf
115		mov x0, #0
116		bl fflush
117		// pop {lr}
118		ldp lr, xzr, [sp], #16
119		ret
120	
121	_malloc:
122		// push {lr}
123		stp lr, xzr, [sp, #-16]!
124		bl malloc
125		cbz x0, _errOutOfMemory
126		// pop {lr}
127		ldp lr, xzr, [sp], #16
128		ret
129	
130	// length of .L._printi_str0
131		.word 2
132	.L._printi_str0:
133		.asciz "%d"
134	.align 4
135	_printi:
136		// push {lr}
137		stp lr, xzr, [sp, #-16]!
138		mov x1, x0
139		adr x0, .L._printi_str0
140		bl printf
141		mov x0, #0
142		bl fflush
143		// pop {lr}
144		ldp lr, xzr, [sp], #16
145		ret
146	
147	// length of .L._println_str0
148		.word 0
149	.L._println_str0:
150		.asciz ""
151	.align 4
152	_println:
153		// push {lr}
154		stp lr, xzr, [sp, #-16]!
155		adr x0, .L._println_str0
156		bl puts
157		mov x0, #0
158		bl fflush
159		// pop {lr}
160		ldp lr, xzr, [sp], #16
161		ret
162	
163	_arrLoad8:
164		// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7
165		// push {lr}
166		stp lr, xzr, [sp, #-16]!
167		sxtw x17, w17
168		cmp w17, #0
169		csel x1, x17, x1, lt
170		b.lt _errOutOfBounds
171		ldrsw lr, [x7, #-4]
172		cmp w17, w30
173		csel x1, x17, x1, ge
174		b.ge _errOutOfBounds
175		ldr x7, [x7, x17, lsl #3]
176		// pop {lr}
177		ldp lr, xzr, [sp], #16
178		ret
179	
180	_arrLoad4:
181		// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7
182		// push {lr}
183		stp lr, xzr, [sp, #-16]!
184		sxtw x17, w17
185		cmp w17, #0
186		csel x1, x17, x1, lt
187		b.lt _errOutOfBounds
188		ldrsw lr, [x7, #-4]
189		cmp w17, w30
190		csel x1, x17, x1, ge
191		b.ge _errOutOfBounds
192		ldrsw x7, [x7, x17, lsl #2]
193		// pop {lr}
194		ldp lr, xzr, [sp], #16
195		ret
196	
197	// length of .L._errOutOfMemory_str0
198		.word 27
199	.L._errOutOfMemory_str0:
200		.asciz "fatal error: out of memory\n"
201	.align 4
202	_errOutOfMemory:
203		adr x0, .L._errOutOfMemory_str0
204		bl _prints
205		mov w0, #-1
206		bl exit
207	
208	// length of .L._errOutOfBounds_str0
209		.word 42
210	.L._errOutOfBounds_str0:
211		.asciz "fatal error: array index %d out of bounds\n"
212	.align 4
213	_errOutOfBounds:
214		adr x0, .L._errOutOfBounds_str0
215		bl printf
216		mov x0, #0
217		bl fflush
218		mov w0, #-1
219		bl exit
===========================================================
-- Finished

