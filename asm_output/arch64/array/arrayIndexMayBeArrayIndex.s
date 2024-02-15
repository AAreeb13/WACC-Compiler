./valid/array/arrayIndexMayBeArrayIndex.wacc
calling the reference compiler on ./valid/array/arrayIndexMayBeArrayIndex.wacc
-- Test: arrayIndexMayBeArrayIndex.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Testing recursive array indexing

# Output:
# 6
# 7
# 8
#

# Program:

begin
    int[] idxs1 = [2, 0, 1] ;
    int[] idxs2 = [1, 2, 0] ;
    # idxs1[idxs2[0]] = 0
    # idxs1[idxs2[1]] = 1
    # idxs1[idxs2[2]] = 2
    int[] xs = [5, 6, 7] ;
    int i = 0 ;
    while i != 3 do
        xs[idxs1[idxs2[i]]] = xs[idxs1[idxs2[i]]] + 1 ;
        println (xs[idxs1[idxs2[i]]]) ;
        i = i + 1
    done
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayIndexMayBeArrayIndex.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19, x20, x21, x22}
8		stp x19, x20, [sp, #-32]!
9		stp x21, x22, [sp, #16]
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
20		mov x8, #2
21		str w8, [x16, #0]
22		mov x8, #0
23		str w8, [x16, #4]
24		mov x8, #1
25		str w8, [x16, #8]
26		mov x8, x16
27		mov x19, x8
28		// 3 element array
29		mov w0, #16
30		bl _malloc
31		mov x16, x0
32		// array pointers are shifted forwards by 4 bytes (to account for size)
33		add x16, x16, #4
34		mov x8, #3
35		str w8, [x16, #-4]
36		mov x8, #1
37		str w8, [x16, #0]
38		mov x8, #2
39		str w8, [x16, #4]
40		mov x8, #0
41		str w8, [x16, #8]
42		mov x8, x16
43		mov x20, x8
44		// 3 element array
45		mov w0, #16
46		bl _malloc
47		mov x16, x0
48		// array pointers are shifted forwards by 4 bytes (to account for size)
49		add x16, x16, #4
50		mov x8, #3
51		str w8, [x16, #-4]
52		mov x8, #5
53		str w8, [x16, #0]
54		mov x8, #6
55		str w8, [x16, #4]
56		mov x8, #7
57		str w8, [x16, #8]
58		mov x8, x16
59		mov x21, x8
60		mov x8, #0
61		mov x22, x8
62		b .L0
63	.L1:
64		mov w17, w22
65		mov x7, x20
66		bl _arrLoad4
67		mov w8, w7
68		mov x8, x8
69		// push {x8}
70		stp x8, xzr, [sp, #-16]!
71		// pop {x17}
72		ldp x17, xzr, [sp], #16
73		mov w17, w17
74		mov x7, x19
75		bl _arrLoad4
76		mov w8, w7
77		mov x8, x8
78		// push {x8}
79		stp x8, xzr, [sp, #-16]!
80		// pop {x17}
81		ldp x17, xzr, [sp], #16
82		mov w17, w17
83		mov x7, x21
84		bl _arrLoad4
85		mov w8, w7
86		adds w8, w8, #1
87		b.vs _errOverflow
88		sxtw x8, w8
89		// push {x8}
90		stp x8, xzr, [sp, #-16]!
91		mov w17, w22
92		mov x7, x20
93		bl _arrLoad4
94		mov w8, w7
95		mov x8, x8
96		// push {x8}
97		stp x8, xzr, [sp, #-16]!
98		// pop {x17}
99		ldp x17, xzr, [sp], #16
100		mov w17, w17
101		mov x7, x19
102		bl _arrLoad4
103		mov w8, w7
104		mov x8, x8
105		// push {x8}
106		stp x8, xzr, [sp, #-16]!
107		// pop {x17}
108		ldp x17, xzr, [sp], #16
109		mov w17, w17
110		// pop {x8}
111		ldp x8, xzr, [sp], #16
112		mov x8, x8
113		mov x7, x21
114		bl _arrStore4
115		// Stack pointer unchanged, no stack allocated arguments
116		mov w17, w22
117		mov x7, x20
118		bl _arrLoad4
119		mov w8, w7
120		mov x8, x8
121		// push {x8}
122		stp x8, xzr, [sp, #-16]!
123		// pop {x17}
124		ldp x17, xzr, [sp], #16
125		mov w17, w17
126		mov x7, x19
127		bl _arrLoad4
128		mov w8, w7
129		mov x8, x8
130		// push {x8}
131		stp x8, xzr, [sp, #-16]!
132		// pop {x17}
133		ldp x17, xzr, [sp], #16
134		mov w17, w17
135		mov x7, x21
136		bl _arrLoad4
137		mov w8, w7
138		mov x8, x8
139		mov x0, x8
140		// statement primitives do not return results (but will clobber r0/rax)
141		bl _printi
142		bl _println
143		adds w8, w22, #1
144		b.vs _errOverflow
145		sxtw x8, w8
146		// push {x8}
147		stp x8, xzr, [sp, #-16]!
148		// pop {x8}
149		ldp x8, xzr, [sp], #16
150		mov x8, x8
151		mov x22, x8
152	.L0:
153		cmp x22, #3
154		b.ne .L1
155		// Stack pointer unchanged, no stack allocated variables
156		mov x0, #0
157		// pop {x19, x20, x21, x22}
158		ldp x21, x22, [sp, #16]
159		ldp x19, x20, [sp], #32
160		// pop {fp, lr}
161		ldp fp, lr, [sp], #16
162		ret
163	
164	// length of .L._prints_str0
165		.word 4
166	.L._prints_str0:
167		.asciz "%.*s"
168	.align 4
169	_prints:
170		// push {lr}
171		stp lr, xzr, [sp, #-16]!
172		mov x2, x0
173		ldrsw x1, [x0, #-4]
174		adr x0, .L._prints_str0
175		bl printf
176		mov x0, #0
177		bl fflush
178		// pop {lr}
179		ldp lr, xzr, [sp], #16
180		ret
181	
182	_malloc:
183		// push {lr}
184		stp lr, xzr, [sp, #-16]!
185		bl malloc
186		cbz x0, _errOutOfMemory
187		// pop {lr}
188		ldp lr, xzr, [sp], #16
189		ret
190	
191	// length of .L._printi_str0
192		.word 2
193	.L._printi_str0:
194		.asciz "%d"
195	.align 4
196	_printi:
197		// push {lr}
198		stp lr, xzr, [sp, #-16]!
199		mov x1, x0
200		adr x0, .L._printi_str0
201		bl printf
202		mov x0, #0
203		bl fflush
204		// pop {lr}
205		ldp lr, xzr, [sp], #16
206		ret
207	
208	// length of .L._println_str0
209		.word 0
210	.L._println_str0:
211		.asciz ""
212	.align 4
213	_println:
214		// push {lr}
215		stp lr, xzr, [sp, #-16]!
216		adr x0, .L._println_str0
217		bl puts
218		mov x0, #0
219		bl fflush
220		// pop {lr}
221		ldp lr, xzr, [sp], #16
222		ret
223	
224	_arrStore4:
225		// Special calling convention: array ptr passed in X7, index in X17, value to store in X8, LR (W30) is used as general register
226		// push {lr}
227		stp lr, xzr, [sp, #-16]!
228		sxtw x17, w17
229		cmp w17, #0
230		csel x1, x17, x1, lt
231		b.lt _errOutOfBounds
232		ldrsw lr, [x7, #-4]
233		cmp w17, w30
234		csel x1, x17, x1, ge
235		b.ge _errOutOfBounds
236		str w8, [x7, x17, lsl #2]
237		// pop {lr}
238		ldp lr, xzr, [sp], #16
239		ret
240	
241	_arrLoad4:
242		// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7
243		// push {lr}
244		stp lr, xzr, [sp, #-16]!
245		sxtw x17, w17
246		cmp w17, #0
247		csel x1, x17, x1, lt
248		b.lt _errOutOfBounds
249		ldrsw lr, [x7, #-4]
250		cmp w17, w30
251		csel x1, x17, x1, ge
252		b.ge _errOutOfBounds
253		ldrsw x7, [x7, x17, lsl #2]
254		// pop {lr}
255		ldp lr, xzr, [sp], #16
256		ret
257	
258	// length of .L._errOutOfMemory_str0
259		.word 27
260	.L._errOutOfMemory_str0:
261		.asciz "fatal error: out of memory\n"
262	.align 4
263	_errOutOfMemory:
264		adr x0, .L._errOutOfMemory_str0
265		bl _prints
266		mov w0, #-1
267		bl exit
268	
269	// length of .L._errOutOfBounds_str0
270		.word 42
271	.L._errOutOfBounds_str0:
272		.asciz "fatal error: array index %d out of bounds\n"
273	.align 4
274	_errOutOfBounds:
275		adr x0, .L._errOutOfBounds_str0
276		bl printf
277		mov x0, #0
278		bl fflush
279		mov w0, #-1
280		bl exit
281	
282	// length of .L._errOverflow_str0
283		.word 52
284	.L._errOverflow_str0:
285		.asciz "fatal error: integer overflow or underflow occurred\n"
286	.align 4
287	_errOverflow:
288		adr x0, .L._errOverflow_str0
289		bl _prints
290		mov w0, #-1
291		bl exit
===========================================================
-- Finished

