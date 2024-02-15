./valid/array/arrayOnHeap.wacc
calling the reference compiler on ./valid/array/arrayOnHeap.wacc
-- Test: arrayOnHeap.wacc

-- Uploaded file: 
---------------------------------------------------------------
# ensures that arrays are heap allocated

# Output:
# false
# 0
# 1
#

# Program:

begin
  int[] dummy_arr = [] ;
  int[][] arrs = [dummy_arr, dummy_arr] ;
  int i = 0;
  while i < 2 do
    int[] arr = [i] ;
    arrs[i] = arr ;
    i = i + 1
  done;

  # should have different addresses
  println arrs[0] == arrs[1];
  # should have different values
  println arrs[0][0];
  println arrs[1][0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayOnHeap.s contents are:
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
12		// 0 element array
13		mov w0, #4
14		bl _malloc
15		mov x16, x0
16		// array pointers are shifted forwards by 4 bytes (to account for size)
17		add x16, x16, #4
18		mov x8, #0
19		str w8, [x16, #-4]
20		mov x8, x16
21		mov x19, x8
22		// 2 element array
23		mov w0, #20
24		bl _malloc
25		mov x16, x0
26		// array pointers are shifted forwards by 4 bytes (to account for size)
27		add x16, x16, #4
28		mov x8, #2
29		str w8, [x16, #-4]
30		mov x8, x19
31		str x8, [x16, #0]
32		mov x8, x19
33		str x8, [x16, #8]
34		mov x8, x16
35		mov x20, x8
36		mov x8, #0
37		mov x21, x8
38		b .L0
39	.L1:
40		// Stack pointer unchanged, no stack allocated variables
41		// 1 element array
42		mov w0, #8
43		bl _malloc
44		mov x16, x0
45		// array pointers are shifted forwards by 4 bytes (to account for size)
46		add x16, x16, #4
47		mov x8, #1
48		str w8, [x16, #-4]
49		mov x8, x21
50		str w8, [x16, #0]
51		mov x8, x16
52		mov x22, x8
53		mov w17, w21
54		mov x8, x22
55		mov x7, x20
56		bl _arrStore8
57		adds w8, w21, #1
58		b.vs _errOverflow
59		sxtw x8, w8
60		// push {x8}
61		stp x8, xzr, [sp, #-16]!
62		// pop {x8}
63		ldp x8, xzr, [sp], #16
64		mov x8, x8
65		mov x21, x8
66		// Stack pointer unchanged, no stack allocated variables
67	.L0:
68		cmp x21, #2
69		b.lt .L1
70		// Stack pointer unchanged, no stack allocated arguments
71		mov w17, #1
72		mov x7, x20
73		bl _arrLoad8
74		mov x9, x7
75		mov w17, #0
76		mov x7, x20
77		bl _arrLoad8
78		mov x8, x7
79		cmp x8, x9
80		cset x8, eq
81		// push {x8}
82		stp x8, xzr, [sp, #-16]!
83		// pop {x8}
84		ldp x8, xzr, [sp], #16
85		mov x8, x8
86		mov x0, x8
87		// statement primitives do not return results (but will clobber r0/rax)
88		bl _printb
89		bl _println
90		// Stack pointer unchanged, no stack allocated arguments
91		mov w17, #0
92		mov x7, x20
93		bl _arrLoad8
94		mov x8, x7
95		mov x8, x8
96		// push {x8}
97		stp x8, xzr, [sp, #-16]!
98		mov w17, #0
99		// pop {x8}
100		ldp x8, xzr, [sp], #16
101		mov x7, x8
102		bl _arrLoad4
103		mov w8, w7
104		mov x8, x8
105		mov x0, x8
106		// statement primitives do not return results (but will clobber r0/rax)
107		bl _printi
108		bl _println
109		// Stack pointer unchanged, no stack allocated arguments
110		mov w17, #1
111		mov x7, x20
112		bl _arrLoad8
113		mov x8, x7
114		mov x8, x8
115		// push {x8}
116		stp x8, xzr, [sp, #-16]!
117		mov w17, #0
118		// pop {x8}
119		ldp x8, xzr, [sp], #16
120		mov x7, x8
121		bl _arrLoad4
122		mov w8, w7
123		mov x8, x8
124		mov x0, x8
125		// statement primitives do not return results (but will clobber r0/rax)
126		bl _printi
127		bl _println
128		// Stack pointer unchanged, no stack allocated variables
129		mov x0, #0
130		// pop {x19, x20, x21, x22}
131		ldp x21, x22, [sp, #16]
132		ldp x19, x20, [sp], #32
133		// pop {fp, lr}
134		ldp fp, lr, [sp], #16
135		ret
136	
137	// length of .L._prints_str0
138		.word 4
139	.L._prints_str0:
140		.asciz "%.*s"
141	.align 4
142	_prints:
143		// push {lr}
144		stp lr, xzr, [sp, #-16]!
145		mov x2, x0
146		ldrsw x1, [x0, #-4]
147		adr x0, .L._prints_str0
148		bl printf
149		mov x0, #0
150		bl fflush
151		// pop {lr}
152		ldp lr, xzr, [sp], #16
153		ret
154	
155	_malloc:
156		// push {lr}
157		stp lr, xzr, [sp, #-16]!
158		bl malloc
159		cbz x0, _errOutOfMemory
160		// pop {lr}
161		ldp lr, xzr, [sp], #16
162		ret
163	
164	// length of .L._printi_str0
165		.word 2
166	.L._printi_str0:
167		.asciz "%d"
168	.align 4
169	_printi:
170		// push {lr}
171		stp lr, xzr, [sp, #-16]!
172		mov x1, x0
173		adr x0, .L._printi_str0
174		bl printf
175		mov x0, #0
176		bl fflush
177		// pop {lr}
178		ldp lr, xzr, [sp], #16
179		ret
180	
181	// length of .L._printb_str0
182		.word 5
183	.L._printb_str0:
184		.asciz "false"
185	// length of .L._printb_str1
186		.word 4
187	.L._printb_str1:
188		.asciz "true"
189	// length of .L._printb_str2
190		.word 4
191	.L._printb_str2:
192		.asciz "%.*s"
193	.align 4
194	_printb:
195		// push {lr}
196		stp lr, xzr, [sp, #-16]!
197		cmp w0, #0
198		b.ne .L_printb0
199		adr x2, .L._printb_str0
200		b .L_printb1
201	.L_printb0:
202		adr x2, .L._printb_str1
203	.L_printb1:
204		ldrsw x1, [x2, #-4]
205		adr x0, .L._printb_str2
206		bl printf
207		mov x0, #0
208		bl fflush
209		// pop {lr}
210		ldp lr, xzr, [sp], #16
211		ret
212	
213	// length of .L._println_str0
214		.word 0
215	.L._println_str0:
216		.asciz ""
217	.align 4
218	_println:
219		// push {lr}
220		stp lr, xzr, [sp, #-16]!
221		adr x0, .L._println_str0
222		bl puts
223		mov x0, #0
224		bl fflush
225		// pop {lr}
226		ldp lr, xzr, [sp], #16
227		ret
228	
229	_arrLoad8:
230		// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7
231		// push {lr}
232		stp lr, xzr, [sp, #-16]!
233		sxtw x17, w17
234		cmp w17, #0
235		csel x1, x17, x1, lt
236		b.lt _errOutOfBounds
237		ldrsw lr, [x7, #-4]
238		cmp w17, w30
239		csel x1, x17, x1, ge
240		b.ge _errOutOfBounds
241		ldr x7, [x7, x17, lsl #3]
242		// pop {lr}
243		ldp lr, xzr, [sp], #16
244		ret
245	
246	_arrLoad4:
247		// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7
248		// push {lr}
249		stp lr, xzr, [sp, #-16]!
250		sxtw x17, w17
251		cmp w17, #0
252		csel x1, x17, x1, lt
253		b.lt _errOutOfBounds
254		ldrsw lr, [x7, #-4]
255		cmp w17, w30
256		csel x1, x17, x1, ge
257		b.ge _errOutOfBounds
258		ldrsw x7, [x7, x17, lsl #2]
259		// pop {lr}
260		ldp lr, xzr, [sp], #16
261		ret
262	
263	_arrStore8:
264		// Special calling convention: array ptr passed in X7, index in X17, value to store in X8, LR (W30) is used as general register
265		// push {lr}
266		stp lr, xzr, [sp, #-16]!
267		sxtw x17, w17
268		cmp w17, #0
269		csel x1, x17, x1, lt
270		b.lt _errOutOfBounds
271		ldrsw lr, [x7, #-4]
272		cmp w17, w30
273		csel x1, x17, x1, ge
274		b.ge _errOutOfBounds
275		str x8, [x7, x17, lsl #3]
276		// pop {lr}
277		ldp lr, xzr, [sp], #16
278		ret
279	
280	// length of .L._errOutOfMemory_str0
281		.word 27
282	.L._errOutOfMemory_str0:
283		.asciz "fatal error: out of memory\n"
284	.align 4
285	_errOutOfMemory:
286		adr x0, .L._errOutOfMemory_str0
287		bl _prints
288		mov w0, #-1
289		bl exit
290	
291	// length of .L._errOutOfBounds_str0
292		.word 42
293	.L._errOutOfBounds_str0:
294		.asciz "fatal error: array index %d out of bounds\n"
295	.align 4
296	_errOutOfBounds:
297		adr x0, .L._errOutOfBounds_str0
298		bl printf
299		mov x0, #0
300		bl fflush
301		mov w0, #-1
302		bl exit
303	
304	// length of .L._errOverflow_str0
305		.word 52
306	.L._errOverflow_str0:
307		.asciz "fatal error: integer overflow or underflow occurred\n"
308	.align 4
309	_errOverflow:
310		adr x0, .L._errOverflow_str0
311		bl _prints
312		mov w0, #-1
313		bl exit
===========================================================
-- Finished

