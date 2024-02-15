./valid/array/array.wacc
calling the reference compiler on ./valid/array/array.wacc
-- Test: array.wacc

-- Uploaded file: 
---------------------------------------------------------------
# moderate complexity array manipulations

# Output:
# #addrs# = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
#

# Program:

begin
  int[] a = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ;
  int i = 0 ;
  while i < 10
  do
    a[i] = i ;
    i = i + 1
  done ;
  print a ;
  print " = {" ;
  i = 0 ;
  while i < 10
  do
    print a[i] ;
    if i < 9
    then
      print ", "
    else
      skip
    fi ;
    i = i + 1
  done ;
  println "}"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
array.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 4
3	.L.str0:
4		.asciz " = {"
5	// length of .L.str1
6		.word 2
7	.L.str1:
8		.asciz ", "
9	// length of .L.str2
10		.word 1
11	.L.str2:
12		.asciz "}"
13	.align 4
14	.text
15	.global main
16	main:
17		// push {fp, lr}
18		stp fp, lr, [sp, #-16]!
19		// push {x19, x20}
20		stp x19, x20, [sp, #-16]!
21		mov fp, sp
22		// Stack pointer unchanged, no stack allocated variables
23		// 10 element array
24		mov w0, #44
25		bl _malloc
26		mov x16, x0
27		// array pointers are shifted forwards by 4 bytes (to account for size)
28		add x16, x16, #4
29		mov x8, #10
30		str w8, [x16, #-4]
31		mov x8, #0
32		str w8, [x16, #0]
33		mov x8, #0
34		str w8, [x16, #4]
35		mov x8, #0
36		str w8, [x16, #8]
37		mov x8, #0
38		str w8, [x16, #12]
39		mov x8, #0
40		str w8, [x16, #16]
41		mov x8, #0
42		str w8, [x16, #20]
43		mov x8, #0
44		str w8, [x16, #24]
45		mov x8, #0
46		str w8, [x16, #28]
47		mov x8, #0
48		str w8, [x16, #32]
49		mov x8, #0
50		str w8, [x16, #36]
51		mov x8, x16
52		mov x19, x8
53		mov x8, #0
54		mov x20, x8
55		b .L0
56	.L1:
57		mov w17, w20
58		mov x8, x20
59		mov x7, x19
60		bl _arrStore4
61		adds w8, w20, #1
62		b.vs _errOverflow
63		sxtw x8, w8
64		// push {x8}
65		stp x8, xzr, [sp, #-16]!
66		// pop {x8}
67		ldp x8, xzr, [sp], #16
68		mov x8, x8
69		mov x20, x8
70	.L0:
71		cmp x20, #10
72		b.lt .L1
73		// Stack pointer unchanged, no stack allocated arguments
74		mov x8, x19
75		mov x0, x8
76		// statement primitives do not return results (but will clobber r0/rax)
77		bl _printp
78		// Stack pointer unchanged, no stack allocated arguments
79		adrp x8, .L.str0
80		add x8, x8, :lo12:.L.str0
81		// push {x8}
82		stp x8, xzr, [sp, #-16]!
83		// pop {x8}
84		ldp x8, xzr, [sp], #16
85		mov x8, x8
86		mov x0, x8
87		// statement primitives do not return results (but will clobber r0/rax)
88		bl _prints
89		mov x8, #0
90		mov x20, x8
91		b .L2
92	.L3:
93		// Stack pointer unchanged, no stack allocated arguments
94		mov w17, w20
95		mov x7, x19
96		bl _arrLoad4
97		mov w8, w7
98		mov x8, x8
99		mov x0, x8
100		// statement primitives do not return results (but will clobber r0/rax)
101		bl _printi
102		cmp x20, #9
103		b.lt .L4
104		b .L5
105	.L4:
106		// Stack pointer unchanged, no stack allocated arguments
107		adrp x8, .L.str1
108		add x8, x8, :lo12:.L.str1
109		// push {x8}
110		stp x8, xzr, [sp, #-16]!
111		// pop {x8}
112		ldp x8, xzr, [sp], #16
113		mov x8, x8
114		mov x0, x8
115		// statement primitives do not return results (but will clobber r0/rax)
116		bl _prints
117	.L5:
118		adds w8, w20, #1
119		b.vs _errOverflow
120		sxtw x8, w8
121		// push {x8}
122		stp x8, xzr, [sp, #-16]!
123		// pop {x8}
124		ldp x8, xzr, [sp], #16
125		mov x8, x8
126		mov x20, x8
127	.L2:
128		cmp x20, #10
129		b.lt .L3
130		// Stack pointer unchanged, no stack allocated arguments
131		adrp x8, .L.str2
132		add x8, x8, :lo12:.L.str2
133		// push {x8}
134		stp x8, xzr, [sp, #-16]!
135		// pop {x8}
136		ldp x8, xzr, [sp], #16
137		mov x8, x8
138		mov x0, x8
139		// statement primitives do not return results (but will clobber r0/rax)
140		bl _prints
141		bl _println
142		// Stack pointer unchanged, no stack allocated variables
143		mov x0, #0
144		// pop {x19, x20}
145		ldp x19, x20, [sp], #16
146		// pop {fp, lr}
147		ldp fp, lr, [sp], #16
148		ret
149	
150	// length of .L._prints_str0
151		.word 4
152	.L._prints_str0:
153		.asciz "%.*s"
154	.align 4
155	_prints:
156		// push {lr}
157		stp lr, xzr, [sp, #-16]!
158		mov x2, x0
159		ldrsw x1, [x0, #-4]
160		adr x0, .L._prints_str0
161		bl printf
162		mov x0, #0
163		bl fflush
164		// pop {lr}
165		ldp lr, xzr, [sp], #16
166		ret
167	
168	// length of .L._printp_str0
169		.word 2
170	.L._printp_str0:
171		.asciz "%p"
172	.align 4
173	_printp:
174		// push {lr}
175		stp lr, xzr, [sp, #-16]!
176		mov x1, x0
177		adr x0, .L._printp_str0
178		bl printf
179		mov x0, #0
180		bl fflush
181		// pop {lr}
182		ldp lr, xzr, [sp], #16
183		ret
184	
185	_malloc:
186		// push {lr}
187		stp lr, xzr, [sp, #-16]!
188		bl malloc
189		cbz x0, _errOutOfMemory
190		// pop {lr}
191		ldp lr, xzr, [sp], #16
192		ret
193	
194	// length of .L._printi_str0
195		.word 2
196	.L._printi_str0:
197		.asciz "%d"
198	.align 4
199	_printi:
200		// push {lr}
201		stp lr, xzr, [sp, #-16]!
202		mov x1, x0
203		adr x0, .L._printi_str0
204		bl printf
205		mov x0, #0
206		bl fflush
207		// pop {lr}
208		ldp lr, xzr, [sp], #16
209		ret
210	
211	// length of .L._println_str0
212		.word 0
213	.L._println_str0:
214		.asciz ""
215	.align 4
216	_println:
217		// push {lr}
218		stp lr, xzr, [sp, #-16]!
219		adr x0, .L._println_str0
220		bl puts
221		mov x0, #0
222		bl fflush
223		// pop {lr}
224		ldp lr, xzr, [sp], #16
225		ret
226	
227	_arrStore4:
228		// Special calling convention: array ptr passed in X7, index in X17, value to store in X8, LR (W30) is used as general register
229		// push {lr}
230		stp lr, xzr, [sp, #-16]!
231		sxtw x17, w17
232		cmp w17, #0
233		csel x1, x17, x1, lt
234		b.lt _errOutOfBounds
235		ldrsw lr, [x7, #-4]
236		cmp w17, w30
237		csel x1, x17, x1, ge
238		b.ge _errOutOfBounds
239		str w8, [x7, x17, lsl #2]
240		// pop {lr}
241		ldp lr, xzr, [sp], #16
242		ret
243	
244	_arrLoad4:
245		// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7
246		// push {lr}
247		stp lr, xzr, [sp, #-16]!
248		sxtw x17, w17
249		cmp w17, #0
250		csel x1, x17, x1, lt
251		b.lt _errOutOfBounds
252		ldrsw lr, [x7, #-4]
253		cmp w17, w30
254		csel x1, x17, x1, ge
255		b.ge _errOutOfBounds
256		ldrsw x7, [x7, x17, lsl #2]
257		// pop {lr}
258		ldp lr, xzr, [sp], #16
259		ret
260	
261	// length of .L._errOutOfMemory_str0
262		.word 27
263	.L._errOutOfMemory_str0:
264		.asciz "fatal error: out of memory\n"
265	.align 4
266	_errOutOfMemory:
267		adr x0, .L._errOutOfMemory_str0
268		bl _prints
269		mov w0, #-1
270		bl exit
271	
272	// length of .L._errOutOfBounds_str0
273		.word 42
274	.L._errOutOfBounds_str0:
275		.asciz "fatal error: array index %d out of bounds\n"
276	.align 4
277	_errOutOfBounds:
278		adr x0, .L._errOutOfBounds_str0
279		bl printf
280		mov x0, #0
281		bl fflush
282		mov w0, #-1
283		bl exit
284	
285	// length of .L._errOverflow_str0
286		.word 52
287	.L._errOverflow_str0:
288		.asciz "fatal error: integer overflow or underflow occurred\n"
289	.align 4
290	_errOverflow:
291		adr x0, .L._errOverflow_str0
292		bl _prints
293		mov w0, #-1
294		bl exit
===========================================================
-- Finished

