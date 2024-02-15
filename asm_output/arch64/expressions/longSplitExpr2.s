./valid/expressions/longSplitExpr2.wacc
calling the reference compiler on ./valid/expressions/longSplitExpr2.wacc
-- Test: longSplitExpr2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions with several variables

# Output:
# 362880
# 128
#

# Exit:
# 128

begin

  int x = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 ;
  int y = -1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9 - 10 - 11 - 12 - 13 - 14 - 15 - 16 - 17 ;
  int z = 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 ;
  int div = 10 ;
  println x + y + ( z / div ) ;
  println (x + y + ( z / div )) % 256 ;
  exit x + y + ( z / div )

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longSplitExpr2.s contents are:
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
12		mov w8, #1
13		adds w8, w8, #2
14		b.vs _errOverflow
15		sxtw x8, w8
16		// push {x8}
17		stp x8, xzr, [sp, #-16]!
18		// pop {x8}
19		ldp x8, xzr, [sp], #16
20		adds w8, w8, #3
21		b.vs _errOverflow
22		sxtw x8, w8
23		// push {x8}
24		stp x8, xzr, [sp, #-16]!
25		// pop {x8}
26		ldp x8, xzr, [sp], #16
27		adds w8, w8, #4
28		b.vs _errOverflow
29		sxtw x8, w8
30		// push {x8}
31		stp x8, xzr, [sp, #-16]!
32		// pop {x8}
33		ldp x8, xzr, [sp], #16
34		adds w8, w8, #5
35		b.vs _errOverflow
36		sxtw x8, w8
37		// push {x8}
38		stp x8, xzr, [sp, #-16]!
39		// pop {x8}
40		ldp x8, xzr, [sp], #16
41		adds w8, w8, #6
42		b.vs _errOverflow
43		sxtw x8, w8
44		// push {x8}
45		stp x8, xzr, [sp, #-16]!
46		// pop {x8}
47		ldp x8, xzr, [sp], #16
48		adds w8, w8, #7
49		b.vs _errOverflow
50		sxtw x8, w8
51		// push {x8}
52		stp x8, xzr, [sp, #-16]!
53		// pop {x8}
54		ldp x8, xzr, [sp], #16
55		adds w8, w8, #8
56		b.vs _errOverflow
57		sxtw x8, w8
58		// push {x8}
59		stp x8, xzr, [sp, #-16]!
60		// pop {x8}
61		ldp x8, xzr, [sp], #16
62		adds w8, w8, #9
63		b.vs _errOverflow
64		sxtw x8, w8
65		// push {x8}
66		stp x8, xzr, [sp, #-16]!
67		// pop {x8}
68		ldp x8, xzr, [sp], #16
69		adds w8, w8, #10
70		b.vs _errOverflow
71		sxtw x8, w8
72		// push {x8}
73		stp x8, xzr, [sp, #-16]!
74		// pop {x8}
75		ldp x8, xzr, [sp], #16
76		adds w8, w8, #11
77		b.vs _errOverflow
78		sxtw x8, w8
79		// push {x8}
80		stp x8, xzr, [sp, #-16]!
81		// pop {x8}
82		ldp x8, xzr, [sp], #16
83		adds w8, w8, #12
84		b.vs _errOverflow
85		sxtw x8, w8
86		// push {x8}
87		stp x8, xzr, [sp, #-16]!
88		// pop {x8}
89		ldp x8, xzr, [sp], #16
90		adds w8, w8, #13
91		b.vs _errOverflow
92		sxtw x8, w8
93		// push {x8}
94		stp x8, xzr, [sp, #-16]!
95		// pop {x8}
96		ldp x8, xzr, [sp], #16
97		adds w8, w8, #14
98		b.vs _errOverflow
99		sxtw x8, w8
100		// push {x8}
101		stp x8, xzr, [sp, #-16]!
102		// pop {x8}
103		ldp x8, xzr, [sp], #16
104		adds w8, w8, #15
105		b.vs _errOverflow
106		sxtw x8, w8
107		// push {x8}
108		stp x8, xzr, [sp, #-16]!
109		// pop {x8}
110		ldp x8, xzr, [sp], #16
111		adds w8, w8, #16
112		b.vs _errOverflow
113		sxtw x8, w8
114		// push {x8}
115		stp x8, xzr, [sp, #-16]!
116		// pop {x8}
117		ldp x8, xzr, [sp], #16
118		adds w8, w8, #17
119		b.vs _errOverflow
120		sxtw x8, w8
121		// push {x8}
122		stp x8, xzr, [sp, #-16]!
123		// pop {x8}
124		ldp x8, xzr, [sp], #16
125		mov x8, x8
126		mov x19, x8
127		mov w8, #-1
128		subs w8, w8, #2
129		b.vs _errOverflow
130		sxtw x8, w8
131		// push {x8}
132		stp x8, xzr, [sp, #-16]!
133		// pop {x8}
134		ldp x8, xzr, [sp], #16
135		subs w8, w8, #3
136		b.vs _errOverflow
137		sxtw x8, w8
138		// push {x8}
139		stp x8, xzr, [sp, #-16]!
140		// pop {x8}
141		ldp x8, xzr, [sp], #16
142		subs w8, w8, #4
143		b.vs _errOverflow
144		sxtw x8, w8
145		// push {x8}
146		stp x8, xzr, [sp, #-16]!
147		// pop {x8}
148		ldp x8, xzr, [sp], #16
149		subs w8, w8, #5
150		b.vs _errOverflow
151		sxtw x8, w8
152		// push {x8}
153		stp x8, xzr, [sp, #-16]!
154		// pop {x8}
155		ldp x8, xzr, [sp], #16
156		subs w8, w8, #6
157		b.vs _errOverflow
158		sxtw x8, w8
159		// push {x8}
160		stp x8, xzr, [sp, #-16]!
161		// pop {x8}
162		ldp x8, xzr, [sp], #16
163		subs w8, w8, #7
164		b.vs _errOverflow
165		sxtw x8, w8
166		// push {x8}
167		stp x8, xzr, [sp, #-16]!
168		// pop {x8}
169		ldp x8, xzr, [sp], #16
170		subs w8, w8, #8
171		b.vs _errOverflow
172		sxtw x8, w8
173		// push {x8}
174		stp x8, xzr, [sp, #-16]!
175		// pop {x8}
176		ldp x8, xzr, [sp], #16
177		subs w8, w8, #9
178		b.vs _errOverflow
179		sxtw x8, w8
180		// push {x8}
181		stp x8, xzr, [sp, #-16]!
182		// pop {x8}
183		ldp x8, xzr, [sp], #16
184		subs w8, w8, #10
185		b.vs _errOverflow
186		sxtw x8, w8
187		// push {x8}
188		stp x8, xzr, [sp, #-16]!
189		// pop {x8}
190		ldp x8, xzr, [sp], #16
191		subs w8, w8, #11
192		b.vs _errOverflow
193		sxtw x8, w8
194		// push {x8}
195		stp x8, xzr, [sp, #-16]!
196		// pop {x8}
197		ldp x8, xzr, [sp], #16
198		subs w8, w8, #12
199		b.vs _errOverflow
200		sxtw x8, w8
201		// push {x8}
202		stp x8, xzr, [sp, #-16]!
203		// pop {x8}
204		ldp x8, xzr, [sp], #16
205		subs w8, w8, #13
206		b.vs _errOverflow
207		sxtw x8, w8
208		// push {x8}
209		stp x8, xzr, [sp, #-16]!
210		// pop {x8}
211		ldp x8, xzr, [sp], #16
212		subs w8, w8, #14
213		b.vs _errOverflow
214		sxtw x8, w8
215		// push {x8}
216		stp x8, xzr, [sp, #-16]!
217		// pop {x8}
218		ldp x8, xzr, [sp], #16
219		subs w8, w8, #15
220		b.vs _errOverflow
221		sxtw x8, w8
222		// push {x8}
223		stp x8, xzr, [sp, #-16]!
224		// pop {x8}
225		ldp x8, xzr, [sp], #16
226		subs w8, w8, #16
227		b.vs _errOverflow
228		sxtw x8, w8
229		// push {x8}
230		stp x8, xzr, [sp, #-16]!
231		// pop {x8}
232		ldp x8, xzr, [sp], #16
233		subs w8, w8, #17
234		b.vs _errOverflow
235		sxtw x8, w8
236		// push {x8}
237		stp x8, xzr, [sp, #-16]!
238		// pop {x8}
239		ldp x8, xzr, [sp], #16
240		mov x8, x8
241		mov x20, x8
242		mov x9, #2
243		mov x8, #1
244		smull x8, w8, w9
245		// take the 31st bit, sign extend it to 64 bits
246		sbfx x9, x8, #31, #1
247		// now take the top 32 bits of the result, shift and sign extend to 64 bits
248		cmp x9, x8, asr #32
249		// if they are not equal then overflow occured
250		b.ne _errOverflow
251		sxtw x8, w8
252		// push {x8}
253		stp x8, xzr, [sp, #-16]!
254		mov x9, #3
255		// pop {x8}
256		ldp x8, xzr, [sp], #16
257		smull x8, w8, w9
258		// take the 31st bit, sign extend it to 64 bits
259		sbfx x9, x8, #31, #1
260		// now take the top 32 bits of the result, shift and sign extend to 64 bits
261		cmp x9, x8, asr #32
262		// if they are not equal then overflow occured
263		b.ne _errOverflow
264		sxtw x8, w8
265		// push {x8}
266		stp x8, xzr, [sp, #-16]!
267		mov x9, #4
268		// pop {x8}
269		ldp x8, xzr, [sp], #16
270		smull x8, w8, w9
271		// take the 31st bit, sign extend it to 64 bits
272		sbfx x9, x8, #31, #1
273		// now take the top 32 bits of the result, shift and sign extend to 64 bits
274		cmp x9, x8, asr #32
275		// if they are not equal then overflow occured
276		b.ne _errOverflow
277		sxtw x8, w8
278		// push {x8}
279		stp x8, xzr, [sp, #-16]!
280		mov x9, #5
281		// pop {x8}
282		ldp x8, xzr, [sp], #16
283		smull x8, w8, w9
284		// take the 31st bit, sign extend it to 64 bits
285		sbfx x9, x8, #31, #1
286		// now take the top 32 bits of the result, shift and sign extend to 64 bits
287		cmp x9, x8, asr #32
288		// if they are not equal then overflow occured
289		b.ne _errOverflow
290		sxtw x8, w8
291		// push {x8}
292		stp x8, xzr, [sp, #-16]!
293		mov x9, #6
294		// pop {x8}
295		ldp x8, xzr, [sp], #16
296		smull x8, w8, w9
297		// take the 31st bit, sign extend it to 64 bits
298		sbfx x9, x8, #31, #1
299		// now take the top 32 bits of the result, shift and sign extend to 64 bits
300		cmp x9, x8, asr #32
301		// if they are not equal then overflow occured
302		b.ne _errOverflow
303		sxtw x8, w8
304		// push {x8}
305		stp x8, xzr, [sp, #-16]!
306		mov x9, #7
307		// pop {x8}
308		ldp x8, xzr, [sp], #16
309		smull x8, w8, w9
310		// take the 31st bit, sign extend it to 64 bits
311		sbfx x9, x8, #31, #1
312		// now take the top 32 bits of the result, shift and sign extend to 64 bits
313		cmp x9, x8, asr #32
314		// if they are not equal then overflow occured
315		b.ne _errOverflow
316		sxtw x8, w8
317		// push {x8}
318		stp x8, xzr, [sp, #-16]!
319		mov x9, #8
320		// pop {x8}
321		ldp x8, xzr, [sp], #16
322		smull x8, w8, w9
323		// take the 31st bit, sign extend it to 64 bits
324		sbfx x9, x8, #31, #1
325		// now take the top 32 bits of the result, shift and sign extend to 64 bits
326		cmp x9, x8, asr #32
327		// if they are not equal then overflow occured
328		b.ne _errOverflow
329		sxtw x8, w8
330		// push {x8}
331		stp x8, xzr, [sp, #-16]!
332		mov x9, #9
333		// pop {x8}
334		ldp x8, xzr, [sp], #16
335		smull x8, w8, w9
336		// take the 31st bit, sign extend it to 64 bits
337		sbfx x9, x8, #31, #1
338		// now take the top 32 bits of the result, shift and sign extend to 64 bits
339		cmp x9, x8, asr #32
340		// if they are not equal then overflow occured
341		b.ne _errOverflow
342		sxtw x8, w8
343		// push {x8}
344		stp x8, xzr, [sp, #-16]!
345		mov x9, #10
346		// pop {x8}
347		ldp x8, xzr, [sp], #16
348		smull x8, w8, w9
349		// take the 31st bit, sign extend it to 64 bits
350		sbfx x9, x8, #31, #1
351		// now take the top 32 bits of the result, shift and sign extend to 64 bits
352		cmp x9, x8, asr #32
353		// if they are not equal then overflow occured
354		b.ne _errOverflow
355		sxtw x8, w8
356		// push {x8}
357		stp x8, xzr, [sp, #-16]!
358		// pop {x8}
359		ldp x8, xzr, [sp], #16
360		mov x8, x8
361		mov x21, x8
362		mov x8, #10
363		mov x22, x8
364		// Stack pointer unchanged, no stack allocated arguments
365		adds w8, w19, w20
366		b.vs _errOverflow
367		sxtw x8, w8
368		// push {x8}
369		stp x8, xzr, [sp, #-16]!
370		cbz x22, _errDivZero
371		sdiv w8, w21, w22
372		sxtw x8, w8
373		// push {x8}
374		stp x8, xzr, [sp, #-16]!
375		// pop {x9}
376		ldp x9, xzr, [sp], #16
377		// pop {x8}
378		ldp x8, xzr, [sp], #16
379		adds w8, w8, w9
380		b.vs _errOverflow
381		sxtw x8, w8
382		// push {x8}
383		stp x8, xzr, [sp, #-16]!
384		// pop {x8}
385		ldp x8, xzr, [sp], #16
386		mov x8, x8
387		mov x0, x8
388		// statement primitives do not return results (but will clobber r0/rax)
389		bl _printi
390		bl _println
391		// Stack pointer unchanged, no stack allocated arguments
392		adds w8, w19, w20
393		b.vs _errOverflow
394		sxtw x8, w8
395		// push {x8}
396		stp x8, xzr, [sp, #-16]!
397		cbz x22, _errDivZero
398		sdiv w8, w21, w22
399		sxtw x8, w8
400		// push {x8}
401		stp x8, xzr, [sp, #-16]!
402		// pop {x9}
403		ldp x9, xzr, [sp], #16
404		// pop {x8}
405		ldp x8, xzr, [sp], #16
406		adds w8, w8, w9
407		b.vs _errOverflow
408		sxtw x8, w8
409		// push {x8}
410		stp x8, xzr, [sp, #-16]!
411		mov w9, #256
412		// pop {x8}
413		ldp x8, xzr, [sp], #16
414		cbz x9, _errDivZero
415		sdiv w17, w8, w9
416		msub w8, w17, w9, w8
417		sxtw x8, w8
418		// push {x8}
419		stp x8, xzr, [sp, #-16]!
420		// pop {x8}
421		ldp x8, xzr, [sp], #16
422		mov x8, x8
423		mov x0, x8
424		// statement primitives do not return results (but will clobber r0/rax)
425		bl _printi
426		bl _println
427		// Stack pointer unchanged, no stack allocated arguments
428		adds w8, w19, w20
429		b.vs _errOverflow
430		sxtw x8, w8
431		// push {x8}
432		stp x8, xzr, [sp, #-16]!
433		cbz x22, _errDivZero
434		sdiv w8, w21, w22
435		sxtw x8, w8
436		// push {x8}
437		stp x8, xzr, [sp, #-16]!
438		// pop {x9}
439		ldp x9, xzr, [sp], #16
440		// pop {x8}
441		ldp x8, xzr, [sp], #16
442		adds w8, w8, w9
443		b.vs _errOverflow
444		sxtw x8, w8
445		// push {x8}
446		stp x8, xzr, [sp, #-16]!
447		// pop {x8}
448		ldp x8, xzr, [sp], #16
449		mov x8, x8
450		mov x0, x8
451		// statement primitives do not return results (but will clobber r0/rax)
452		bl exit
453		// Stack pointer unchanged, no stack allocated variables
454		mov x0, #0
455		// pop {x19, x20, x21, x22}
456		ldp x21, x22, [sp, #16]
457		ldp x19, x20, [sp], #32
458		// pop {fp, lr}
459		ldp fp, lr, [sp], #16
460		ret
461	
462	// length of .L._prints_str0
463		.word 4
464	.L._prints_str0:
465		.asciz "%.*s"
466	.align 4
467	_prints:
468		// push {lr}
469		stp lr, xzr, [sp, #-16]!
470		mov x2, x0
471		ldrsw x1, [x0, #-4]
472		adr x0, .L._prints_str0
473		bl printf
474		mov x0, #0
475		bl fflush
476		// pop {lr}
477		ldp lr, xzr, [sp], #16
478		ret
479	
480	// length of .L._printi_str0
481		.word 2
482	.L._printi_str0:
483		.asciz "%d"
484	.align 4
485	_printi:
486		// push {lr}
487		stp lr, xzr, [sp, #-16]!
488		mov x1, x0
489		adr x0, .L._printi_str0
490		bl printf
491		mov x0, #0
492		bl fflush
493		// pop {lr}
494		ldp lr, xzr, [sp], #16
495		ret
496	
497	// length of .L._println_str0
498		.word 0
499	.L._println_str0:
500		.asciz ""
501	.align 4
502	_println:
503		// push {lr}
504		stp lr, xzr, [sp, #-16]!
505		adr x0, .L._println_str0
506		bl puts
507		mov x0, #0
508		bl fflush
509		// pop {lr}
510		ldp lr, xzr, [sp], #16
511		ret
512	
513	// length of .L._errDivZero_str0
514		.word 40
515	.L._errDivZero_str0:
516		.asciz "fatal error: division or modulo by zero\n"
517	.align 4
518	_errDivZero:
519		adr x0, .L._errDivZero_str0
520		bl _prints
521		mov w0, #-1
522		bl exit
523	
524	// length of .L._errOverflow_str0
525		.word 52
526	.L._errOverflow_str0:
527		.asciz "fatal error: integer overflow or underflow occurred\n"
528	.align 4
529	_errOverflow:
530		adr x0, .L._errOverflow_str0
531		bl _prints
532		mov w0, #-1
533		bl exit
===========================================================
-- Finished

