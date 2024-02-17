./valid/function/simple_functions/functionManyArguments.wacc
calling the reference compiler on ./valid/function/simple_functions/functionManyArguments.wacc
-- Test: functionManyArguments.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a function with varied inputs

# Output:
# a is 42
# b is true
# c is u
# d is hello
# e is #addrs#
# f is #addrs#
# answer is g
#

# Program:

begin
  char doSomething(int a, bool b, char c, string d, bool[] e, int[] f) is
    print "a is " ;
    println a ;
    print "b is " ;
    println b ;
    print "c is " ;
    println c ;
    print "d is " ;
    println d ;
    print "e is " ;
    println e ;
    print "f is " ;
    println f ;
    return 'g'
  end
  bool[] bools = [ false, true ] ;
  int[] ints = [ 1, 2 ] ;
  char answer = call doSomething(42, true, 'u', "hello", bools, ints) ;
  print "answer is " ;
  println answer
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionManyArguments.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 5
3	.L.str0:
4		.asciz "a is "
5	// length of .L.str1
6		.word 5
7	.L.str1:
8		.asciz "b is "
9	// length of .L.str2
10		.word 5
11	.L.str2:
12		.asciz "c is "
13	// length of .L.str3
14		.word 5
15	.L.str3:
16		.asciz "d is "
17	// length of .L.str4
18		.word 5
19	.L.str4:
20		.asciz "e is "
21	// length of .L.str5
22		.word 5
23	.L.str5:
24		.asciz "f is "
25	// length of .L.str6
26		.word 5
27	.L.str6:
28		.asciz "hello"
29	// length of .L.str7
30		.word 10
31	.L.str7:
32		.asciz "answer is "
33	.align 4
34	.text
35	.global main
36	main:
37		// push {fp, lr}
38		stp fp, lr, [sp, #-16]!
39		// push {x19, x20, x21}
40		stp x19, x20, [sp, #-32]!
41		stp x21, xzr, [sp, #16]
42		mov fp, sp
43		// Stack pointer unchanged, no stack allocated variables
44		// 2 element array
45		mov w0, #6
46		bl _malloc
47		mov x16, x0
48		// array pointers are shifted forwards by 4 bytes (to account for size)
49		add x16, x16, #4
50		mov x8, #2
51		str w8, [x16, #-4]
52		mov x8, #0
53		strb w8, [x16, #0]
54		mov x8, #1
55		strb w8, [x16, #1]
56		mov x8, x16
57		mov x19, x8
58		// 2 element array
59		mov w0, #12
60		bl _malloc
61		mov x16, x0
62		// array pointers are shifted forwards by 4 bytes (to account for size)
63		add x16, x16, #4
64		mov x8, #2
65		str w8, [x16, #-4]
66		mov x8, #1
67		str w8, [x16, #0]
68		mov x8, #2
69		str w8, [x16, #4]
70		mov x8, x16
71		mov x20, x8
72		// Stack pointer unchanged, no stack allocated arguments
73		mov x8, #42
74		mov x0, x8
75		mov x8, #1
76		mov x1, x8
77		mov x8, #117
78		mov x2, x8
79		adrp x8, .L.str6
80		add x8, x8, :lo12:.L.str6
81		// push {x8}
82		stp x8, xzr, [sp, #-16]!
83		// pop {x8}
84		ldp x8, xzr, [sp], #16
85		mov x8, x8
86		mov x3, x8
87		mov x8, x19
88		mov x4, x8
89		mov x8, x20
90		mov x5, x8
91		bl wacc_doSomething
92		mov x16, x0
93		// Stack pointer unchanged, no stack allocated arguments
94		mov x8, x16
95		mov x21, x8
96		// Stack pointer unchanged, no stack allocated arguments
97		adrp x8, .L.str7
98		add x8, x8, :lo12:.L.str7
99		// push {x8}
100		stp x8, xzr, [sp, #-16]!
101		// pop {x8}
102		ldp x8, xzr, [sp], #16
103		mov x8, x8
104		mov x0, x8
105		// statement primitives do not return results (but will clobber r0/rax)
106		bl _prints
107		// Stack pointer unchanged, no stack allocated arguments
108		mov x8, x21
109		mov x0, x8
110		// statement primitives do not return results (but will clobber r0/rax)
111		bl _printc
112		bl _println
113		// Stack pointer unchanged, no stack allocated variables
114		mov x0, #0
115		// pop {x19, x20, x21}
116		ldp x21, xzr, [sp, #16]
117		ldp x19, x20, [sp], #32
118		// pop {fp, lr}
119		ldp fp, lr, [sp], #16
120		ret
121	
122	wacc_doSomething:
123		// push {fp, lr}
124		stp fp, lr, [sp, #-16]!
125		mov fp, sp
126		// push {x0, x1, x2, x3, x4, x5}
127		stp x0, x1, [sp, #-48]!
128		stp x2, x3, [sp, #16]
129		stp x4, x5, [sp, #32]
130		// Set up X16 as a temporary second base pointer for the caller saved things
131		mov x16, sp
132		// Stack pointer unchanged, no stack allocated arguments
133		adrp x8, .L.str0
134		add x8, x8, :lo12:.L.str0
135		// push {x8}
136		stp x8, xzr, [sp, #-16]!
137		// pop {x8}
138		ldp x8, xzr, [sp], #16
139		mov x8, x8
140		mov x0, x8
141		// statement primitives do not return results (but will clobber r0/rax)
142		bl _prints
143		// pop {x0, x1, x2, x3, x4, x5}
144		ldp x2, x3, [sp, #16]
145		ldp x4, x5, [sp, #32]
146		ldp x0, x1, [sp], #48
147		// push {x0, x1, x2, x3, x4, x5}
148		stp x0, x1, [sp, #-48]!
149		stp x2, x3, [sp, #16]
150		stp x4, x5, [sp, #32]
151		// Set up X16 as a temporary second base pointer for the caller saved things
152		mov x16, sp
153		// Stack pointer unchanged, no stack allocated arguments
154		mov x8, x0
155		mov x0, x8
156		// statement primitives do not return results (but will clobber r0/rax)
157		bl _printi
158		bl _println
159		// pop {x0, x1, x2, x3, x4, x5}
160		ldp x2, x3, [sp, #16]
161		ldp x4, x5, [sp, #32]
162		ldp x0, x1, [sp], #48
163		// push {x0, x1, x2, x3, x4, x5}
164		stp x0, x1, [sp, #-48]!
165		stp x2, x3, [sp, #16]
166		stp x4, x5, [sp, #32]
167		// Set up X16 as a temporary second base pointer for the caller saved things
168		mov x16, sp
169		// Stack pointer unchanged, no stack allocated arguments
170		adrp x8, .L.str1
171		add x8, x8, :lo12:.L.str1
172		// push {x8}
173		stp x8, xzr, [sp, #-16]!
174		// pop {x8}
175		ldp x8, xzr, [sp], #16
176		mov x8, x8
177		mov x0, x8
178		// statement primitives do not return results (but will clobber r0/rax)
179		bl _prints
180		// pop {x0, x1, x2, x3, x4, x5}
181		ldp x2, x3, [sp, #16]
182		ldp x4, x5, [sp, #32]
183		ldp x0, x1, [sp], #48
184		// push {x0, x1, x2, x3, x4, x5}
185		stp x0, x1, [sp, #-48]!
186		stp x2, x3, [sp, #16]
187		stp x4, x5, [sp, #32]
188		// Set up X16 as a temporary second base pointer for the caller saved things
189		mov x16, sp
190		// Stack pointer unchanged, no stack allocated arguments
191		mov x8, x1
192		mov x0, x8
193		// statement primitives do not return results (but will clobber r0/rax)
194		bl _printb
195		bl _println
196		// pop {x0, x1, x2, x3, x4, x5}
197		ldp x2, x3, [sp, #16]
198		ldp x4, x5, [sp, #32]
199		ldp x0, x1, [sp], #48
200		// push {x0, x1, x2, x3, x4, x5}
201		stp x0, x1, [sp, #-48]!
202		stp x2, x3, [sp, #16]
203		stp x4, x5, [sp, #32]
204		// Set up X16 as a temporary second base pointer for the caller saved things
205		mov x16, sp
206		// Stack pointer unchanged, no stack allocated arguments
207		adrp x8, .L.str2
208		add x8, x8, :lo12:.L.str2
209		// push {x8}
210		stp x8, xzr, [sp, #-16]!
211		// pop {x8}
212		ldp x8, xzr, [sp], #16
213		mov x8, x8
214		mov x0, x8
215		// statement primitives do not return results (but will clobber r0/rax)
216		bl _prints
217		// pop {x0, x1, x2, x3, x4, x5}
218		ldp x2, x3, [sp, #16]
219		ldp x4, x5, [sp, #32]
220		ldp x0, x1, [sp], #48
221		// push {x0, x1, x2, x3, x4, x5}
222		stp x0, x1, [sp, #-48]!
223		stp x2, x3, [sp, #16]
224		stp x4, x5, [sp, #32]
225		// Set up X16 as a temporary second base pointer for the caller saved things
226		mov x16, sp
227		// Stack pointer unchanged, no stack allocated arguments
228		mov x8, x2
229		mov x0, x8
230		// statement primitives do not return results (but will clobber r0/rax)
231		bl _printc
232		bl _println
233		// pop {x0, x1, x2, x3, x4, x5}
234		ldp x2, x3, [sp, #16]
235		ldp x4, x5, [sp, #32]
236		ldp x0, x1, [sp], #48
237		// push {x0, x1, x2, x3, x4, x5}
238		stp x0, x1, [sp, #-48]!
239		stp x2, x3, [sp, #16]
240		stp x4, x5, [sp, #32]
241		// Set up X16 as a temporary second base pointer for the caller saved things
242		mov x16, sp
243		// Stack pointer unchanged, no stack allocated arguments
244		adrp x8, .L.str3
245		add x8, x8, :lo12:.L.str3
246		// push {x8}
247		stp x8, xzr, [sp, #-16]!
248		// pop {x8}
249		ldp x8, xzr, [sp], #16
250		mov x8, x8
251		mov x0, x8
252		// statement primitives do not return results (but will clobber r0/rax)
253		bl _prints
254		// pop {x0, x1, x2, x3, x4, x5}
255		ldp x2, x3, [sp, #16]
256		ldp x4, x5, [sp, #32]
257		ldp x0, x1, [sp], #48
258		// push {x0, x1, x2, x3, x4, x5}
259		stp x0, x1, [sp, #-48]!
260		stp x2, x3, [sp, #16]
261		stp x4, x5, [sp, #32]
262		// Set up X16 as a temporary second base pointer for the caller saved things
263		mov x16, sp
264		// Stack pointer unchanged, no stack allocated arguments
265		mov x8, x3
266		mov x0, x8
267		// statement primitives do not return results (but will clobber r0/rax)
268		bl _prints
269		bl _println
270		// pop {x0, x1, x2, x3, x4, x5}
271		ldp x2, x3, [sp, #16]
272		ldp x4, x5, [sp, #32]
273		ldp x0, x1, [sp], #48
274		// push {x0, x1, x2, x3, x4, x5}
275		stp x0, x1, [sp, #-48]!
276		stp x2, x3, [sp, #16]
277		stp x4, x5, [sp, #32]
278		// Set up X16 as a temporary second base pointer for the caller saved things
279		mov x16, sp
280		// Stack pointer unchanged, no stack allocated arguments
281		adrp x8, .L.str4
282		add x8, x8, :lo12:.L.str4
283		// push {x8}
284		stp x8, xzr, [sp, #-16]!
285		// pop {x8}
286		ldp x8, xzr, [sp], #16
287		mov x8, x8
288		mov x0, x8
289		// statement primitives do not return results (but will clobber r0/rax)
290		bl _prints
291		// pop {x0, x1, x2, x3, x4, x5}
292		ldp x2, x3, [sp, #16]
293		ldp x4, x5, [sp, #32]
294		ldp x0, x1, [sp], #48
295		// push {x0, x1, x2, x3, x4, x5}
296		stp x0, x1, [sp, #-48]!
297		stp x2, x3, [sp, #16]
298		stp x4, x5, [sp, #32]
299		// Set up X16 as a temporary second base pointer for the caller saved things
300		mov x16, sp
301		// Stack pointer unchanged, no stack allocated arguments
302		mov x8, x4
303		mov x0, x8
304		// statement primitives do not return results (but will clobber r0/rax)
305		bl _printp
306		bl _println
307		// pop {x0, x1, x2, x3, x4, x5}
308		ldp x2, x3, [sp, #16]
309		ldp x4, x5, [sp, #32]
310		ldp x0, x1, [sp], #48
311		// push {x0, x1, x2, x3, x4, x5}
312		stp x0, x1, [sp, #-48]!
313		stp x2, x3, [sp, #16]
314		stp x4, x5, [sp, #32]
315		// Set up X16 as a temporary second base pointer for the caller saved things
316		mov x16, sp
317		// Stack pointer unchanged, no stack allocated arguments
318		adrp x8, .L.str5
319		add x8, x8, :lo12:.L.str5
320		// push {x8}
321		stp x8, xzr, [sp, #-16]!
322		// pop {x8}
323		ldp x8, xzr, [sp], #16
324		mov x8, x8
325		mov x0, x8
326		// statement primitives do not return results (but will clobber r0/rax)
327		bl _prints
328		// pop {x0, x1, x2, x3, x4, x5}
329		ldp x2, x3, [sp, #16]
330		ldp x4, x5, [sp, #32]
331		ldp x0, x1, [sp], #48
332		// push {x0, x1, x2, x3, x4, x5}
333		stp x0, x1, [sp, #-48]!
334		stp x2, x3, [sp, #16]
335		stp x4, x5, [sp, #32]
336		// Set up X16 as a temporary second base pointer for the caller saved things
337		mov x16, sp
338		// Stack pointer unchanged, no stack allocated arguments
339		mov x8, x5
340		mov x0, x8
341		// statement primitives do not return results (but will clobber r0/rax)
342		bl _printp
343		bl _println
344		// pop {x0, x1, x2, x3, x4, x5}
345		ldp x2, x3, [sp, #16]
346		ldp x4, x5, [sp, #32]
347		ldp x0, x1, [sp], #48
348		mov x0, #103
349		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
350		mov sp, fp
351		// pop {fp, lr}
352		ldp fp, lr, [sp], #16
353		ret
354		// 'ere be dragons: this is 100% dead code, functions always end in returns!
355	
356	// length of .L._prints_str0
357		.word 4
358	.L._prints_str0:
359		.asciz "%.*s"
360	.align 4
361	_prints:
362		// push {lr}
363		stp lr, xzr, [sp, #-16]!
364		mov x2, x0
365		ldrsw x1, [x0, #-4]
366		adr x0, .L._prints_str0
367		bl printf
368		mov x0, #0
369		bl fflush
370		// pop {lr}
371		ldp lr, xzr, [sp], #16
372		ret
373	
374	// length of .L._printp_str0
375		.word 2
376	.L._printp_str0:
377		.asciz "%p"
378	.align 4
379	_printp:
380		// push {lr}
381		stp lr, xzr, [sp, #-16]!
382		mov x1, x0
383		adr x0, .L._printp_str0
384		bl printf
385		mov x0, #0
386		bl fflush
387		// pop {lr}
388		ldp lr, xzr, [sp], #16
389		ret
390	
391	_malloc:
392		// push {lr}
393		stp lr, xzr, [sp, #-16]!
394		bl malloc
395		cbz x0, _errOutOfMemory
396		// pop {lr}
397		ldp lr, xzr, [sp], #16
398		ret
399	
400	// length of .L._printi_str0
401		.word 2
402	.L._printi_str0:
403		.asciz "%d"
404	.align 4
405	_printi:
406		// push {lr}
407		stp lr, xzr, [sp, #-16]!
408		mov x1, x0
409		adr x0, .L._printi_str0
410		bl printf
411		mov x0, #0
412		bl fflush
413		// pop {lr}
414		ldp lr, xzr, [sp], #16
415		ret
416	
417	// length of .L._printb_str0
418		.word 5
419	.L._printb_str0:
420		.asciz "false"
421	// length of .L._printb_str1
422		.word 4
423	.L._printb_str1:
424		.asciz "true"
425	// length of .L._printb_str2
426		.word 4
427	.L._printb_str2:
428		.asciz "%.*s"
429	.align 4
430	_printb:
431		// push {lr}
432		stp lr, xzr, [sp, #-16]!
433		cmp w0, #0
434		b.ne .L_printb0
435		adr x2, .L._printb_str0
436		b .L_printb1
437	.L_printb0:
438		adr x2, .L._printb_str1
439	.L_printb1:
440		ldrsw x1, [x2, #-4]
441		adr x0, .L._printb_str2
442		bl printf
443		mov x0, #0
444		bl fflush
445		// pop {lr}
446		ldp lr, xzr, [sp], #16
447		ret
448	
449	// length of .L._printc_str0
450		.word 2
451	.L._printc_str0:
452		.asciz "%c"
453	.align 4
454	_printc:
455		// push {lr}
456		stp lr, xzr, [sp, #-16]!
457		mov x1, x0
458		adr x0, .L._printc_str0
459		bl printf
460		mov x0, #0
461		bl fflush
462		// pop {lr}
463		ldp lr, xzr, [sp], #16
464		ret
465	
466	// length of .L._println_str0
467		.word 0
468	.L._println_str0:
469		.asciz ""
470	.align 4
471	_println:
472		// push {lr}
473		stp lr, xzr, [sp, #-16]!
474		adr x0, .L._println_str0
475		bl puts
476		mov x0, #0
477		bl fflush
478		// pop {lr}
479		ldp lr, xzr, [sp], #16
480		ret
481	
482	// length of .L._errOutOfMemory_str0
483		.word 27
484	.L._errOutOfMemory_str0:
485		.asciz "fatal error: out of memory\n"
486	.align 4
487	_errOutOfMemory:
488		adr x0, .L._errOutOfMemory_str0
489		bl _prints
490		mov w0, #-1
491		bl exit
===========================================================
-- Finished

