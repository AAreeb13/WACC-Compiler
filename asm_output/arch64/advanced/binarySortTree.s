./valid/advanced/binarySortTree.wacc
calling the reference compiler on ./valid/advanced/binarySortTree.wacc
-- Test: binarySortTree.wacc

-- Uploaded file: 
---------------------------------------------------------------
# The program reads n (number of integers), then n integers. After each input, 
# it insert the integer into a binary search tree. At the end, it prints out 
# the content in the binary search tree so that we have a sorted list of 
# integer.
# 
# We represent a node in the binary search tree using two pair elements. The 
# first element has a type <int, pair>, the int is the integer stored in the 
# node, the pair is the pointer to the second pair element. The second pair 
# element has a type <pair, pair> which is the pointer to the two children 
# nodes in the binary search tree.

begin

  # Create a new node of a binary search tree having the given integer value 
  # and points to the two given pairs.
  pair(int, pair) createNewNode(int value, pair(int, pair) left, pair(int, pair) right) is
    pair(pair, pair) p = newpair(left, right) ;
    pair(int, pair) q = newpair(value, p) ;
    return q
  end

  # Given a root of a binary search tree and an integer to insert, the function 
  # inserts the integer into the tree and returns the new root of the tree.
  pair(int, pair) insert(pair(int, pair) root, int n) is
    if root == null then
      root = call createNewNode(n, null, null)
    else
      pair(pair, pair) p = snd root ;
      int current = fst root ;
      pair(int, pair) q = null ;
      if n < current then
      	q = fst p ;
        fst p = call insert(q, n)
      else 
      	q = snd p ;
        snd p = call insert(q, n)
      fi 
    fi ;
    return root
  end

  # Print the integers in the binary search tree in the increasing order.
  int printTree(pair(int, pair) root) is
    if root == null then
      return 0 
    else
      pair(pair, pair) body = snd root ;
      pair(int, pair) p = fst body ;
      int temp = call printTree(p) ;
      temp = fst root ; 
      print temp ;
      print ' ' ;
      p = snd body ;
      temp = call printTree(p) ;
      return 0
    fi
  end

  # The main function
  int n = 0 ;
  print "Please enter the number of integers to insert: " ;
  read n ;
  print "There are " ;
  print n ;
  println " integers." ;
  int i = 0 ;
  pair(int, pair) root = null ;
  while i < n do
    int x = 0 ;
    print "Please enter the number at position " ; 
    print i + 1 ;
    print " : " ;
    read x ;
    root = call insert(root, x) ;
    i = i + 1
  done ;
  print "Here are the numbers sorted: " ;
  i = call printTree(root) ;
  println ""
end



---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
binarySortTree.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 47
3	.L.str0:
4		.asciz "Please enter the number of integers to insert: "
5	// length of .L.str1
6		.word 10
7	.L.str1:
8		.asciz "There are "
9	// length of .L.str2
10		.word 10
11	.L.str2:
12		.asciz " integers."
13	// length of .L.str3
14		.word 36
15	.L.str3:
16		.asciz "Please enter the number at position "
17	// length of .L.str4
18		.word 3
19	.L.str4:
20		.asciz " : "
21	// length of .L.str5
22		.word 29
23	.L.str5:
24		.asciz "Here are the numbers sorted: "
25	// length of .L.str6
26		.word 0
27	.L.str6:
28		.asciz ""
29	.align 4
30	.text
31	.global main
32	main:
33		// push {fp, lr}
34		stp fp, lr, [sp, #-16]!
35		// push {x19, x20, x21, x22}
36		stp x19, x20, [sp, #-32]!
37		stp x21, x22, [sp, #16]
38		mov fp, sp
39		// Stack pointer unchanged, no stack allocated variables
40		mov x8, #0
41		mov x19, x8
42		// Stack pointer unchanged, no stack allocated arguments
43		adrp x8, .L.str0
44		add x8, x8, :lo12:.L.str0
45		// push {x8}
46		stp x8, xzr, [sp, #-16]!
47		// pop {x8}
48		ldp x8, xzr, [sp], #16
49		mov x8, x8
50		mov x0, x8
51		// statement primitives do not return results (but will clobber r0/rax)
52		bl _prints
53		// Stack pointer unchanged, no stack allocated arguments
54		// load the current value in the destination of the read so it supports defaults
55		mov x8, x19
56		mov x0, x8
57		bl _readi
58		mov x16, x0
59		mov x8, x16
60		mov x19, x8
61		// Stack pointer unchanged, no stack allocated arguments
62		adrp x8, .L.str1
63		add x8, x8, :lo12:.L.str1
64		// push {x8}
65		stp x8, xzr, [sp, #-16]!
66		// pop {x8}
67		ldp x8, xzr, [sp], #16
68		mov x8, x8
69		mov x0, x8
70		// statement primitives do not return results (but will clobber r0/rax)
71		bl _prints
72		// Stack pointer unchanged, no stack allocated arguments
73		mov x8, x19
74		mov x0, x8
75		// statement primitives do not return results (but will clobber r0/rax)
76		bl _printi
77		// Stack pointer unchanged, no stack allocated arguments
78		adrp x8, .L.str2
79		add x8, x8, :lo12:.L.str2
80		// push {x8}
81		stp x8, xzr, [sp, #-16]!
82		// pop {x8}
83		ldp x8, xzr, [sp], #16
84		mov x8, x8
85		mov x0, x8
86		// statement primitives do not return results (but will clobber r0/rax)
87		bl _prints
88		bl _println
89		mov x8, #0
90		mov x20, x8
91		mov x8, #0
92		mov x21, x8
93		b .L6
94	.L7:
95		// Stack pointer unchanged, no stack allocated variables
96		mov x8, #0
97		mov x22, x8
98		// Stack pointer unchanged, no stack allocated arguments
99		adrp x8, .L.str3
100		add x8, x8, :lo12:.L.str3
101		// push {x8}
102		stp x8, xzr, [sp, #-16]!
103		// pop {x8}
104		ldp x8, xzr, [sp], #16
105		mov x8, x8
106		mov x0, x8
107		// statement primitives do not return results (but will clobber r0/rax)
108		bl _prints
109		// Stack pointer unchanged, no stack allocated arguments
110		adds w8, w20, #1
111		b.vs _errOverflow
112		sxtw x8, w8
113		// push {x8}
114		stp x8, xzr, [sp, #-16]!
115		// pop {x8}
116		ldp x8, xzr, [sp], #16
117		mov x8, x8
118		mov x0, x8
119		// statement primitives do not return results (but will clobber r0/rax)
120		bl _printi
121		// Stack pointer unchanged, no stack allocated arguments
122		adrp x8, .L.str4
123		add x8, x8, :lo12:.L.str4
124		// push {x8}
125		stp x8, xzr, [sp, #-16]!
126		// pop {x8}
127		ldp x8, xzr, [sp], #16
128		mov x8, x8
129		mov x0, x8
130		// statement primitives do not return results (but will clobber r0/rax)
131		bl _prints
132		// Stack pointer unchanged, no stack allocated arguments
133		// load the current value in the destination of the read so it supports defaults
134		mov x8, x22
135		mov x0, x8
136		bl _readi
137		mov x16, x0
138		mov x8, x16
139		mov x22, x8
140		// Stack pointer unchanged, no stack allocated arguments
141		mov x8, x21
142		mov x0, x8
143		mov x8, x22
144		mov x1, x8
145		bl wacc_insert
146		mov x16, x0
147		// Stack pointer unchanged, no stack allocated arguments
148		mov x8, x16
149		mov x21, x8
150		adds w8, w20, #1
151		b.vs _errOverflow
152		sxtw x8, w8
153		// push {x8}
154		stp x8, xzr, [sp, #-16]!
155		// pop {x8}
156		ldp x8, xzr, [sp], #16
157		mov x8, x8
158		mov x20, x8
159		// Stack pointer unchanged, no stack allocated variables
160	.L6:
161		cmp x20, x19
162		b.lt .L7
163		// Stack pointer unchanged, no stack allocated arguments
164		adrp x8, .L.str5
165		add x8, x8, :lo12:.L.str5
166		// push {x8}
167		stp x8, xzr, [sp, #-16]!
168		// pop {x8}
169		ldp x8, xzr, [sp], #16
170		mov x8, x8
171		mov x0, x8
172		// statement primitives do not return results (but will clobber r0/rax)
173		bl _prints
174		// Stack pointer unchanged, no stack allocated arguments
175		mov x8, x21
176		mov x0, x8
177		bl wacc_printTree
178		mov x16, x0
179		// Stack pointer unchanged, no stack allocated arguments
180		mov x8, x16
181		mov x20, x8
182		// Stack pointer unchanged, no stack allocated arguments
183		adrp x8, .L.str6
184		add x8, x8, :lo12:.L.str6
185		// push {x8}
186		stp x8, xzr, [sp, #-16]!
187		// pop {x8}
188		ldp x8, xzr, [sp], #16
189		mov x8, x8
190		mov x0, x8
191		// statement primitives do not return results (but will clobber r0/rax)
192		bl _prints
193		bl _println
194		// Stack pointer unchanged, no stack allocated variables
195		mov x0, #0
196		// pop {x19, x20, x21, x22}
197		ldp x21, x22, [sp, #16]
198		ldp x19, x20, [sp], #32
199		// pop {fp, lr}
200		ldp fp, lr, [sp], #16
201		ret
202	
203	wacc_createNewNode:
204		// push {fp, lr}
205		stp fp, lr, [sp, #-16]!
206		// push {x19, x20}
207		stp x19, x20, [sp, #-16]!
208		mov fp, sp
209		// Stack pointer unchanged, no stack allocated variables
210		// push {x0, x1, x2}
211		stp x0, x1, [sp, #-32]!
212		stp x2, xzr, [sp, #16]
213		mov w0, #16
214		bl _malloc
215		mov x16, x0
216		// pop {x0, x1, x2}
217		ldp x2, xzr, [sp, #16]
218		ldp x0, x1, [sp], #32
219		mov x8, x1
220		str x8, [x16, #0]
221		mov x8, x2
222		str x8, [x16, #8]
223		mov x8, x16
224		mov x19, x8
225		// push {x0, x1, x2}
226		stp x0, x1, [sp, #-32]!
227		stp x2, xzr, [sp, #16]
228		mov w0, #16
229		bl _malloc
230		mov x16, x0
231		// pop {x0, x1, x2}
232		ldp x2, xzr, [sp, #16]
233		ldp x0, x1, [sp], #32
234		mov x8, x0
235		str x8, [x16, #0]
236		mov x8, x19
237		str x8, [x16, #8]
238		mov x8, x16
239		mov x20, x8
240		mov x0, x20
241		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
242		mov sp, fp
243		// pop {x19, x20}
244		ldp x19, x20, [sp], #16
245		// pop {fp, lr}
246		ldp fp, lr, [sp], #16
247		ret
248		// Stack pointer unchanged, no stack allocated variables
249		// 'ere be dragons: this is 100% dead code, functions always end in returns!
250	
251	wacc_insert:
252		// push {fp, lr}
253		stp fp, lr, [sp, #-16]!
254		// push {x19, x20, x21}
255		stp x19, x20, [sp, #-32]!
256		stp x21, xzr, [sp, #16]
257		mov fp, sp
258		cmp x0, #0
259		b.eq .L0
260		// Stack pointer unchanged, no stack allocated variables
261		cbz x0, _errNull
262		ldr x8, [x0, #8]
263		mov x8, x8
264		mov x19, x8
265		cbz x0, _errNull
266		ldr x8, [x0, #0]
267		mov x8, x8
268		mov x20, x8
269		mov x8, #0
270		mov x21, x8
271		cmp x1, x20
272		b.lt .L2
273		cbz x19, _errNull
274		ldr x8, [x19, #8]
275		mov x8, x8
276		// push {x8}
277		stp x8, xzr, [sp, #-16]!
278		// pop {x8}
279		ldp x8, xzr, [sp], #16
280		mov x8, x8
281		mov x21, x8
282		// push {x0, x1}
283		stp x0, x1, [sp, #-16]!
284		// Set up X16 as a temporary second base pointer for the caller saved things
285		mov x16, sp
286		// Stack pointer unchanged, no stack allocated arguments
287		mov x8, x21
288		mov x0, x8
289		mov x8, x1
290		mov x1, x8
291		bl wacc_insert
292		mov x16, x0
293		// Stack pointer unchanged, no stack allocated arguments
294		// pop {x0, x1}
295		ldp x0, x1, [sp], #16
296		cbz x19, _errNull
297		mov x8, x16
298		str x8, [x19, #8]
299		b .L3
300	.L2:
301		cbz x19, _errNull
302		ldr x8, [x19, #0]
303		mov x8, x8
304		// push {x8}
305		stp x8, xzr, [sp, #-16]!
306		// pop {x8}
307		ldp x8, xzr, [sp], #16
308		mov x8, x8
309		mov x21, x8
310		// push {x0, x1}
311		stp x0, x1, [sp, #-16]!
312		// Set up X16 as a temporary second base pointer for the caller saved things
313		mov x16, sp
314		// Stack pointer unchanged, no stack allocated arguments
315		mov x8, x21
316		mov x0, x8
317		mov x8, x1
318		mov x1, x8
319		bl wacc_insert
320		mov x16, x0
321		// Stack pointer unchanged, no stack allocated arguments
322		// pop {x0, x1}
323		ldp x0, x1, [sp], #16
324		cbz x19, _errNull
325		mov x8, x16
326		str x8, [x19, #0]
327	.L3:
328		// Stack pointer unchanged, no stack allocated variables
329		b .L1
330	.L0:
331		// push {x0, x1}
332		stp x0, x1, [sp, #-16]!
333		// Set up X16 as a temporary second base pointer for the caller saved things
334		mov x16, sp
335		// Stack pointer unchanged, no stack allocated arguments
336		mov x8, x1
337		mov x0, x8
338		mov x8, #0
339		mov x1, x8
340		mov x8, #0
341		mov x2, x8
342		bl wacc_createNewNode
343		mov x16, x0
344		// Stack pointer unchanged, no stack allocated arguments
345		// pop {x0, x1}
346		ldp x0, x1, [sp], #16
347		mov x8, x16
348		mov x0, x8
349	.L1:
350		mov x0, x0
351		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
352		mov sp, fp
353		// pop {x19, x20, x21}
354		ldp x21, xzr, [sp, #16]
355		ldp x19, x20, [sp], #32
356		// pop {fp, lr}
357		ldp fp, lr, [sp], #16
358		ret
359		// 'ere be dragons: this is 100% dead code, functions always end in returns!
360	
361	wacc_printTree:
362		// push {fp, lr}
363		stp fp, lr, [sp, #-16]!
364		// push {x19, x20, x21}
365		stp x19, x20, [sp, #-32]!
366		stp x21, xzr, [sp, #16]
367		mov fp, sp
368		cmp x0, #0
369		b.eq .L4
370		// Stack pointer unchanged, no stack allocated variables
371		cbz x0, _errNull
372		ldr x8, [x0, #8]
373		mov x8, x8
374		mov x19, x8
375		cbz x19, _errNull
376		ldr x8, [x19, #0]
377		mov x8, x8
378		mov x20, x8
379		// push {x0}
380		stp x0, xzr, [sp, #-16]!
381		// Set up X16 as a temporary second base pointer for the caller saved things
382		mov x16, sp
383		// Stack pointer unchanged, no stack allocated arguments
384		mov x8, x20
385		mov x0, x8
386		bl wacc_printTree
387		mov x16, x0
388		// Stack pointer unchanged, no stack allocated arguments
389		// pop {x0}
390		ldp x0, xzr, [sp], #16
391		mov x8, x16
392		mov x21, x8
393		cbz x0, _errNull
394		ldr x8, [x0, #0]
395		mov x8, x8
396		// push {x8}
397		stp x8, xzr, [sp, #-16]!
398		// pop {x8}
399		ldp x8, xzr, [sp], #16
400		mov x8, x8
401		mov x21, x8
402		// push {x0}
403		stp x0, xzr, [sp, #-16]!
404		// Set up X16 as a temporary second base pointer for the caller saved things
405		mov x16, sp
406		// Stack pointer unchanged, no stack allocated arguments
407		mov x8, x21
408		mov x0, x8
409		// statement primitives do not return results (but will clobber r0/rax)
410		bl _printi
411		// pop {x0}
412		ldp x0, xzr, [sp], #16
413		// push {x0}
414		stp x0, xzr, [sp, #-16]!
415		// Set up X16 as a temporary second base pointer for the caller saved things
416		mov x16, sp
417		// Stack pointer unchanged, no stack allocated arguments
418		mov x8, #32
419		mov x0, x8
420		// statement primitives do not return results (but will clobber r0/rax)
421		bl _printc
422		// pop {x0}
423		ldp x0, xzr, [sp], #16
424		cbz x19, _errNull
425		ldr x8, [x19, #8]
426		mov x8, x8
427		// push {x8}
428		stp x8, xzr, [sp, #-16]!
429		// pop {x8}
430		ldp x8, xzr, [sp], #16
431		mov x8, x8
432		mov x20, x8
433		// push {x0}
434		stp x0, xzr, [sp, #-16]!
435		// Set up X16 as a temporary second base pointer for the caller saved things
436		mov x16, sp
437		// Stack pointer unchanged, no stack allocated arguments
438		mov x8, x20
439		mov x0, x8
440		bl wacc_printTree
441		mov x16, x0
442		// Stack pointer unchanged, no stack allocated arguments
443		// pop {x0}
444		ldp x0, xzr, [sp], #16
445		mov x8, x16
446		mov x21, x8
447		mov x0, #0
448		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
449		mov sp, fp
450		// pop {x19, x20, x21}
451		ldp x21, xzr, [sp, #16]
452		ldp x19, x20, [sp], #32
453		// pop {fp, lr}
454		ldp fp, lr, [sp], #16
455		ret
456		// Stack pointer unchanged, no stack allocated variables
457		b .L5
458	.L4:
459		mov x0, #0
460		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
461		mov sp, fp
462		// pop {x19, x20, x21}
463		ldp x21, xzr, [sp, #16]
464		ldp x19, x20, [sp], #32
465		// pop {fp, lr}
466		ldp fp, lr, [sp], #16
467		ret
468	.L5:
469		// 'ere be dragons: this is 100% dead code, functions always end in returns!
470	
471	// length of .L._prints_str0
472		.word 4
473	.L._prints_str0:
474		.asciz "%.*s"
475	.align 4
476	_prints:
477		// push {lr}
478		stp lr, xzr, [sp, #-16]!
479		mov x2, x0
480		ldrsw x1, [x0, #-4]
481		adr x0, .L._prints_str0
482		bl printf
483		mov x0, #0
484		bl fflush
485		// pop {lr}
486		ldp lr, xzr, [sp], #16
487		ret
488	
489	// length of .L._readi_str0
490		.word 2
491	.L._readi_str0:
492		.asciz "%d"
493	.align 4
494	_readi:
495		// X0 contains the "original" value of the destination of the read
496		// allocate space on the stack to store the read: preserve alignment!
497		// the passed default argument should be stored in case of EOF
498		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
499		// push {x0, lr}
500		stp x0, lr, [sp, #-16]!
501		mov x1, sp
502		adr x0, .L._readi_str0
503		bl scanf
504		// pop {x0, lr}
505		ldp x0, lr, [sp], #16
506		ret
507	
508	_malloc:
509		// push {lr}
510		stp lr, xzr, [sp, #-16]!
511		bl malloc
512		cbz x0, _errOutOfMemory
513		// pop {lr}
514		ldp lr, xzr, [sp], #16
515		ret
516	
517	// length of .L._printi_str0
518		.word 2
519	.L._printi_str0:
520		.asciz "%d"
521	.align 4
522	_printi:
523		// push {lr}
524		stp lr, xzr, [sp, #-16]!
525		mov x1, x0
526		adr x0, .L._printi_str0
527		bl printf
528		mov x0, #0
529		bl fflush
530		// pop {lr}
531		ldp lr, xzr, [sp], #16
532		ret
533	
534	// length of .L._printc_str0
535		.word 2
536	.L._printc_str0:
537		.asciz "%c"
538	.align 4
539	_printc:
540		// push {lr}
541		stp lr, xzr, [sp, #-16]!
542		mov x1, x0
543		adr x0, .L._printc_str0
544		bl printf
545		mov x0, #0
546		bl fflush
547		// pop {lr}
548		ldp lr, xzr, [sp], #16
549		ret
550	
551	// length of .L._println_str0
552		.word 0
553	.L._println_str0:
554		.asciz ""
555	.align 4
556	_println:
557		// push {lr}
558		stp lr, xzr, [sp, #-16]!
559		adr x0, .L._println_str0
560		bl puts
561		mov x0, #0
562		bl fflush
563		// pop {lr}
564		ldp lr, xzr, [sp], #16
565		ret
566	
567	// length of .L._errOutOfMemory_str0
568		.word 27
569	.L._errOutOfMemory_str0:
570		.asciz "fatal error: out of memory\n"
571	.align 4
572	_errOutOfMemory:
573		adr x0, .L._errOutOfMemory_str0
574		bl _prints
575		mov w0, #-1
576		bl exit
577	
578	// length of .L._errNull_str0
579		.word 45
580	.L._errNull_str0:
581		.asciz "fatal error: null pair dereferenced or freed\n"
582	.align 4
583	_errNull:
584		adr x0, .L._errNull_str0
585		bl _prints
586		mov w0, #-1
587		bl exit
588	
589	// length of .L._errOverflow_str0
590		.word 52
591	.L._errOverflow_str0:
592		.asciz "fatal error: integer overflow or underflow occurred\n"
593	.align 4
594	_errOverflow:
595		adr x0, .L._errOverflow_str0
596		bl _prints
597		mov w0, #-1
598		bl exit
===========================================================
-- Finished

