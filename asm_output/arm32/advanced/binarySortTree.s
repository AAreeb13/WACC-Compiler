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
1	@ length of .L.str0
2		.word 47
3	.L.str0:
4		.asciz "Please enter the number of integers to insert: "
5	@ length of .L.str1
6		.word 10
7	.L.str1:
8		.asciz "There are "
9	@ length of .L.str2
10		.word 10
11	.L.str2:
12		.asciz " integers."
13	@ length of .L.str3
14		.word 36
15	.L.str3:
16		.asciz "Please enter the number at position "
17	@ length of .L.str4
18		.word 3
19	.L.str4:
20		.asciz " : "
21	@ length of .L.str5
22		.word 29
23	.L.str5:
24		.asciz "Here are the numbers sorted: "
25	@ length of .L.str6
26		.word 0
27	.L.str6:
28		.asciz ""
29	.align 4
30	.text
31	.global main
32	main:
33		push {fp, lr}
34		push {r4, r5, r6, r7, r8, r10, r12}
35		mov fp, sp
36		@ Stack pointer unchanged, no stack allocated variables
37		mov r8, #0
38		mov r4, r8
39		@ Stack pointer unchanged, no stack allocated arguments
40		ldr r8, =.L.str0
41		push {r8}
42		pop {r8}
43		mov r8, r8
44		mov r0, r8
45		@ statement primitives do not return results (but will clobber r0/rax)
46		bl _prints
47		@ Stack pointer unchanged, no stack allocated arguments
48		@ load the current value in the destination of the read so it supports defaults
49		mov r8, r4
50		mov r0, r8
51		bl _readi
52		mov r12, r0
53		mov r8, r12
54		mov r4, r8
55		@ Stack pointer unchanged, no stack allocated arguments
56		ldr r8, =.L.str1
57		push {r8}
58		pop {r8}
59		mov r8, r8
60		mov r0, r8
61		@ statement primitives do not return results (but will clobber r0/rax)
62		bl _prints
63		@ Stack pointer unchanged, no stack allocated arguments
64		mov r8, r4
65		mov r0, r8
66		@ statement primitives do not return results (but will clobber r0/rax)
67		bl _printi
68		@ Stack pointer unchanged, no stack allocated arguments
69		ldr r8, =.L.str2
70		push {r8}
71		pop {r8}
72		mov r8, r8
73		mov r0, r8
74		@ statement primitives do not return results (but will clobber r0/rax)
75		bl _prints
76		bl _println
77		mov r8, #0
78		mov r5, r8
79		mov r8, #0
80		mov r6, r8
81		b .L6
82	.L7:
83		@ Stack pointer unchanged, no stack allocated variables
84		mov r8, #0
85		mov r7, r8
86		@ Stack pointer unchanged, no stack allocated arguments
87		ldr r8, =.L.str3
88		push {r8}
89		pop {r8}
90		mov r8, r8
91		mov r0, r8
92		@ statement primitives do not return results (but will clobber r0/rax)
93		bl _prints
94		@ Stack pointer unchanged, no stack allocated arguments
95		adds r8, r5, #1
96		blvs _errOverflow
97		push {r8}
98		pop {r8}
99		mov r8, r8
100		mov r0, r8
101		@ statement primitives do not return results (but will clobber r0/rax)
102		bl _printi
103		@ Stack pointer unchanged, no stack allocated arguments
104		ldr r8, =.L.str4
105		push {r8}
106		pop {r8}
107		mov r8, r8
108		mov r0, r8
109		@ statement primitives do not return results (but will clobber r0/rax)
110		bl _prints
111		@ Stack pointer unchanged, no stack allocated arguments
112		@ load the current value in the destination of the read so it supports defaults
113		mov r8, r7
114		mov r0, r8
115		bl _readi
116		mov r12, r0
117		mov r8, r12
118		mov r7, r8
119		@ Stack pointer unchanged, no stack allocated arguments
120		mov r8, r6
121		mov r0, r8
122		mov r8, r7
123		mov r1, r8
124		bl wacc_insert
125		mov r12, r0
126		@ Stack pointer unchanged, no stack allocated arguments
127		mov r8, r12
128		mov r6, r8
129		adds r8, r5, #1
130		blvs _errOverflow
131		push {r8}
132		pop {r8}
133		mov r8, r8
134		mov r5, r8
135		@ Stack pointer unchanged, no stack allocated variables
136	.L6:
137		cmp r5, r4
138		blt .L7
139		@ Stack pointer unchanged, no stack allocated arguments
140		ldr r8, =.L.str5
141		push {r8}
142		pop {r8}
143		mov r8, r8
144		mov r0, r8
145		@ statement primitives do not return results (but will clobber r0/rax)
146		bl _prints
147		@ Stack pointer unchanged, no stack allocated arguments
148		mov r8, r6
149		mov r0, r8
150		bl wacc_printTree
151		mov r12, r0
152		@ Stack pointer unchanged, no stack allocated arguments
153		mov r8, r12
154		mov r5, r8
155		@ Stack pointer unchanged, no stack allocated arguments
156		ldr r8, =.L.str6
157		push {r8}
158		pop {r8}
159		mov r8, r8
160		mov r0, r8
161		@ statement primitives do not return results (but will clobber r0/rax)
162		bl _prints
163		bl _println
164		@ Stack pointer unchanged, no stack allocated variables
165		mov r0, #0
166		pop {r4, r5, r6, r7, r8, r10, r12}
167		pop {fp, pc}
168	
169	wacc_createNewNode:
170		push {fp, lr}
171		push {r4, r5}
172		mov fp, sp
173		@ Stack pointer unchanged, no stack allocated variables
174		push {r0, r1, r2}
175		mov r0, #8
176		bl _malloc
177		mov r12, r0
178		pop {r0, r1, r2}
179		mov r8, r1
180		str r8, [r12, #0]
181		mov r8, r2
182		str r8, [r12, #4]
183		mov r8, r12
184		mov r4, r8
185		push {r0, r1, r2}
186		mov r0, #8
187		bl _malloc
188		mov r12, r0
189		pop {r0, r1, r2}
190		mov r8, r0
191		str r8, [r12, #0]
192		mov r8, r4
193		str r8, [r12, #4]
194		mov r8, r12
195		mov r5, r8
196		mov r0, r5
197		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
198		mov sp, fp
199		pop {r4, r5}
200		pop {fp, pc}
201		@ Stack pointer unchanged, no stack allocated variables
202		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
203		@ the .ltorg here is used to generate the constant pool after every function
204		@ this is important for ensuring string literals and large integers are accessible!
205		.ltorg
206	
207	wacc_insert:
208		push {fp, lr}
209		push {r4, r5, r6}
210		mov fp, sp
211		cmp r0, #0
212		beq .L0
213		@ Stack pointer unchanged, no stack allocated variables
214		cmp r0, #0
215		bleq _errNull
216		ldr r8, [r0, #4]
217		mov r8, r8
218		mov r4, r8
219		cmp r0, #0
220		bleq _errNull
221		ldr r8, [r0, #0]
222		mov r8, r8
223		mov r5, r8
224		mov r8, #0
225		mov r6, r8
226		cmp r1, r5
227		blt .L2
228		cmp r4, #0
229		bleq _errNull
230		ldr r8, [r4, #4]
231		mov r8, r8
232		push {r8}
233		pop {r8}
234		mov r8, r8
235		mov r6, r8
236		push {r0, r1}
237		@ Set up R12 as a temporary second base pointer for the caller saved things
238		mov r12, sp
239		@ Stack pointer unchanged, no stack allocated arguments
240		mov r8, r6
241		mov r0, r8
242		mov r8, r1
243		mov r1, r8
244		bl wacc_insert
245		mov r12, r0
246		@ Stack pointer unchanged, no stack allocated arguments
247		pop {r0, r1}
248		cmp r4, #0
249		bleq _errNull
250		mov r8, r12
251		str r8, [r4, #4]
252		b .L3
253	.L2:
254		cmp r4, #0
255		bleq _errNull
256		ldr r8, [r4, #0]
257		mov r8, r8
258		push {r8}
259		pop {r8}
260		mov r8, r8
261		mov r6, r8
262		push {r0, r1}
263		@ Set up R12 as a temporary second base pointer for the caller saved things
264		mov r12, sp
265		@ Stack pointer unchanged, no stack allocated arguments
266		mov r8, r6
267		mov r0, r8
268		mov r8, r1
269		mov r1, r8
270		bl wacc_insert
271		mov r12, r0
272		@ Stack pointer unchanged, no stack allocated arguments
273		pop {r0, r1}
274		cmp r4, #0
275		bleq _errNull
276		mov r8, r12
277		str r8, [r4, #0]
278	.L3:
279		@ Stack pointer unchanged, no stack allocated variables
280		b .L1
281	.L0:
282		push {r0, r1}
283		@ Set up R12 as a temporary second base pointer for the caller saved things
284		mov r12, sp
285		@ Stack pointer unchanged, no stack allocated arguments
286		mov r8, r1
287		mov r0, r8
288		mov r8, #0
289		mov r1, r8
290		mov r8, #0
291		mov r2, r8
292		bl wacc_createNewNode
293		mov r12, r0
294		@ Stack pointer unchanged, no stack allocated arguments
295		pop {r0, r1}
296		mov r8, r12
297		mov r0, r8
298	.L1:
299		mov r0, r0
300		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
301		mov sp, fp
302		pop {r4, r5, r6}
303		pop {fp, pc}
304		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
305		@ the .ltorg here is used to generate the constant pool after every function
306		@ this is important for ensuring string literals and large integers are accessible!
307		.ltorg
308	
309	wacc_printTree:
310		push {fp, lr}
311		push {r4, r5, r6}
312		mov fp, sp
313		cmp r0, #0
314		beq .L4
315		@ Stack pointer unchanged, no stack allocated variables
316		cmp r0, #0
317		bleq _errNull
318		ldr r8, [r0, #4]
319		mov r8, r8
320		mov r4, r8
321		cmp r4, #0
322		bleq _errNull
323		ldr r8, [r4, #0]
324		mov r8, r8
325		mov r5, r8
326		push {r0}
327		@ Set up R12 as a temporary second base pointer for the caller saved things
328		mov r12, sp
329		@ Stack pointer unchanged, no stack allocated arguments
330		mov r8, r5
331		mov r0, r8
332		bl wacc_printTree
333		mov r12, r0
334		@ Stack pointer unchanged, no stack allocated arguments
335		pop {r0}
336		mov r8, r12
337		mov r6, r8
338		cmp r0, #0
339		bleq _errNull
340		ldr r8, [r0, #0]
341		mov r8, r8
342		push {r8}
343		pop {r8}
344		mov r8, r8
345		mov r6, r8
346		push {r0}
347		@ Set up R12 as a temporary second base pointer for the caller saved things
348		mov r12, sp
349		@ Stack pointer unchanged, no stack allocated arguments
350		mov r8, r6
351		mov r0, r8
352		@ statement primitives do not return results (but will clobber r0/rax)
353		bl _printi
354		pop {r0}
355		push {r0}
356		@ Set up R12 as a temporary second base pointer for the caller saved things
357		mov r12, sp
358		@ Stack pointer unchanged, no stack allocated arguments
359		mov r8, #32
360		mov r0, r8
361		@ statement primitives do not return results (but will clobber r0/rax)
362		bl _printc
363		pop {r0}
364		cmp r4, #0
365		bleq _errNull
366		ldr r8, [r4, #4]
367		mov r8, r8
368		push {r8}
369		pop {r8}
370		mov r8, r8
371		mov r5, r8
372		push {r0}
373		@ Set up R12 as a temporary second base pointer for the caller saved things
374		mov r12, sp
375		@ Stack pointer unchanged, no stack allocated arguments
376		mov r8, r5
377		mov r0, r8
378		bl wacc_printTree
379		mov r12, r0
380		@ Stack pointer unchanged, no stack allocated arguments
381		pop {r0}
382		mov r8, r12
383		mov r6, r8
384		mov r0, #0
385		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
386		mov sp, fp
387		pop {r4, r5, r6}
388		pop {fp, pc}
389		@ Stack pointer unchanged, no stack allocated variables
390		b .L5
391	.L4:
392		mov r0, #0
393		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
394		mov sp, fp
395		pop {r4, r5, r6}
396		pop {fp, pc}
397	.L5:
398		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
399		@ the .ltorg here is used to generate the constant pool after every function
400		@ this is important for ensuring string literals and large integers are accessible!
401		.ltorg
402	
403	@ length of .L._prints_str0
404		.word 4
405	.L._prints_str0:
406		.asciz "%.*s"
407	.align 4
408	_prints:
409		push {fp, lr}
410		mov fp, sp
411		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
412		bic sp, sp, #0x7
413		mov r2, r0
414		ldr r1, [r0, #-4]
415		adr r0, .L._prints_str0
416		bl printf
417		mov r0, #0
418		bl fflush
419		mov sp, fp
420		pop {fp, pc}
421	
422	@ length of .L._readi_str0
423		.word 2
424	.L._readi_str0:
425		.asciz "%d"
426	.align 4
427	_readi:
428		push {fp, lr}
429		mov fp, sp
430		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
431		bic sp, sp, #0x7
432		@ R0 contains the "original" value of the destination of the read
433		@ allocate space on the stack to store the read: preserve alignment!
434		@ the passed default argument should be stored in case of EOF
435		sub sp, sp, #8
436		str r0, [sp, #0]
437		mov r1, sp
438		adr r0, .L._readi_str0
439		bl scanf
440		ldr r0, [sp, #0]
441		add sp, sp, #8
442		mov sp, fp
443		pop {fp, pc}
444	
445	_malloc:
446		push {fp, lr}
447		mov fp, sp
448		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
449		bic sp, sp, #0x7
450		bl malloc
451		cmp r0, #0
452		bleq _errOutOfMemory
453		mov sp, fp
454		pop {fp, pc}
455	
456	@ length of .L._printi_str0
457		.word 2
458	.L._printi_str0:
459		.asciz "%d"
460	.align 4
461	_printi:
462		push {fp, lr}
463		mov fp, sp
464		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
465		bic sp, sp, #0x7
466		mov r1, r0
467		adr r0, .L._printi_str0
468		bl printf
469		mov r0, #0
470		bl fflush
471		mov sp, fp
472		pop {fp, pc}
473	
474	@ length of .L._printc_str0
475		.word 2
476	.L._printc_str0:
477		.asciz "%c"
478	.align 4
479	_printc:
480		push {fp, lr}
481		mov fp, sp
482		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
483		bic sp, sp, #0x7
484		mov r1, r0
485		adr r0, .L._printc_str0
486		bl printf
487		mov r0, #0
488		bl fflush
489		mov sp, fp
490		pop {fp, pc}
491	
492	@ length of .L._println_str0
493		.word 0
494	.L._println_str0:
495		.asciz ""
496	.align 4
497	_println:
498		push {fp, lr}
499		mov fp, sp
500		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
501		bic sp, sp, #0x7
502		adr r0, .L._println_str0
503		bl puts
504		mov r0, #0
505		bl fflush
506		mov sp, fp
507		pop {fp, pc}
508	
509	_exit:
510		push {fp, lr}
511		mov fp, sp
512		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
513		bic sp, sp, #0x7
514		bl exit
515		mov sp, fp
516		pop {fp, pc}
517	
518	@ length of .L._errOutOfMemory_str0
519		.word 27
520	.L._errOutOfMemory_str0:
521		.asciz "fatal error: out of memory\n"
522	.align 4
523	_errOutOfMemory:
524		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
525		bic sp, sp, #0x7
526		adr r0, .L._errOutOfMemory_str0
527		bl _prints
528		mov r0, #255
529		bl _exit
530	
531	@ length of .L._errNull_str0
532		.word 45
533	.L._errNull_str0:
534		.asciz "fatal error: null pair dereferenced or freed\n"
535	.align 4
536	_errNull:
537		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
538		bic sp, sp, #0x7
539		adr r0, .L._errNull_str0
540		bl _prints
541		mov r0, #255
542		bl _exit
543	
544	@ length of .L._errOverflow_str0
545		.word 52
546	.L._errOverflow_str0:
547		.asciz "fatal error: integer overflow or underflow occurred\n"
548	.align 4
549	_errOverflow:
550		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
551		bic sp, sp, #0x7
552		adr r0, .L._errOverflow_str0
553		bl _prints
554		mov r0, #255
555		bl _exit
===========================================================
-- Finished

