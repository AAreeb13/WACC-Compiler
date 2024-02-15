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
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 47
4	.L.str0:
5		.asciz "Please enter the number of integers to insert: "
6	# length of .L.str1
7		.int 10
8	.L.str1:
9		.asciz "There are "
10	# length of .L.str2
11		.int 10
12	.L.str2:
13		.asciz " integers."
14	# length of .L.str3
15		.int 36
16	.L.str3:
17		.asciz "Please enter the number at position "
18	# length of .L.str4
19		.int 3
20	.L.str4:
21		.asciz " : "
22	# length of .L.str5
23		.int 29
24	.L.str5:
25		.asciz "Here are the numbers sorted: "
26	# length of .L.str6
27		.int 0
28	.L.str6:
29		.asciz ""
30	.text
31	main:
32		pushq %rbp
33		# pushq {%rbx, %r12, %r13, %r14, %r15}
34		subq $40, %rsp
35		movq %rbx, (%rsp)
36		movq %r12, 8(%rsp)
37		movq %r13, 16(%rsp)
38		movq %r14, 24(%rsp)
39		movq %r15, 32(%rsp)
40		movq %rsp, %rbp
41		# Stack pointer unchanged, no stack allocated variables
42		movq $0, %rax
43		movq %rax, %r12
44		# Stack pointer unchanged, no stack allocated arguments
45		leaq .L.str0(%rip), %rax
46		pushq %rax
47		popq %rax
48		movq %rax, %rax
49		movq %rax, %rdi
50		# statement primitives do not return results (but will clobber r0/rax)
51		call _prints
52		# Stack pointer unchanged, no stack allocated arguments
53		# load the current value in the destination of the read so it supports defaults
54		movq %r12, %rax
55		movq %rax, %rdi
56		call _readi
57		movq %rax, %r11
58		movq %r11, %rax
59		movq %rax, %r12
60		# Stack pointer unchanged, no stack allocated arguments
61		leaq .L.str1(%rip), %rax
62		pushq %rax
63		popq %rax
64		movq %rax, %rax
65		movq %rax, %rdi
66		# statement primitives do not return results (but will clobber r0/rax)
67		call _prints
68		# Stack pointer unchanged, no stack allocated arguments
69		movq %r12, %rax
70		movq %rax, %rdi
71		# statement primitives do not return results (but will clobber r0/rax)
72		call _printi
73		# Stack pointer unchanged, no stack allocated arguments
74		leaq .L.str2(%rip), %rax
75		pushq %rax
76		popq %rax
77		movq %rax, %rax
78		movq %rax, %rdi
79		# statement primitives do not return results (but will clobber r0/rax)
80		call _prints
81		call _println
82		movq $0, %rax
83		movq %rax, %r13
84		movq $0, %rax
85		movq %rax, %r14
86		jmp .L6
87	.L7:
88		# Stack pointer unchanged, no stack allocated variables
89		movq $0, %rax
90		movq %rax, %r15
91		# Stack pointer unchanged, no stack allocated arguments
92		leaq .L.str3(%rip), %rax
93		pushq %rax
94		popq %rax
95		movq %rax, %rax
96		movq %rax, %rdi
97		# statement primitives do not return results (but will clobber r0/rax)
98		call _prints
99		# Stack pointer unchanged, no stack allocated arguments
100		movl %r13d, %eax
101		addl $1, %eax
102		jo _errOverflow
103		movslq %eax, %rax
104		pushq %rax
105		popq %rax
106		movq %rax, %rax
107		movq %rax, %rdi
108		# statement primitives do not return results (but will clobber r0/rax)
109		call _printi
110		# Stack pointer unchanged, no stack allocated arguments
111		leaq .L.str4(%rip), %rax
112		pushq %rax
113		popq %rax
114		movq %rax, %rax
115		movq %rax, %rdi
116		# statement primitives do not return results (but will clobber r0/rax)
117		call _prints
118		# Stack pointer unchanged, no stack allocated arguments
119		# load the current value in the destination of the read so it supports defaults
120		movq %r15, %rax
121		movq %rax, %rdi
122		call _readi
123		movq %rax, %r11
124		movq %r11, %rax
125		movq %rax, %r15
126		# Stack pointer unchanged, no stack allocated arguments
127		movq %r14, %rax
128		movq %rax, %rdi
129		movq %r15, %rax
130		movq %rax, %rsi
131		call wacc_insert
132		movq %rax, %r11
133		# Stack pointer unchanged, no stack allocated arguments
134		movq %r11, %rax
135		movq %rax, %r14
136		movl %r13d, %eax
137		addl $1, %eax
138		jo _errOverflow
139		movslq %eax, %rax
140		pushq %rax
141		popq %rax
142		movq %rax, %rax
143		movq %rax, %r13
144		# Stack pointer unchanged, no stack allocated variables
145	.L6:
146		cmpq %r12, %r13
147		jl .L7
148		# Stack pointer unchanged, no stack allocated arguments
149		leaq .L.str5(%rip), %rax
150		pushq %rax
151		popq %rax
152		movq %rax, %rax
153		movq %rax, %rdi
154		# statement primitives do not return results (but will clobber r0/rax)
155		call _prints
156		# Stack pointer unchanged, no stack allocated arguments
157		movq %r14, %rax
158		movq %rax, %rdi
159		call wacc_printTree
160		movq %rax, %r11
161		# Stack pointer unchanged, no stack allocated arguments
162		movq %r11, %rax
163		movq %rax, %r13
164		# Stack pointer unchanged, no stack allocated arguments
165		leaq .L.str6(%rip), %rax
166		pushq %rax
167		popq %rax
168		movq %rax, %rax
169		movq %rax, %rdi
170		# statement primitives do not return results (but will clobber r0/rax)
171		call _prints
172		call _println
173		# Stack pointer unchanged, no stack allocated variables
174		movq $0, %rax
175		# popq {%rbx, %r12, %r13, %r14, %r15}
176		movq (%rsp), %rbx
177		movq 8(%rsp), %r12
178		movq 16(%rsp), %r13
179		movq 24(%rsp), %r14
180		movq 32(%rsp), %r15
181		addq $40, %rsp
182		popq %rbp
183		ret
184	
185	wacc_createNewNode:
186		pushq %rbp
187		# pushq {%r12, %r13}
188		subq $16, %rsp
189		movq %r12, (%rsp)
190		movq %r13, 8(%rsp)
191		movq %rsp, %rbp
192		# Stack pointer unchanged, no stack allocated variables
193		# pushq {%rdx, %rsi, %rdi}
194		subq $24, %rsp
195		movq %rdx, (%rsp)
196		movq %rsi, 8(%rsp)
197		movq %rdi, 16(%rsp)
198		movl $16, %edi
199		call _malloc
200		movq %rax, %r11
201		# popq {%rdx, %rsi, %rdi}
202		movq (%rsp), %rdx
203		movq 8(%rsp), %rsi
204		movq 16(%rsp), %rdi
205		addq $24, %rsp
206		movq %rsi, %rax
207		movq %rax, (%r11)
208		movq %rdx, %rax
209		movq %rax, 8(%r11)
210		movq %r11, %rax
211		movq %rax, %r12
212		# pushq {%rdx, %rsi, %rdi}
213		subq $24, %rsp
214		movq %rdx, (%rsp)
215		movq %rsi, 8(%rsp)
216		movq %rdi, 16(%rsp)
217		movl $16, %edi
218		call _malloc
219		movq %rax, %r11
220		# popq {%rdx, %rsi, %rdi}
221		movq (%rsp), %rdx
222		movq 8(%rsp), %rsi
223		movq 16(%rsp), %rdi
224		addq $24, %rsp
225		movq %rdi, %rax
226		movq %rax, (%r11)
227		movq %r12, %rax
228		movq %rax, 8(%r11)
229		movq %r11, %rax
230		movq %rax, %r13
231		movq %r13, %rax
232		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
233		movq %rbp, %rsp
234		# popq {%r12, %r13}
235		movq (%rsp), %r12
236		movq 8(%rsp), %r13
237		addq $16, %rsp
238		popq %rbp
239		ret
240		# Stack pointer unchanged, no stack allocated variables
241		# 'ere be dragons: this is 100% dead code, functions always end in returns!
242	
243	wacc_insert:
244		pushq %rbp
245		# pushq {%r12, %r13, %r14}
246		subq $24, %rsp
247		movq %r12, (%rsp)
248		movq %r13, 8(%rsp)
249		movq %r14, 16(%rsp)
250		movq %rsp, %rbp
251		cmpq $0, %rdi
252		je .L0
253		# Stack pointer unchanged, no stack allocated variables
254		cmpq $0, %rdi
255		je _errNull
256		movq 8(%rdi), %rax
257		movq %rax, %rax
258		movq %rax, %r12
259		cmpq $0, %rdi
260		je _errNull
261		movq (%rdi), %rax
262		movq %rax, %rax
263		movq %rax, %r13
264		movq $0, %rax
265		movq %rax, %r14
266		cmpq %r13, %rsi
267		jl .L2
268		cmpq $0, %r12
269		je _errNull
270		movq 8(%r12), %rax
271		movq %rax, %rax
272		pushq %rax
273		popq %rax
274		movq %rax, %rax
275		movq %rax, %r14
276		# pushq {%rsi, %rdi}
277		subq $16, %rsp
278		movq %rsi, (%rsp)
279		movq %rdi, 8(%rsp)
280		# Set up R11 as a temporary second base pointer for the caller saved things
281		movq %rsp, %r11
282		# Stack pointer unchanged, no stack allocated arguments
283		movq %r14, %rax
284		movq %rax, %rdi
285		movq %rsi, %rax
286		movq %rax, %rsi
287		call wacc_insert
288		movq %rax, %r11
289		# Stack pointer unchanged, no stack allocated arguments
290		# popq {%rsi, %rdi}
291		movq (%rsp), %rsi
292		movq 8(%rsp), %rdi
293		addq $16, %rsp
294		cmpq $0, %r12
295		je _errNull
296		movq %r11, %rax
297		movq %rax, 8(%r12)
298		jmp .L3
299	.L2:
300		cmpq $0, %r12
301		je _errNull
302		movq (%r12), %rax
303		movq %rax, %rax
304		pushq %rax
305		popq %rax
306		movq %rax, %rax
307		movq %rax, %r14
308		# pushq {%rsi, %rdi}
309		subq $16, %rsp
310		movq %rsi, (%rsp)
311		movq %rdi, 8(%rsp)
312		# Set up R11 as a temporary second base pointer for the caller saved things
313		movq %rsp, %r11
314		# Stack pointer unchanged, no stack allocated arguments
315		movq %r14, %rax
316		movq %rax, %rdi
317		movq %rsi, %rax
318		movq %rax, %rsi
319		call wacc_insert
320		movq %rax, %r11
321		# Stack pointer unchanged, no stack allocated arguments
322		# popq {%rsi, %rdi}
323		movq (%rsp), %rsi
324		movq 8(%rsp), %rdi
325		addq $16, %rsp
326		cmpq $0, %r12
327		je _errNull
328		movq %r11, %rax
329		movq %rax, (%r12)
330	.L3:
331		# Stack pointer unchanged, no stack allocated variables
332		jmp .L1
333	.L0:
334		# pushq {%rsi, %rdi}
335		subq $16, %rsp
336		movq %rsi, (%rsp)
337		movq %rdi, 8(%rsp)
338		# Set up R11 as a temporary second base pointer for the caller saved things
339		movq %rsp, %r11
340		# Stack pointer unchanged, no stack allocated arguments
341		movq %rsi, %rax
342		movq %rax, %rdi
343		movq $0, %rax
344		movq %rax, %rsi
345		movq $0, %rax
346		movq %rax, %rdx
347		call wacc_createNewNode
348		movq %rax, %r11
349		# Stack pointer unchanged, no stack allocated arguments
350		# popq {%rsi, %rdi}
351		movq (%rsp), %rsi
352		movq 8(%rsp), %rdi
353		addq $16, %rsp
354		movq %r11, %rax
355		movq %rax, %rdi
356	.L1:
357		movq %rdi, %rax
358		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
359		movq %rbp, %rsp
360		# popq {%r12, %r13, %r14}
361		movq (%rsp), %r12
362		movq 8(%rsp), %r13
363		movq 16(%rsp), %r14
364		addq $24, %rsp
365		popq %rbp
366		ret
367		# 'ere be dragons: this is 100% dead code, functions always end in returns!
368	
369	wacc_printTree:
370		pushq %rbp
371		# pushq {%r12, %r13, %r14}
372		subq $24, %rsp
373		movq %r12, (%rsp)
374		movq %r13, 8(%rsp)
375		movq %r14, 16(%rsp)
376		movq %rsp, %rbp
377		cmpq $0, %rdi
378		je .L4
379		# Stack pointer unchanged, no stack allocated variables
380		cmpq $0, %rdi
381		je _errNull
382		movq 8(%rdi), %rax
383		movq %rax, %rax
384		movq %rax, %r12
385		cmpq $0, %r12
386		je _errNull
387		movq (%r12), %rax
388		movq %rax, %rax
389		movq %rax, %r13
390		pushq %rdi
391		# Set up R11 as a temporary second base pointer for the caller saved things
392		movq %rsp, %r11
393		# Stack pointer unchanged, no stack allocated arguments
394		movq %r13, %rax
395		movq %rax, %rdi
396		call wacc_printTree
397		movq %rax, %r11
398		# Stack pointer unchanged, no stack allocated arguments
399		popq %rdi
400		movq %r11, %rax
401		movq %rax, %r14
402		cmpq $0, %rdi
403		je _errNull
404		movq (%rdi), %rax
405		movq %rax, %rax
406		pushq %rax
407		popq %rax
408		movq %rax, %rax
409		movq %rax, %r14
410		pushq %rdi
411		# Set up R11 as a temporary second base pointer for the caller saved things
412		movq %rsp, %r11
413		# Stack pointer unchanged, no stack allocated arguments
414		movq %r14, %rax
415		movq %rax, %rdi
416		# statement primitives do not return results (but will clobber r0/rax)
417		call _printi
418		popq %rdi
419		pushq %rdi
420		# Set up R11 as a temporary second base pointer for the caller saved things
421		movq %rsp, %r11
422		# Stack pointer unchanged, no stack allocated arguments
423		movq $32, %rax
424		movq %rax, %rdi
425		# statement primitives do not return results (but will clobber r0/rax)
426		call _printc
427		popq %rdi
428		cmpq $0, %r12
429		je _errNull
430		movq 8(%r12), %rax
431		movq %rax, %rax
432		pushq %rax
433		popq %rax
434		movq %rax, %rax
435		movq %rax, %r13
436		pushq %rdi
437		# Set up R11 as a temporary second base pointer for the caller saved things
438		movq %rsp, %r11
439		# Stack pointer unchanged, no stack allocated arguments
440		movq %r13, %rax
441		movq %rax, %rdi
442		call wacc_printTree
443		movq %rax, %r11
444		# Stack pointer unchanged, no stack allocated arguments
445		popq %rdi
446		movq %r11, %rax
447		movq %rax, %r14
448		movq $0, %rax
449		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
450		movq %rbp, %rsp
451		# popq {%r12, %r13, %r14}
452		movq (%rsp), %r12
453		movq 8(%rsp), %r13
454		movq 16(%rsp), %r14
455		addq $24, %rsp
456		popq %rbp
457		ret
458		# Stack pointer unchanged, no stack allocated variables
459		jmp .L5
460	.L4:
461		movq $0, %rax
462		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
463		movq %rbp, %rsp
464		# popq {%r12, %r13, %r14}
465		movq (%rsp), %r12
466		movq 8(%rsp), %r13
467		movq 16(%rsp), %r14
468		addq $24, %rsp
469		popq %rbp
470		ret
471	.L5:
472		# 'ere be dragons: this is 100% dead code, functions always end in returns!
473	
474	.section .rodata
475	# length of .L._prints_str0
476		.int 4
477	.L._prints_str0:
478		.asciz "%.*s"
479	.text
480	_prints:
481		pushq %rbp
482		movq %rsp, %rbp
483		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
484		andq $-16, %rsp
485		movq %rdi, %rdx
486		movl -4(%rdi), %esi
487		leaq .L._prints_str0(%rip), %rdi
488		# on x86, al represents the number of SIMD registers used as variadic arguments
489		movb $0, %al
490		call printf@plt
491		movq $0, %rdi
492		call fflush@plt
493		movq %rbp, %rsp
494		popq %rbp
495		ret
496	
497	.section .rodata
498	# length of .L._readi_str0
499		.int 2
500	.L._readi_str0:
501		.asciz "%d"
502	.text
503	_readi:
504		pushq %rbp
505		movq %rsp, %rbp
506		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
507		andq $-16, %rsp
508		# RDI contains the "original" value of the destination of the read
509		# allocate space on the stack to store the read: preserve alignment!
510		# the passed default argument should be stored in case of EOF
511		subq $16, %rsp
512		movl %edi, (%rsp)
513		leaq (%rsp), %rsi
514		leaq .L._readi_str0(%rip), %rdi
515		# on x86, al represents the number of SIMD registers used as variadic arguments
516		movb $0, %al
517		call scanf@plt
518		movslq (%rsp), %rax
519		addq $16, %rsp
520		movq %rbp, %rsp
521		popq %rbp
522		ret
523	
524	_malloc:
525		pushq %rbp
526		movq %rsp, %rbp
527		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
528		andq $-16, %rsp
529		call malloc@plt
530		cmpq $0, %rax
531		je _errOutOfMemory
532		movq %rbp, %rsp
533		popq %rbp
534		ret
535	
536	.section .rodata
537	# length of .L._printi_str0
538		.int 2
539	.L._printi_str0:
540		.asciz "%d"
541	.text
542	_printi:
543		pushq %rbp
544		movq %rsp, %rbp
545		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
546		andq $-16, %rsp
547		movl %edi, %esi
548		leaq .L._printi_str0(%rip), %rdi
549		# on x86, al represents the number of SIMD registers used as variadic arguments
550		movb $0, %al
551		call printf@plt
552		movq $0, %rdi
553		call fflush@plt
554		movq %rbp, %rsp
555		popq %rbp
556		ret
557	
558	.section .rodata
559	# length of .L._printc_str0
560		.int 2
561	.L._printc_str0:
562		.asciz "%c"
563	.text
564	_printc:
565		pushq %rbp
566		movq %rsp, %rbp
567		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
568		andq $-16, %rsp
569		movb %dil, %sil
570		leaq .L._printc_str0(%rip), %rdi
571		# on x86, al represents the number of SIMD registers used as variadic arguments
572		movb $0, %al
573		call printf@plt
574		movq $0, %rdi
575		call fflush@plt
576		movq %rbp, %rsp
577		popq %rbp
578		ret
579	
580	.section .rodata
581	# length of .L._println_str0
582		.int 0
583	.L._println_str0:
584		.asciz ""
585	.text
586	_println:
587		pushq %rbp
588		movq %rsp, %rbp
589		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
590		andq $-16, %rsp
591		leaq .L._println_str0(%rip), %rdi
592		call puts@plt
593		movq $0, %rdi
594		call fflush@plt
595		movq %rbp, %rsp
596		popq %rbp
597		ret
598	
599	_exit:
600		pushq %rbp
601		movq %rsp, %rbp
602		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
603		andq $-16, %rsp
604		call exit@plt
605		movq %rbp, %rsp
606		popq %rbp
607		ret
608	
609	.section .rodata
610	# length of .L._errOutOfMemory_str0
611		.int 27
612	.L._errOutOfMemory_str0:
613		.asciz "fatal error: out of memory\n"
614	.text
615	_errOutOfMemory:
616		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
617		andq $-16, %rsp
618		leaq .L._errOutOfMemory_str0(%rip), %rdi
619		call _prints
620		movb $-1, %dil
621		call exit@plt
622	
623	.section .rodata
624	# length of .L._errNull_str0
625		.int 45
626	.L._errNull_str0:
627		.asciz "fatal error: null pair dereferenced or freed\n"
628	.text
629	_errNull:
630		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
631		andq $-16, %rsp
632		leaq .L._errNull_str0(%rip), %rdi
633		call _prints
634		movb $-1, %dil
635		call exit@plt
636	
637	.section .rodata
638	# length of .L._errOverflow_str0
639		.int 52
640	.L._errOverflow_str0:
641		.asciz "fatal error: integer overflow or underflow occurred\n"
642	.text
643	_errOverflow:
644		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
645		andq $-16, %rsp
646		leaq .L._errOverflow_str0(%rip), %rdi
647		call _prints
648		movb $-1, %dil
649		call exit@plt
===========================================================
-- Finished

