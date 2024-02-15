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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 47
5	.L.str0:
6		.asciz "Please enter the number of integers to insert: "
7	# length of .L.str1
8		.int 10
9	.L.str1:
10		.asciz "There are "
11	# length of .L.str2
12		.int 10
13	.L.str2:
14		.asciz " integers."
15	# length of .L.str3
16		.int 36
17	.L.str3:
18		.asciz "Please enter the number at position "
19	# length of .L.str4
20		.int 3
21	.L.str4:
22		.asciz " : "
23	# length of .L.str5
24		.int 29
25	.L.str5:
26		.asciz "Here are the numbers sorted: "
27	# length of .L.str6
28		.int 0
29	.L.str6:
30		.asciz ""
31	.text
32	main:
33		push rbp
34		# push {rbx, r12, r13, r14, r15}
35		sub rsp, 40
36		mov qword ptr [rsp], rbx
37		mov qword ptr [rsp + 8], r12
38		mov qword ptr [rsp + 16], r13
39		mov qword ptr [rsp + 24], r14
40		mov qword ptr [rsp + 32], r15
41		mov rbp, rsp
42		# Stack pointer unchanged, no stack allocated variables
43		mov rax, 0
44		mov r12, rax
45		# Stack pointer unchanged, no stack allocated arguments
46		lea rax, [rip + .L.str0]
47		push rax
48		pop rax
49		mov rax, rax
50		mov rdi, rax
51		# statement primitives do not return results (but will clobber r0/rax)
52		call _prints
53		# Stack pointer unchanged, no stack allocated arguments
54		# load the current value in the destination of the read so it supports defaults
55		mov rax, r12
56		mov rdi, rax
57		call _readi
58		mov r11, rax
59		mov rax, r11
60		mov r12, rax
61		# Stack pointer unchanged, no stack allocated arguments
62		lea rax, [rip + .L.str1]
63		push rax
64		pop rax
65		mov rax, rax
66		mov rdi, rax
67		# statement primitives do not return results (but will clobber r0/rax)
68		call _prints
69		# Stack pointer unchanged, no stack allocated arguments
70		mov rax, r12
71		mov rdi, rax
72		# statement primitives do not return results (but will clobber r0/rax)
73		call _printi
74		# Stack pointer unchanged, no stack allocated arguments
75		lea rax, [rip + .L.str2]
76		push rax
77		pop rax
78		mov rax, rax
79		mov rdi, rax
80		# statement primitives do not return results (but will clobber r0/rax)
81		call _prints
82		call _println
83		mov rax, 0
84		mov r13, rax
85		mov rax, 0
86		mov r14, rax
87		jmp .L6
88	.L7:
89		# Stack pointer unchanged, no stack allocated variables
90		mov rax, 0
91		mov r15, rax
92		# Stack pointer unchanged, no stack allocated arguments
93		lea rax, [rip + .L.str3]
94		push rax
95		pop rax
96		mov rax, rax
97		mov rdi, rax
98		# statement primitives do not return results (but will clobber r0/rax)
99		call _prints
100		# Stack pointer unchanged, no stack allocated arguments
101		mov eax, r13d
102		add eax, 1
103		jo _errOverflow
104		movsx rax, eax
105		push rax
106		pop rax
107		mov rax, rax
108		mov rdi, rax
109		# statement primitives do not return results (but will clobber r0/rax)
110		call _printi
111		# Stack pointer unchanged, no stack allocated arguments
112		lea rax, [rip + .L.str4]
113		push rax
114		pop rax
115		mov rax, rax
116		mov rdi, rax
117		# statement primitives do not return results (but will clobber r0/rax)
118		call _prints
119		# Stack pointer unchanged, no stack allocated arguments
120		# load the current value in the destination of the read so it supports defaults
121		mov rax, r15
122		mov rdi, rax
123		call _readi
124		mov r11, rax
125		mov rax, r11
126		mov r15, rax
127		# Stack pointer unchanged, no stack allocated arguments
128		mov rax, r14
129		mov rdi, rax
130		mov rax, r15
131		mov rsi, rax
132		call wacc_insert
133		mov r11, rax
134		# Stack pointer unchanged, no stack allocated arguments
135		mov rax, r11
136		mov r14, rax
137		mov eax, r13d
138		add eax, 1
139		jo _errOverflow
140		movsx rax, eax
141		push rax
142		pop rax
143		mov rax, rax
144		mov r13, rax
145		# Stack pointer unchanged, no stack allocated variables
146	.L6:
147		cmp r13, r12
148		jl .L7
149		# Stack pointer unchanged, no stack allocated arguments
150		lea rax, [rip + .L.str5]
151		push rax
152		pop rax
153		mov rax, rax
154		mov rdi, rax
155		# statement primitives do not return results (but will clobber r0/rax)
156		call _prints
157		# Stack pointer unchanged, no stack allocated arguments
158		mov rax, r14
159		mov rdi, rax
160		call wacc_printTree
161		mov r11, rax
162		# Stack pointer unchanged, no stack allocated arguments
163		mov rax, r11
164		mov r13, rax
165		# Stack pointer unchanged, no stack allocated arguments
166		lea rax, [rip + .L.str6]
167		push rax
168		pop rax
169		mov rax, rax
170		mov rdi, rax
171		# statement primitives do not return results (but will clobber r0/rax)
172		call _prints
173		call _println
174		# Stack pointer unchanged, no stack allocated variables
175		mov rax, 0
176		# pop {rbx, r12, r13, r14, r15}
177		mov rbx, qword ptr [rsp]
178		mov r12, qword ptr [rsp + 8]
179		mov r13, qword ptr [rsp + 16]
180		mov r14, qword ptr [rsp + 24]
181		mov r15, qword ptr [rsp + 32]
182		add rsp, 40
183		pop rbp
184		ret
185	
186	wacc_createNewNode:
187		push rbp
188		# push {r12, r13}
189		sub rsp, 16
190		mov qword ptr [rsp], r12
191		mov qword ptr [rsp + 8], r13
192		mov rbp, rsp
193		# Stack pointer unchanged, no stack allocated variables
194		# push {rdx, rsi, rdi}
195		sub rsp, 24
196		mov qword ptr [rsp], rdx
197		mov qword ptr [rsp + 8], rsi
198		mov qword ptr [rsp + 16], rdi
199		mov edi, 16
200		call _malloc
201		mov r11, rax
202		# pop {rdx, rsi, rdi}
203		mov rdx, qword ptr [rsp]
204		mov rsi, qword ptr [rsp + 8]
205		mov rdi, qword ptr [rsp + 16]
206		add rsp, 24
207		mov rax, rsi
208		mov qword ptr [r11], rax
209		mov rax, rdx
210		mov qword ptr [r11 + 8], rax
211		mov rax, r11
212		mov r12, rax
213		# push {rdx, rsi, rdi}
214		sub rsp, 24
215		mov qword ptr [rsp], rdx
216		mov qword ptr [rsp + 8], rsi
217		mov qword ptr [rsp + 16], rdi
218		mov edi, 16
219		call _malloc
220		mov r11, rax
221		# pop {rdx, rsi, rdi}
222		mov rdx, qword ptr [rsp]
223		mov rsi, qword ptr [rsp + 8]
224		mov rdi, qword ptr [rsp + 16]
225		add rsp, 24
226		mov rax, rdi
227		mov qword ptr [r11], rax
228		mov rax, r12
229		mov qword ptr [r11 + 8], rax
230		mov rax, r11
231		mov r13, rax
232		mov rax, r13
233		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
234		mov rsp, rbp
235		# pop {r12, r13}
236		mov r12, qword ptr [rsp]
237		mov r13, qword ptr [rsp + 8]
238		add rsp, 16
239		pop rbp
240		ret
241		# Stack pointer unchanged, no stack allocated variables
242		# 'ere be dragons: this is 100% dead code, functions always end in returns!
243	
244	wacc_insert:
245		push rbp
246		# push {r12, r13, r14}
247		sub rsp, 24
248		mov qword ptr [rsp], r12
249		mov qword ptr [rsp + 8], r13
250		mov qword ptr [rsp + 16], r14
251		mov rbp, rsp
252		cmp rdi, 0
253		je .L0
254		# Stack pointer unchanged, no stack allocated variables
255		cmp rdi, 0
256		je _errNull
257		mov rax, qword ptr [rdi + 8]
258		mov rax, rax
259		mov r12, rax
260		cmp rdi, 0
261		je _errNull
262		mov rax, qword ptr [rdi]
263		mov rax, rax
264		mov r13, rax
265		mov rax, 0
266		mov r14, rax
267		cmp rsi, r13
268		jl .L2
269		cmp r12, 0
270		je _errNull
271		mov rax, qword ptr [r12 + 8]
272		mov rax, rax
273		push rax
274		pop rax
275		mov rax, rax
276		mov r14, rax
277		# push {rsi, rdi}
278		sub rsp, 16
279		mov qword ptr [rsp], rsi
280		mov qword ptr [rsp + 8], rdi
281		# Set up R11 as a temporary second base pointer for the caller saved things
282		mov r11, rsp
283		# Stack pointer unchanged, no stack allocated arguments
284		mov rax, r14
285		mov rdi, rax
286		mov rax, rsi
287		mov rsi, rax
288		call wacc_insert
289		mov r11, rax
290		# Stack pointer unchanged, no stack allocated arguments
291		# pop {rsi, rdi}
292		mov rsi, qword ptr [rsp]
293		mov rdi, qword ptr [rsp + 8]
294		add rsp, 16
295		cmp r12, 0
296		je _errNull
297		mov rax, r11
298		mov qword ptr [r12 + 8], rax
299		jmp .L3
300	.L2:
301		cmp r12, 0
302		je _errNull
303		mov rax, qword ptr [r12]
304		mov rax, rax
305		push rax
306		pop rax
307		mov rax, rax
308		mov r14, rax
309		# push {rsi, rdi}
310		sub rsp, 16
311		mov qword ptr [rsp], rsi
312		mov qword ptr [rsp + 8], rdi
313		# Set up R11 as a temporary second base pointer for the caller saved things
314		mov r11, rsp
315		# Stack pointer unchanged, no stack allocated arguments
316		mov rax, r14
317		mov rdi, rax
318		mov rax, rsi
319		mov rsi, rax
320		call wacc_insert
321		mov r11, rax
322		# Stack pointer unchanged, no stack allocated arguments
323		# pop {rsi, rdi}
324		mov rsi, qword ptr [rsp]
325		mov rdi, qword ptr [rsp + 8]
326		add rsp, 16
327		cmp r12, 0
328		je _errNull
329		mov rax, r11
330		mov qword ptr [r12], rax
331	.L3:
332		# Stack pointer unchanged, no stack allocated variables
333		jmp .L1
334	.L0:
335		# push {rsi, rdi}
336		sub rsp, 16
337		mov qword ptr [rsp], rsi
338		mov qword ptr [rsp + 8], rdi
339		# Set up R11 as a temporary second base pointer for the caller saved things
340		mov r11, rsp
341		# Stack pointer unchanged, no stack allocated arguments
342		mov rax, rsi
343		mov rdi, rax
344		mov rax, 0
345		mov rsi, rax
346		mov rax, 0
347		mov rdx, rax
348		call wacc_createNewNode
349		mov r11, rax
350		# Stack pointer unchanged, no stack allocated arguments
351		# pop {rsi, rdi}
352		mov rsi, qword ptr [rsp]
353		mov rdi, qword ptr [rsp + 8]
354		add rsp, 16
355		mov rax, r11
356		mov rdi, rax
357	.L1:
358		mov rax, rdi
359		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
360		mov rsp, rbp
361		# pop {r12, r13, r14}
362		mov r12, qword ptr [rsp]
363		mov r13, qword ptr [rsp + 8]
364		mov r14, qword ptr [rsp + 16]
365		add rsp, 24
366		pop rbp
367		ret
368		# 'ere be dragons: this is 100% dead code, functions always end in returns!
369	
370	wacc_printTree:
371		push rbp
372		# push {r12, r13, r14}
373		sub rsp, 24
374		mov qword ptr [rsp], r12
375		mov qword ptr [rsp + 8], r13
376		mov qword ptr [rsp + 16], r14
377		mov rbp, rsp
378		cmp rdi, 0
379		je .L4
380		# Stack pointer unchanged, no stack allocated variables
381		cmp rdi, 0
382		je _errNull
383		mov rax, qword ptr [rdi + 8]
384		mov rax, rax
385		mov r12, rax
386		cmp r12, 0
387		je _errNull
388		mov rax, qword ptr [r12]
389		mov rax, rax
390		mov r13, rax
391		push rdi
392		# Set up R11 as a temporary second base pointer for the caller saved things
393		mov r11, rsp
394		# Stack pointer unchanged, no stack allocated arguments
395		mov rax, r13
396		mov rdi, rax
397		call wacc_printTree
398		mov r11, rax
399		# Stack pointer unchanged, no stack allocated arguments
400		pop rdi
401		mov rax, r11
402		mov r14, rax
403		cmp rdi, 0
404		je _errNull
405		mov rax, qword ptr [rdi]
406		mov rax, rax
407		push rax
408		pop rax
409		mov rax, rax
410		mov r14, rax
411		push rdi
412		# Set up R11 as a temporary second base pointer for the caller saved things
413		mov r11, rsp
414		# Stack pointer unchanged, no stack allocated arguments
415		mov rax, r14
416		mov rdi, rax
417		# statement primitives do not return results (but will clobber r0/rax)
418		call _printi
419		pop rdi
420		push rdi
421		# Set up R11 as a temporary second base pointer for the caller saved things
422		mov r11, rsp
423		# Stack pointer unchanged, no stack allocated arguments
424		mov rax, 32
425		mov rdi, rax
426		# statement primitives do not return results (but will clobber r0/rax)
427		call _printc
428		pop rdi
429		cmp r12, 0
430		je _errNull
431		mov rax, qword ptr [r12 + 8]
432		mov rax, rax
433		push rax
434		pop rax
435		mov rax, rax
436		mov r13, rax
437		push rdi
438		# Set up R11 as a temporary second base pointer for the caller saved things
439		mov r11, rsp
440		# Stack pointer unchanged, no stack allocated arguments
441		mov rax, r13
442		mov rdi, rax
443		call wacc_printTree
444		mov r11, rax
445		# Stack pointer unchanged, no stack allocated arguments
446		pop rdi
447		mov rax, r11
448		mov r14, rax
449		mov rax, 0
450		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
451		mov rsp, rbp
452		# pop {r12, r13, r14}
453		mov r12, qword ptr [rsp]
454		mov r13, qword ptr [rsp + 8]
455		mov r14, qword ptr [rsp + 16]
456		add rsp, 24
457		pop rbp
458		ret
459		# Stack pointer unchanged, no stack allocated variables
460		jmp .L5
461	.L4:
462		mov rax, 0
463		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
464		mov rsp, rbp
465		# pop {r12, r13, r14}
466		mov r12, qword ptr [rsp]
467		mov r13, qword ptr [rsp + 8]
468		mov r14, qword ptr [rsp + 16]
469		add rsp, 24
470		pop rbp
471		ret
472	.L5:
473		# 'ere be dragons: this is 100% dead code, functions always end in returns!
474	
475	.section .rodata
476	# length of .L._prints_str0
477		.int 4
478	.L._prints_str0:
479		.asciz "%.*s"
480	.text
481	_prints:
482		push rbp
483		mov rbp, rsp
484		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
485		and rsp, -16
486		mov rdx, rdi
487		mov esi, dword ptr [rdi - 4]
488		lea rdi, [rip + .L._prints_str0]
489		# on x86, al represents the number of SIMD registers used as variadic arguments
490		mov al, 0
491		call printf@plt
492		mov rdi, 0
493		call fflush@plt
494		mov rsp, rbp
495		pop rbp
496		ret
497	
498	.section .rodata
499	# length of .L._readi_str0
500		.int 2
501	.L._readi_str0:
502		.asciz "%d"
503	.text
504	_readi:
505		push rbp
506		mov rbp, rsp
507		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
508		and rsp, -16
509		# RDI contains the "original" value of the destination of the read
510		# allocate space on the stack to store the read: preserve alignment!
511		# the passed default argument should be stored in case of EOF
512		sub rsp, 16
513		mov dword ptr [rsp], edi
514		lea rsi, qword ptr [rsp]
515		lea rdi, [rip + .L._readi_str0]
516		# on x86, al represents the number of SIMD registers used as variadic arguments
517		mov al, 0
518		call scanf@plt
519		movsx rax, dword ptr [rsp]
520		add rsp, 16
521		mov rsp, rbp
522		pop rbp
523		ret
524	
525	_malloc:
526		push rbp
527		mov rbp, rsp
528		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
529		and rsp, -16
530		call malloc@plt
531		cmp rax, 0
532		je _errOutOfMemory
533		mov rsp, rbp
534		pop rbp
535		ret
536	
537	.section .rodata
538	# length of .L._printi_str0
539		.int 2
540	.L._printi_str0:
541		.asciz "%d"
542	.text
543	_printi:
544		push rbp
545		mov rbp, rsp
546		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
547		and rsp, -16
548		mov esi, edi
549		lea rdi, [rip + .L._printi_str0]
550		# on x86, al represents the number of SIMD registers used as variadic arguments
551		mov al, 0
552		call printf@plt
553		mov rdi, 0
554		call fflush@plt
555		mov rsp, rbp
556		pop rbp
557		ret
558	
559	.section .rodata
560	# length of .L._printc_str0
561		.int 2
562	.L._printc_str0:
563		.asciz "%c"
564	.text
565	_printc:
566		push rbp
567		mov rbp, rsp
568		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
569		and rsp, -16
570		mov sil, dil
571		lea rdi, [rip + .L._printc_str0]
572		# on x86, al represents the number of SIMD registers used as variadic arguments
573		mov al, 0
574		call printf@plt
575		mov rdi, 0
576		call fflush@plt
577		mov rsp, rbp
578		pop rbp
579		ret
580	
581	.section .rodata
582	# length of .L._println_str0
583		.int 0
584	.L._println_str0:
585		.asciz ""
586	.text
587	_println:
588		push rbp
589		mov rbp, rsp
590		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
591		and rsp, -16
592		lea rdi, [rip + .L._println_str0]
593		call puts@plt
594		mov rdi, 0
595		call fflush@plt
596		mov rsp, rbp
597		pop rbp
598		ret
599	
600	_exit:
601		push rbp
602		mov rbp, rsp
603		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
604		and rsp, -16
605		call exit@plt
606		mov rsp, rbp
607		pop rbp
608		ret
609	
610	.section .rodata
611	# length of .L._errOutOfMemory_str0
612		.int 27
613	.L._errOutOfMemory_str0:
614		.asciz "fatal error: out of memory\n"
615	.text
616	_errOutOfMemory:
617		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
618		and rsp, -16
619		lea rdi, [rip + .L._errOutOfMemory_str0]
620		call _prints
621		mov dil, -1
622		call exit@plt
623	
624	.section .rodata
625	# length of .L._errNull_str0
626		.int 45
627	.L._errNull_str0:
628		.asciz "fatal error: null pair dereferenced or freed\n"
629	.text
630	_errNull:
631		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
632		and rsp, -16
633		lea rdi, [rip + .L._errNull_str0]
634		call _prints
635		mov dil, -1
636		call exit@plt
637	
638	.section .rodata
639	# length of .L._errOverflow_str0
640		.int 52
641	.L._errOverflow_str0:
642		.asciz "fatal error: integer overflow or underflow occurred\n"
643	.text
644	_errOverflow:
645		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
646		and rsp, -16
647		lea rdi, [rip + .L._errOverflow_str0]
648		call _prints
649		mov dil, -1
650		call exit@plt
===========================================================
-- Finished

