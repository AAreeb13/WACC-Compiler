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
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 5
4	.L.str0:
5		.asciz "a is "
6	# length of .L.str1
7		.int 5
8	.L.str1:
9		.asciz "b is "
10	# length of .L.str2
11		.int 5
12	.L.str2:
13		.asciz "c is "
14	# length of .L.str3
15		.int 5
16	.L.str3:
17		.asciz "d is "
18	# length of .L.str4
19		.int 5
20	.L.str4:
21		.asciz "e is "
22	# length of .L.str5
23		.int 5
24	.L.str5:
25		.asciz "f is "
26	# length of .L.str6
27		.int 5
28	.L.str6:
29		.asciz "hello"
30	# length of .L.str7
31		.int 10
32	.L.str7:
33		.asciz "answer is "
34	.text
35	main:
36		pushq %rbp
37		# pushq {%rbx, %r12, %r13, %r14}
38		subq $32, %rsp
39		movq %rbx, (%rsp)
40		movq %r12, 8(%rsp)
41		movq %r13, 16(%rsp)
42		movq %r14, 24(%rsp)
43		movq %rsp, %rbp
44		# Stack pointer unchanged, no stack allocated variables
45		# 2 element array
46		movl $6, %edi
47		call _malloc
48		movq %rax, %r11
49		# array pointers are shifted forwards by 4 bytes (to account for size)
50		movq %r11, %r11
51		addq $4, %r11
52		movq $2, %rax
53		movl %eax, -4(%r11)
54		movq $0, %rax
55		movb %al, (%r11)
56		movq $1, %rax
57		movb %al, 1(%r11)
58		movq %r11, %rax
59		movq %rax, %r12
60		# 2 element array
61		movl $12, %edi
62		call _malloc
63		movq %rax, %r11
64		# array pointers are shifted forwards by 4 bytes (to account for size)
65		movq %r11, %r11
66		addq $4, %r11
67		movq $2, %rax
68		movl %eax, -4(%r11)
69		movq $1, %rax
70		movl %eax, (%r11)
71		movq $2, %rax
72		movl %eax, 4(%r11)
73		movq %r11, %rax
74		movq %rax, %r13
75		# Stack pointer unchanged, no stack allocated arguments
76		movq $42, %rax
77		movq %rax, %rdi
78		movq $1, %rax
79		movq %rax, %rsi
80		movq $117, %rax
81		movq %rax, %rdx
82		leaq .L.str6(%rip), %rax
83		pushq %rax
84		popq %rax
85		movq %rax, %rax
86		movq %rax, %rcx
87		movq %r12, %rax
88		movq %rax, %r8
89		movq %r13, %rax
90		movq %rax, %r9
91		call wacc_doSomething
92		movq %rax, %r11
93		# Stack pointer unchanged, no stack allocated arguments
94		movq %r11, %rax
95		movq %rax, %r14
96		# Stack pointer unchanged, no stack allocated arguments
97		leaq .L.str7(%rip), %rax
98		pushq %rax
99		popq %rax
100		movq %rax, %rax
101		movq %rax, %rdi
102		# statement primitives do not return results (but will clobber r0/rax)
103		call _prints
104		# Stack pointer unchanged, no stack allocated arguments
105		movq %r14, %rax
106		movq %rax, %rdi
107		# statement primitives do not return results (but will clobber r0/rax)
108		call _printc
109		call _println
110		# Stack pointer unchanged, no stack allocated variables
111		movq $0, %rax
112		# popq {%rbx, %r12, %r13, %r14}
113		movq (%rsp), %rbx
114		movq 8(%rsp), %r12
115		movq 16(%rsp), %r13
116		movq 24(%rsp), %r14
117		addq $32, %rsp
118		popq %rbp
119		ret
120	
121	wacc_doSomething:
122		pushq %rbp
123		movq %rsp, %rbp
124		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
125		subq $48, %rsp
126		movq %rcx, (%rsp)
127		movq %rdx, 8(%rsp)
128		movq %rsi, 16(%rsp)
129		movq %rdi, 24(%rsp)
130		movq %r8, 32(%rsp)
131		movq %r9, 40(%rsp)
132		# Set up R11 as a temporary second base pointer for the caller saved things
133		movq %rsp, %r11
134		# Stack pointer unchanged, no stack allocated arguments
135		leaq .L.str0(%rip), %rax
136		pushq %rax
137		popq %rax
138		movq %rax, %rax
139		movq %rax, %rdi
140		# statement primitives do not return results (but will clobber r0/rax)
141		call _prints
142		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
143		movq (%rsp), %rcx
144		movq 8(%rsp), %rdx
145		movq 16(%rsp), %rsi
146		movq 24(%rsp), %rdi
147		movq 32(%rsp), %r8
148		movq 40(%rsp), %r9
149		addq $48, %rsp
150		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
151		subq $48, %rsp
152		movq %rcx, (%rsp)
153		movq %rdx, 8(%rsp)
154		movq %rsi, 16(%rsp)
155		movq %rdi, 24(%rsp)
156		movq %r8, 32(%rsp)
157		movq %r9, 40(%rsp)
158		# Set up R11 as a temporary second base pointer for the caller saved things
159		movq %rsp, %r11
160		# Stack pointer unchanged, no stack allocated arguments
161		movq %rdi, %rax
162		movq %rax, %rdi
163		# statement primitives do not return results (but will clobber r0/rax)
164		call _printi
165		call _println
166		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
167		movq (%rsp), %rcx
168		movq 8(%rsp), %rdx
169		movq 16(%rsp), %rsi
170		movq 24(%rsp), %rdi
171		movq 32(%rsp), %r8
172		movq 40(%rsp), %r9
173		addq $48, %rsp
174		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
175		subq $48, %rsp
176		movq %rcx, (%rsp)
177		movq %rdx, 8(%rsp)
178		movq %rsi, 16(%rsp)
179		movq %rdi, 24(%rsp)
180		movq %r8, 32(%rsp)
181		movq %r9, 40(%rsp)
182		# Set up R11 as a temporary second base pointer for the caller saved things
183		movq %rsp, %r11
184		# Stack pointer unchanged, no stack allocated arguments
185		leaq .L.str1(%rip), %rax
186		pushq %rax
187		popq %rax
188		movq %rax, %rax
189		movq %rax, %rdi
190		# statement primitives do not return results (but will clobber r0/rax)
191		call _prints
192		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
193		movq (%rsp), %rcx
194		movq 8(%rsp), %rdx
195		movq 16(%rsp), %rsi
196		movq 24(%rsp), %rdi
197		movq 32(%rsp), %r8
198		movq 40(%rsp), %r9
199		addq $48, %rsp
200		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
201		subq $48, %rsp
202		movq %rcx, (%rsp)
203		movq %rdx, 8(%rsp)
204		movq %rsi, 16(%rsp)
205		movq %rdi, 24(%rsp)
206		movq %r8, 32(%rsp)
207		movq %r9, 40(%rsp)
208		# Set up R11 as a temporary second base pointer for the caller saved things
209		movq %rsp, %r11
210		# Stack pointer unchanged, no stack allocated arguments
211		movq %rsi, %rax
212		movq %rax, %rdi
213		# statement primitives do not return results (but will clobber r0/rax)
214		call _printb
215		call _println
216		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
217		movq (%rsp), %rcx
218		movq 8(%rsp), %rdx
219		movq 16(%rsp), %rsi
220		movq 24(%rsp), %rdi
221		movq 32(%rsp), %r8
222		movq 40(%rsp), %r9
223		addq $48, %rsp
224		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
225		subq $48, %rsp
226		movq %rcx, (%rsp)
227		movq %rdx, 8(%rsp)
228		movq %rsi, 16(%rsp)
229		movq %rdi, 24(%rsp)
230		movq %r8, 32(%rsp)
231		movq %r9, 40(%rsp)
232		# Set up R11 as a temporary second base pointer for the caller saved things
233		movq %rsp, %r11
234		# Stack pointer unchanged, no stack allocated arguments
235		leaq .L.str2(%rip), %rax
236		pushq %rax
237		popq %rax
238		movq %rax, %rax
239		movq %rax, %rdi
240		# statement primitives do not return results (but will clobber r0/rax)
241		call _prints
242		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
243		movq (%rsp), %rcx
244		movq 8(%rsp), %rdx
245		movq 16(%rsp), %rsi
246		movq 24(%rsp), %rdi
247		movq 32(%rsp), %r8
248		movq 40(%rsp), %r9
249		addq $48, %rsp
250		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
251		subq $48, %rsp
252		movq %rcx, (%rsp)
253		movq %rdx, 8(%rsp)
254		movq %rsi, 16(%rsp)
255		movq %rdi, 24(%rsp)
256		movq %r8, 32(%rsp)
257		movq %r9, 40(%rsp)
258		# Set up R11 as a temporary second base pointer for the caller saved things
259		movq %rsp, %r11
260		# Stack pointer unchanged, no stack allocated arguments
261		movq %rdx, %rax
262		movq %rax, %rdi
263		# statement primitives do not return results (but will clobber r0/rax)
264		call _printc
265		call _println
266		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
267		movq (%rsp), %rcx
268		movq 8(%rsp), %rdx
269		movq 16(%rsp), %rsi
270		movq 24(%rsp), %rdi
271		movq 32(%rsp), %r8
272		movq 40(%rsp), %r9
273		addq $48, %rsp
274		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
275		subq $48, %rsp
276		movq %rcx, (%rsp)
277		movq %rdx, 8(%rsp)
278		movq %rsi, 16(%rsp)
279		movq %rdi, 24(%rsp)
280		movq %r8, 32(%rsp)
281		movq %r9, 40(%rsp)
282		# Set up R11 as a temporary second base pointer for the caller saved things
283		movq %rsp, %r11
284		# Stack pointer unchanged, no stack allocated arguments
285		leaq .L.str3(%rip), %rax
286		pushq %rax
287		popq %rax
288		movq %rax, %rax
289		movq %rax, %rdi
290		# statement primitives do not return results (but will clobber r0/rax)
291		call _prints
292		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
293		movq (%rsp), %rcx
294		movq 8(%rsp), %rdx
295		movq 16(%rsp), %rsi
296		movq 24(%rsp), %rdi
297		movq 32(%rsp), %r8
298		movq 40(%rsp), %r9
299		addq $48, %rsp
300		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
301		subq $48, %rsp
302		movq %rcx, (%rsp)
303		movq %rdx, 8(%rsp)
304		movq %rsi, 16(%rsp)
305		movq %rdi, 24(%rsp)
306		movq %r8, 32(%rsp)
307		movq %r9, 40(%rsp)
308		# Set up R11 as a temporary second base pointer for the caller saved things
309		movq %rsp, %r11
310		# Stack pointer unchanged, no stack allocated arguments
311		movq %rcx, %rax
312		movq %rax, %rdi
313		# statement primitives do not return results (but will clobber r0/rax)
314		call _prints
315		call _println
316		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
317		movq (%rsp), %rcx
318		movq 8(%rsp), %rdx
319		movq 16(%rsp), %rsi
320		movq 24(%rsp), %rdi
321		movq 32(%rsp), %r8
322		movq 40(%rsp), %r9
323		addq $48, %rsp
324		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
325		subq $48, %rsp
326		movq %rcx, (%rsp)
327		movq %rdx, 8(%rsp)
328		movq %rsi, 16(%rsp)
329		movq %rdi, 24(%rsp)
330		movq %r8, 32(%rsp)
331		movq %r9, 40(%rsp)
332		# Set up R11 as a temporary second base pointer for the caller saved things
333		movq %rsp, %r11
334		# Stack pointer unchanged, no stack allocated arguments
335		leaq .L.str4(%rip), %rax
336		pushq %rax
337		popq %rax
338		movq %rax, %rax
339		movq %rax, %rdi
340		# statement primitives do not return results (but will clobber r0/rax)
341		call _prints
342		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
343		movq (%rsp), %rcx
344		movq 8(%rsp), %rdx
345		movq 16(%rsp), %rsi
346		movq 24(%rsp), %rdi
347		movq 32(%rsp), %r8
348		movq 40(%rsp), %r9
349		addq $48, %rsp
350		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
351		subq $48, %rsp
352		movq %rcx, (%rsp)
353		movq %rdx, 8(%rsp)
354		movq %rsi, 16(%rsp)
355		movq %rdi, 24(%rsp)
356		movq %r8, 32(%rsp)
357		movq %r9, 40(%rsp)
358		# Set up R11 as a temporary second base pointer for the caller saved things
359		movq %rsp, %r11
360		# Stack pointer unchanged, no stack allocated arguments
361		movq %r8, %rax
362		movq %rax, %rdi
363		# statement primitives do not return results (but will clobber r0/rax)
364		call _printp
365		call _println
366		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
367		movq (%rsp), %rcx
368		movq 8(%rsp), %rdx
369		movq 16(%rsp), %rsi
370		movq 24(%rsp), %rdi
371		movq 32(%rsp), %r8
372		movq 40(%rsp), %r9
373		addq $48, %rsp
374		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
375		subq $48, %rsp
376		movq %rcx, (%rsp)
377		movq %rdx, 8(%rsp)
378		movq %rsi, 16(%rsp)
379		movq %rdi, 24(%rsp)
380		movq %r8, 32(%rsp)
381		movq %r9, 40(%rsp)
382		# Set up R11 as a temporary second base pointer for the caller saved things
383		movq %rsp, %r11
384		# Stack pointer unchanged, no stack allocated arguments
385		leaq .L.str5(%rip), %rax
386		pushq %rax
387		popq %rax
388		movq %rax, %rax
389		movq %rax, %rdi
390		# statement primitives do not return results (but will clobber r0/rax)
391		call _prints
392		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
393		movq (%rsp), %rcx
394		movq 8(%rsp), %rdx
395		movq 16(%rsp), %rsi
396		movq 24(%rsp), %rdi
397		movq 32(%rsp), %r8
398		movq 40(%rsp), %r9
399		addq $48, %rsp
400		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
401		subq $48, %rsp
402		movq %rcx, (%rsp)
403		movq %rdx, 8(%rsp)
404		movq %rsi, 16(%rsp)
405		movq %rdi, 24(%rsp)
406		movq %r8, 32(%rsp)
407		movq %r9, 40(%rsp)
408		# Set up R11 as a temporary second base pointer for the caller saved things
409		movq %rsp, %r11
410		# Stack pointer unchanged, no stack allocated arguments
411		movq %r9, %rax
412		movq %rax, %rdi
413		# statement primitives do not return results (but will clobber r0/rax)
414		call _printp
415		call _println
416		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
417		movq (%rsp), %rcx
418		movq 8(%rsp), %rdx
419		movq 16(%rsp), %rsi
420		movq 24(%rsp), %rdi
421		movq 32(%rsp), %r8
422		movq 40(%rsp), %r9
423		addq $48, %rsp
424		movq $103, %rax
425		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
426		movq %rbp, %rsp
427		popq %rbp
428		ret
429		# 'ere be dragons: this is 100% dead code, functions always end in returns!
430	
431	.section .rodata
432	# length of .L._prints_str0
433		.int 4
434	.L._prints_str0:
435		.asciz "%.*s"
436	.text
437	_prints:
438		pushq %rbp
439		movq %rsp, %rbp
440		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
441		andq $-16, %rsp
442		movq %rdi, %rdx
443		movl -4(%rdi), %esi
444		leaq .L._prints_str0(%rip), %rdi
445		# on x86, al represents the number of SIMD registers used as variadic arguments
446		movb $0, %al
447		call printf@plt
448		movq $0, %rdi
449		call fflush@plt
450		movq %rbp, %rsp
451		popq %rbp
452		ret
453	
454	.section .rodata
455	# length of .L._printp_str0
456		.int 2
457	.L._printp_str0:
458		.asciz "%p"
459	.text
460	_printp:
461		pushq %rbp
462		movq %rsp, %rbp
463		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
464		andq $-16, %rsp
465		movq %rdi, %rsi
466		leaq .L._printp_str0(%rip), %rdi
467		# on x86, al represents the number of SIMD registers used as variadic arguments
468		movb $0, %al
469		call printf@plt
470		movq $0, %rdi
471		call fflush@plt
472		movq %rbp, %rsp
473		popq %rbp
474		ret
475	
476	_malloc:
477		pushq %rbp
478		movq %rsp, %rbp
479		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
480		andq $-16, %rsp
481		call malloc@plt
482		cmpq $0, %rax
483		je _errOutOfMemory
484		movq %rbp, %rsp
485		popq %rbp
486		ret
487	
488	.section .rodata
489	# length of .L._printi_str0
490		.int 2
491	.L._printi_str0:
492		.asciz "%d"
493	.text
494	_printi:
495		pushq %rbp
496		movq %rsp, %rbp
497		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
498		andq $-16, %rsp
499		movl %edi, %esi
500		leaq .L._printi_str0(%rip), %rdi
501		# on x86, al represents the number of SIMD registers used as variadic arguments
502		movb $0, %al
503		call printf@plt
504		movq $0, %rdi
505		call fflush@plt
506		movq %rbp, %rsp
507		popq %rbp
508		ret
509	
510	.section .rodata
511	# length of .L._printb_str0
512		.int 5
513	.L._printb_str0:
514		.asciz "false"
515	# length of .L._printb_str1
516		.int 4
517	.L._printb_str1:
518		.asciz "true"
519	# length of .L._printb_str2
520		.int 4
521	.L._printb_str2:
522		.asciz "%.*s"
523	.text
524	_printb:
525		pushq %rbp
526		movq %rsp, %rbp
527		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
528		andq $-16, %rsp
529		cmpb $0, %dil
530		jne .L_printb0
531		leaq .L._printb_str0(%rip), %rdx
532		jmp .L_printb1
533	.L_printb0:
534		leaq .L._printb_str1(%rip), %rdx
535	.L_printb1:
536		movl -4(%rdx), %esi
537		leaq .L._printb_str2(%rip), %rdi
538		# on x86, al represents the number of SIMD registers used as variadic arguments
539		movb $0, %al
540		call printf@plt
541		movq $0, %rdi
542		call fflush@plt
543		movq %rbp, %rsp
544		popq %rbp
545		ret
546	
547	.section .rodata
548	# length of .L._printc_str0
549		.int 2
550	.L._printc_str0:
551		.asciz "%c"
552	.text
553	_printc:
554		pushq %rbp
555		movq %rsp, %rbp
556		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
557		andq $-16, %rsp
558		movb %dil, %sil
559		leaq .L._printc_str0(%rip), %rdi
560		# on x86, al represents the number of SIMD registers used as variadic arguments
561		movb $0, %al
562		call printf@plt
563		movq $0, %rdi
564		call fflush@plt
565		movq %rbp, %rsp
566		popq %rbp
567		ret
568	
569	.section .rodata
570	# length of .L._println_str0
571		.int 0
572	.L._println_str0:
573		.asciz ""
574	.text
575	_println:
576		pushq %rbp
577		movq %rsp, %rbp
578		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
579		andq $-16, %rsp
580		leaq .L._println_str0(%rip), %rdi
581		call puts@plt
582		movq $0, %rdi
583		call fflush@plt
584		movq %rbp, %rsp
585		popq %rbp
586		ret
587	
588	_exit:
589		pushq %rbp
590		movq %rsp, %rbp
591		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
592		andq $-16, %rsp
593		call exit@plt
594		movq %rbp, %rsp
595		popq %rbp
596		ret
597	
598	.section .rodata
599	# length of .L._errOutOfMemory_str0
600		.int 27
601	.L._errOutOfMemory_str0:
602		.asciz "fatal error: out of memory\n"
603	.text
604	_errOutOfMemory:
605		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
606		andq $-16, %rsp
607		leaq .L._errOutOfMemory_str0(%rip), %rdi
608		call _prints
609		movb $-1, %dil
610		call exit@plt
===========================================================
-- Finished

