./valid/function/nested_functions/fixedPointRealArithmetic.wacc
calling the reference compiler on ./valid/function/nested_functions/fixedPointRealArithmetic.wacc
-- Test: fixedPointRealArithmetic.wacc

-- Uploaded file: 
---------------------------------------------------------------
# This program implements floating-point type using integers.
# The details about how it is done can found here:
# http://www.cse.iitd.ernet.in/~sbansal/csl373/pintos/doc/pintos_7.html#SEC135
#
# Basically, our integer have 32 bits. We use the first bit for sign, the next
# 17 bits for value above the decimal digit and the last 14 bits for the values
# after the decimal digits.
#
# We call the number 17 above p, and the number 14 above q.
# We have f = 2**q.
#

# Output:
# Using fixed-point real: 10 / 3 * 3 = 10
#

# Program:

begin
    # Returns the number of bits behind the decimal points.
    int q() is
        return 14
    end

    # Because we do not have bitwise shit in the language, we have to calculate it manually.
    int power(int base, int amount) is
        int result = 1 ;
        while amount > 0 do
            result = result * base ;
            amount = amount - 1
        done ;
        return result
    end

    int f() is
        int qq = call q() ;
        # f = 2**q
        int f = call power(2, qq) ;
        return f
    end

    # The implementation of the following functions are translated from the URI above.
    # Arguments start with 'x' have type fixed-point. Those start with 'n' have type integer.

    int intToFixedPoint(int n) is
        int ff = call f() ;
        return n * ff
    end

    int fixedPointToIntRoundDown(int x) is
        int ff = call f() ;
        return x / ff
    end

    int fixedPointToIntRoundNear(int x) is
        int ff = call f() ;
        if x >= 0
        then
            return (x + ff / 2) / ff
        else
            return (x - ff / 2) / ff
        fi
    end

    int add(int x1, int x2) is
        return x1 + x2
    end

    int subtract(int x1, int x2) is
        return x1 - x2
    end

    int addByInt(int x, int n) is
        int ff = call f() ;
        return x + n * ff
    end

    int subtractByInt(int x, int n) is
        int ff = call f() ;
        return x - n * ff
    end

    int multiply(int x1, int x2) is
        # We don't have int_64 in our language so we just ignore the overflow
        int ff = call f() ;
        return x1 * x2 / ff
    end

    int multiplyByInt(int x, int n) is
        return x * n
    end

    int divide(int x1, int x2) is
        # We don't have int_64 in our language so we just ignore the overflow
        int ff = call f() ;
        return x1 * ff / x2
    end

    int divideByInt(int x, int n) is
        return x / n
    end

    # Main function
    int n1 = 10 ;
    int n2 = 3 ;

    print "Using fixed-point real: " ;
    print n1 ;
    print " / " ;
    print n2 ;
    print " * " ;
    print n2 ;
    print " = " ;

    int x = call intToFixedPoint(n1) ;
    x = call divideByInt(x, n2) ;
    x = call multiplyByInt(x, n2) ;
    int result = call fixedPointToIntRoundNear(x) ;
    println result
end



---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
fixedPointRealArithmetic.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 24
3	.L.str0:
4		.asciz "Using fixed-point real: "
5	@ length of .L.str1
6		.word 3
7	.L.str1:
8		.asciz " / "
9	@ length of .L.str2
10		.word 3
11	.L.str2:
12		.asciz " * "
13	@ length of .L.str3
14		.word 3
15	.L.str3:
16		.asciz " = "
17	.align 4
18	.text
19	.global main
20	main:
21		push {fp, lr}
22		push {r4, r5, r6, r7, r8, r10, r12}
23		mov fp, sp
24		@ Stack pointer unchanged, no stack allocated variables
25		mov r8, #10
26		mov r4, r8
27		mov r8, #3
28		mov r5, r8
29		@ Stack pointer unchanged, no stack allocated arguments
30		ldr r8, =.L.str0
31		push {r8}
32		pop {r8}
33		mov r8, r8
34		mov r0, r8
35		@ statement primitives do not return results (but will clobber r0/rax)
36		bl _prints
37		@ Stack pointer unchanged, no stack allocated arguments
38		mov r8, r4
39		mov r0, r8
40		@ statement primitives do not return results (but will clobber r0/rax)
41		bl _printi
42		@ Stack pointer unchanged, no stack allocated arguments
43		ldr r8, =.L.str1
44		push {r8}
45		pop {r8}
46		mov r8, r8
47		mov r0, r8
48		@ statement primitives do not return results (but will clobber r0/rax)
49		bl _prints
50		@ Stack pointer unchanged, no stack allocated arguments
51		mov r8, r5
52		mov r0, r8
53		@ statement primitives do not return results (but will clobber r0/rax)
54		bl _printi
55		@ Stack pointer unchanged, no stack allocated arguments
56		ldr r8, =.L.str2
57		push {r8}
58		pop {r8}
59		mov r8, r8
60		mov r0, r8
61		@ statement primitives do not return results (but will clobber r0/rax)
62		bl _prints
63		@ Stack pointer unchanged, no stack allocated arguments
64		mov r8, r5
65		mov r0, r8
66		@ statement primitives do not return results (but will clobber r0/rax)
67		bl _printi
68		@ Stack pointer unchanged, no stack allocated arguments
69		ldr r8, =.L.str3
70		push {r8}
71		pop {r8}
72		mov r8, r8
73		mov r0, r8
74		@ statement primitives do not return results (but will clobber r0/rax)
75		bl _prints
76		@ Stack pointer unchanged, no stack allocated arguments
77		mov r8, r4
78		mov r0, r8
79		bl wacc_intToFixedPoint
80		mov r12, r0
81		@ Stack pointer unchanged, no stack allocated arguments
82		mov r8, r12
83		mov r6, r8
84		@ Stack pointer unchanged, no stack allocated arguments
85		mov r8, r6
86		mov r0, r8
87		mov r8, r5
88		mov r1, r8
89		bl wacc_divideByInt
90		mov r12, r0
91		@ Stack pointer unchanged, no stack allocated arguments
92		mov r8, r12
93		mov r6, r8
94		@ Stack pointer unchanged, no stack allocated arguments
95		mov r8, r6
96		mov r0, r8
97		mov r8, r5
98		mov r1, r8
99		bl wacc_multiplyByInt
100		mov r12, r0
101		@ Stack pointer unchanged, no stack allocated arguments
102		mov r8, r12
103		mov r6, r8
104		@ Stack pointer unchanged, no stack allocated arguments
105		mov r8, r6
106		mov r0, r8
107		bl wacc_fixedPointToIntRoundNear
108		mov r12, r0
109		@ Stack pointer unchanged, no stack allocated arguments
110		mov r8, r12
111		mov r7, r8
112		@ Stack pointer unchanged, no stack allocated arguments
113		mov r8, r7
114		mov r0, r8
115		@ statement primitives do not return results (but will clobber r0/rax)
116		bl _printi
117		bl _println
118		@ Stack pointer unchanged, no stack allocated variables
119		mov r0, #0
120		pop {r4, r5, r6, r7, r8, r10, r12}
121		pop {fp, pc}
122	
123	wacc_q:
124		push {fp, lr}
125		mov fp, sp
126		mov r0, #14
127		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
128		mov sp, fp
129		pop {fp, pc}
130		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
131		@ the .ltorg here is used to generate the constant pool after every function
132		@ this is important for ensuring string literals and large integers are accessible!
133		.ltorg
134	
135	wacc_power:
136		push {fp, lr}
137		push {r4}
138		mov fp, sp
139		@ Stack pointer unchanged, no stack allocated variables
140		mov r8, #1
141		mov r4, r8
142		b .L0
143	.L1:
144		smull r8, r9, r4, r0
145		@ Compare the sign of the lowest bits of the multiplication against the highest
146		cmp r9, r8, asr #31
147		blne _errOverflow
148		push {r8}
149		pop {r8}
150		mov r8, r8
151		mov r4, r8
152		subs r8, r1, #1
153		blvs _errOverflow
154		push {r8}
155		pop {r8}
156		mov r8, r8
157		mov r1, r8
158	.L0:
159		cmp r1, #0
160		bgt .L1
161		mov r0, r4
162		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
163		mov sp, fp
164		pop {r4}
165		pop {fp, pc}
166		@ Stack pointer unchanged, no stack allocated variables
167		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
168		@ the .ltorg here is used to generate the constant pool after every function
169		@ this is important for ensuring string literals and large integers are accessible!
170		.ltorg
171	
172	wacc_f:
173		push {fp, lr}
174		push {r4, r5}
175		mov fp, sp
176		@ Stack pointer unchanged, no stack allocated variables
177		@ Stack pointer unchanged, no stack allocated arguments
178		bl wacc_q
179		mov r12, r0
180		@ Stack pointer unchanged, no stack allocated arguments
181		mov r8, r12
182		mov r4, r8
183		@ Stack pointer unchanged, no stack allocated arguments
184		mov r8, #2
185		mov r0, r8
186		mov r8, r4
187		mov r1, r8
188		bl wacc_power
189		mov r12, r0
190		@ Stack pointer unchanged, no stack allocated arguments
191		mov r8, r12
192		mov r5, r8
193		mov r0, r5
194		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
195		mov sp, fp
196		pop {r4, r5}
197		pop {fp, pc}
198		@ Stack pointer unchanged, no stack allocated variables
199		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
200		@ the .ltorg here is used to generate the constant pool after every function
201		@ this is important for ensuring string literals and large integers are accessible!
202		.ltorg
203	
204	wacc_intToFixedPoint:
205		push {fp, lr}
206		push {r4}
207		mov fp, sp
208		@ Stack pointer unchanged, no stack allocated variables
209		push {r0}
210		@ Set up R12 as a temporary second base pointer for the caller saved things
211		mov r12, sp
212		@ Stack pointer unchanged, no stack allocated arguments
213		bl wacc_f
214		mov r12, r0
215		@ Stack pointer unchanged, no stack allocated arguments
216		pop {r0}
217		mov r8, r12
218		mov r4, r8
219		smull r8, r9, r0, r4
220		@ Compare the sign of the lowest bits of the multiplication against the highest
221		cmp r9, r8, asr #31
222		blne _errOverflow
223		push {r8}
224		pop {r0}
225		mov r0, r0
226		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
227		mov sp, fp
228		pop {r4}
229		pop {fp, pc}
230		@ Stack pointer unchanged, no stack allocated variables
231		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
232		@ the .ltorg here is used to generate the constant pool after every function
233		@ this is important for ensuring string literals and large integers are accessible!
234		.ltorg
235	
236	wacc_fixedPointToIntRoundDown:
237		push {fp, lr}
238		push {r4}
239		mov fp, sp
240		@ Stack pointer unchanged, no stack allocated variables
241		push {r0}
242		@ Set up R12 as a temporary second base pointer for the caller saved things
243		mov r12, sp
244		@ Stack pointer unchanged, no stack allocated arguments
245		bl wacc_f
246		mov r12, r0
247		@ Stack pointer unchanged, no stack allocated arguments
248		pop {r0}
249		mov r8, r12
250		mov r4, r8
251		push {r0}
252		mov r0, r0
253		mov r1, r4
254		cmp r1, #0
255		bleq _errDivZero
256		bl __aeabi_idivmod
257		mov r12, r0
258		pop {r0}
259		mov r8, r12
260		push {r8}
261		pop {r0}
262		mov r0, r0
263		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
264		mov sp, fp
265		pop {r4}
266		pop {fp, pc}
267		@ Stack pointer unchanged, no stack allocated variables
268		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
269		@ the .ltorg here is used to generate the constant pool after every function
270		@ this is important for ensuring string literals and large integers are accessible!
271		.ltorg
272	
273	wacc_fixedPointToIntRoundNear:
274		push {fp, lr}
275		push {r4}
276		mov fp, sp
277		@ Stack pointer unchanged, no stack allocated variables
278		push {r0}
279		@ Set up R12 as a temporary second base pointer for the caller saved things
280		mov r12, sp
281		@ Stack pointer unchanged, no stack allocated arguments
282		bl wacc_f
283		mov r12, r0
284		@ Stack pointer unchanged, no stack allocated arguments
285		pop {r0}
286		mov r8, r12
287		mov r4, r8
288		cmp r0, #0
289		bge .L2
290		push {r0}
291		mov r0, r4
292		mov r1, #2
293		cmp r1, #0
294		bleq _errDivZero
295		bl __aeabi_idivmod
296		mov r12, r0
297		pop {r0}
298		mov r8, r12
299		push {r8}
300		pop {r9}
301		subs r8, r0, r9
302		blvs _errOverflow
303		push {r8}
304		pop {r8}
305		push {r0}
306		mov r0, r8
307		mov r1, r4
308		cmp r1, #0
309		bleq _errDivZero
310		bl __aeabi_idivmod
311		mov r12, r0
312		pop {r0}
313		mov r8, r12
314		push {r8}
315		pop {r0}
316		mov r0, r0
317		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
318		mov sp, fp
319		pop {r4}
320		pop {fp, pc}
321		b .L3
322	.L2:
323		push {r0}
324		mov r0, r4
325		mov r1, #2
326		cmp r1, #0
327		bleq _errDivZero
328		bl __aeabi_idivmod
329		mov r12, r0
330		pop {r0}
331		mov r8, r12
332		push {r8}
333		pop {r9}
334		adds r8, r0, r9
335		blvs _errOverflow
336		push {r8}
337		pop {r8}
338		push {r0}
339		mov r0, r8
340		mov r1, r4
341		cmp r1, #0
342		bleq _errDivZero
343		bl __aeabi_idivmod
344		mov r12, r0
345		pop {r0}
346		mov r8, r12
347		push {r8}
348		pop {r0}
349		mov r0, r0
350		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
351		mov sp, fp
352		pop {r4}
353		pop {fp, pc}
354	.L3:
355		@ Stack pointer unchanged, no stack allocated variables
356		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
357		@ the .ltorg here is used to generate the constant pool after every function
358		@ this is important for ensuring string literals and large integers are accessible!
359		.ltorg
360	
361	wacc_add:
362		push {fp, lr}
363		mov fp, sp
364		adds r8, r0, r1
365		blvs _errOverflow
366		push {r8}
367		pop {r0}
368		mov r0, r0
369		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
370		mov sp, fp
371		pop {fp, pc}
372		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
373		@ the .ltorg here is used to generate the constant pool after every function
374		@ this is important for ensuring string literals and large integers are accessible!
375		.ltorg
376	
377	wacc_subtract:
378		push {fp, lr}
379		mov fp, sp
380		subs r8, r0, r1
381		blvs _errOverflow
382		push {r8}
383		pop {r0}
384		mov r0, r0
385		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
386		mov sp, fp
387		pop {fp, pc}
388		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
389		@ the .ltorg here is used to generate the constant pool after every function
390		@ this is important for ensuring string literals and large integers are accessible!
391		.ltorg
392	
393	wacc_addByInt:
394		push {fp, lr}
395		push {r4}
396		mov fp, sp
397		@ Stack pointer unchanged, no stack allocated variables
398		push {r0, r1}
399		@ Set up R12 as a temporary second base pointer for the caller saved things
400		mov r12, sp
401		@ Stack pointer unchanged, no stack allocated arguments
402		bl wacc_f
403		mov r12, r0
404		@ Stack pointer unchanged, no stack allocated arguments
405		pop {r0, r1}
406		mov r8, r12
407		mov r4, r8
408		smull r8, r9, r1, r4
409		@ Compare the sign of the lowest bits of the multiplication against the highest
410		cmp r9, r8, asr #31
411		blne _errOverflow
412		push {r8}
413		pop {r9}
414		adds r8, r0, r9
415		blvs _errOverflow
416		push {r8}
417		pop {r0}
418		mov r0, r0
419		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
420		mov sp, fp
421		pop {r4}
422		pop {fp, pc}
423		@ Stack pointer unchanged, no stack allocated variables
424		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
425		@ the .ltorg here is used to generate the constant pool after every function
426		@ this is important for ensuring string literals and large integers are accessible!
427		.ltorg
428	
429	wacc_subtractByInt:
430		push {fp, lr}
431		push {r4}
432		mov fp, sp
433		@ Stack pointer unchanged, no stack allocated variables
434		push {r0, r1}
435		@ Set up R12 as a temporary second base pointer for the caller saved things
436		mov r12, sp
437		@ Stack pointer unchanged, no stack allocated arguments
438		bl wacc_f
439		mov r12, r0
440		@ Stack pointer unchanged, no stack allocated arguments
441		pop {r0, r1}
442		mov r8, r12
443		mov r4, r8
444		smull r8, r9, r1, r4
445		@ Compare the sign of the lowest bits of the multiplication against the highest
446		cmp r9, r8, asr #31
447		blne _errOverflow
448		push {r8}
449		pop {r9}
450		subs r8, r0, r9
451		blvs _errOverflow
452		push {r8}
453		pop {r0}
454		mov r0, r0
455		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
456		mov sp, fp
457		pop {r4}
458		pop {fp, pc}
459		@ Stack pointer unchanged, no stack allocated variables
460		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
461		@ the .ltorg here is used to generate the constant pool after every function
462		@ this is important for ensuring string literals and large integers are accessible!
463		.ltorg
464	
465	wacc_multiply:
466		push {fp, lr}
467		push {r4}
468		mov fp, sp
469		@ Stack pointer unchanged, no stack allocated variables
470		push {r0, r1}
471		@ Set up R12 as a temporary second base pointer for the caller saved things
472		mov r12, sp
473		@ Stack pointer unchanged, no stack allocated arguments
474		bl wacc_f
475		mov r12, r0
476		@ Stack pointer unchanged, no stack allocated arguments
477		pop {r0, r1}
478		mov r8, r12
479		mov r4, r8
480		smull r8, r9, r0, r1
481		@ Compare the sign of the lowest bits of the multiplication against the highest
482		cmp r9, r8, asr #31
483		blne _errOverflow
484		push {r8}
485		pop {r8}
486		push {r0, r1}
487		mov r0, r8
488		mov r1, r4
489		cmp r1, #0
490		bleq _errDivZero
491		bl __aeabi_idivmod
492		mov r12, r0
493		pop {r0, r1}
494		mov r8, r12
495		push {r8}
496		pop {r0}
497		mov r0, r0
498		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
499		mov sp, fp
500		pop {r4}
501		pop {fp, pc}
502		@ Stack pointer unchanged, no stack allocated variables
503		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
504		@ the .ltorg here is used to generate the constant pool after every function
505		@ this is important for ensuring string literals and large integers are accessible!
506		.ltorg
507	
508	wacc_multiplyByInt:
509		push {fp, lr}
510		mov fp, sp
511		smull r8, r9, r0, r1
512		@ Compare the sign of the lowest bits of the multiplication against the highest
513		cmp r9, r8, asr #31
514		blne _errOverflow
515		push {r8}
516		pop {r0}
517		mov r0, r0
518		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
519		mov sp, fp
520		pop {fp, pc}
521		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
522		@ the .ltorg here is used to generate the constant pool after every function
523		@ this is important for ensuring string literals and large integers are accessible!
524		.ltorg
525	
526	wacc_divide:
527		push {fp, lr}
528		push {r4}
529		mov fp, sp
530		@ Stack pointer unchanged, no stack allocated variables
531		push {r0, r1}
532		@ Set up R12 as a temporary second base pointer for the caller saved things
533		mov r12, sp
534		@ Stack pointer unchanged, no stack allocated arguments
535		bl wacc_f
536		mov r12, r0
537		@ Stack pointer unchanged, no stack allocated arguments
538		pop {r0, r1}
539		mov r8, r12
540		mov r4, r8
541		smull r8, r9, r0, r4
542		@ Compare the sign of the lowest bits of the multiplication against the highest
543		cmp r9, r8, asr #31
544		blne _errOverflow
545		push {r8}
546		pop {r8}
547		push {r0, r1}
548		mov r0, r8
549		mov r1, r1
550		cmp r1, #0
551		bleq _errDivZero
552		bl __aeabi_idivmod
553		mov r12, r0
554		pop {r0, r1}
555		mov r8, r12
556		push {r8}
557		pop {r0}
558		mov r0, r0
559		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
560		mov sp, fp
561		pop {r4}
562		pop {fp, pc}
563		@ Stack pointer unchanged, no stack allocated variables
564		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
565		@ the .ltorg here is used to generate the constant pool after every function
566		@ this is important for ensuring string literals and large integers are accessible!
567		.ltorg
568	
569	wacc_divideByInt:
570		push {fp, lr}
571		mov fp, sp
572		push {r0, r1}
573		mov r0, r0
574		mov r1, r1
575		cmp r1, #0
576		bleq _errDivZero
577		bl __aeabi_idivmod
578		mov r12, r0
579		pop {r0, r1}
580		mov r8, r12
581		push {r8}
582		pop {r0}
583		mov r0, r0
584		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
585		mov sp, fp
586		pop {fp, pc}
587		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
588		@ the .ltorg here is used to generate the constant pool after every function
589		@ this is important for ensuring string literals and large integers are accessible!
590		.ltorg
591	
592	@ length of .L._prints_str0
593		.word 4
594	.L._prints_str0:
595		.asciz "%.*s"
596	.align 4
597	_prints:
598		push {fp, lr}
599		mov fp, sp
600		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
601		bic sp, sp, #0x7
602		mov r2, r0
603		ldr r1, [r0, #-4]
604		adr r0, .L._prints_str0
605		bl printf
606		mov r0, #0
607		bl fflush
608		mov sp, fp
609		pop {fp, pc}
610	
611	@ length of .L._printi_str0
612		.word 2
613	.L._printi_str0:
614		.asciz "%d"
615	.align 4
616	_printi:
617		push {fp, lr}
618		mov fp, sp
619		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
620		bic sp, sp, #0x7
621		mov r1, r0
622		adr r0, .L._printi_str0
623		bl printf
624		mov r0, #0
625		bl fflush
626		mov sp, fp
627		pop {fp, pc}
628	
629	@ length of .L._println_str0
630		.word 0
631	.L._println_str0:
632		.asciz ""
633	.align 4
634	_println:
635		push {fp, lr}
636		mov fp, sp
637		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
638		bic sp, sp, #0x7
639		adr r0, .L._println_str0
640		bl puts
641		mov r0, #0
642		bl fflush
643		mov sp, fp
644		pop {fp, pc}
645	
646	_exit:
647		push {fp, lr}
648		mov fp, sp
649		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
650		bic sp, sp, #0x7
651		bl exit
652		mov sp, fp
653		pop {fp, pc}
654	
655	@ length of .L._errDivZero_str0
656		.word 40
657	.L._errDivZero_str0:
658		.asciz "fatal error: division or modulo by zero\n"
659	.align 4
660	_errDivZero:
661		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
662		bic sp, sp, #0x7
663		adr r0, .L._errDivZero_str0
664		bl _prints
665		mov r0, #255
666		bl _exit
667	
668	@ length of .L._errOverflow_str0
669		.word 52
670	.L._errOverflow_str0:
671		.asciz "fatal error: integer overflow or underflow occurred\n"
672	.align 4
673	_errOverflow:
674		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
675		bic sp, sp, #0x7
676		adr r0, .L._errOverflow_str0
677		bl _prints
678		mov r0, #255
679		bl _exit
===========================================================
-- Finished

