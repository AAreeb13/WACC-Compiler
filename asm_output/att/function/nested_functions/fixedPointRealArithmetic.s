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
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 24
4	.L.str0:
5		.asciz "Using fixed-point real: "
6	# length of .L.str1
7		.int 3
8	.L.str1:
9		.asciz " / "
10	# length of .L.str2
11		.int 3
12	.L.str2:
13		.asciz " * "
14	# length of .L.str3
15		.int 3
16	.L.str3:
17		.asciz " = "
18	.text
19	main:
20		pushq %rbp
21		# pushq {%rbx, %r12, %r13, %r14, %r15}
22		subq $40, %rsp
23		movq %rbx, (%rsp)
24		movq %r12, 8(%rsp)
25		movq %r13, 16(%rsp)
26		movq %r14, 24(%rsp)
27		movq %r15, 32(%rsp)
28		movq %rsp, %rbp
29		# Stack pointer unchanged, no stack allocated variables
30		movq $10, %rax
31		movq %rax, %r12
32		movq $3, %rax
33		movq %rax, %r13
34		# Stack pointer unchanged, no stack allocated arguments
35		leaq .L.str0(%rip), %rax
36		pushq %rax
37		popq %rax
38		movq %rax, %rax
39		movq %rax, %rdi
40		# statement primitives do not return results (but will clobber r0/rax)
41		call _prints
42		# Stack pointer unchanged, no stack allocated arguments
43		movq %r12, %rax
44		movq %rax, %rdi
45		# statement primitives do not return results (but will clobber r0/rax)
46		call _printi
47		# Stack pointer unchanged, no stack allocated arguments
48		leaq .L.str1(%rip), %rax
49		pushq %rax
50		popq %rax
51		movq %rax, %rax
52		movq %rax, %rdi
53		# statement primitives do not return results (but will clobber r0/rax)
54		call _prints
55		# Stack pointer unchanged, no stack allocated arguments
56		movq %r13, %rax
57		movq %rax, %rdi
58		# statement primitives do not return results (but will clobber r0/rax)
59		call _printi
60		# Stack pointer unchanged, no stack allocated arguments
61		leaq .L.str2(%rip), %rax
62		pushq %rax
63		popq %rax
64		movq %rax, %rax
65		movq %rax, %rdi
66		# statement primitives do not return results (but will clobber r0/rax)
67		call _prints
68		# Stack pointer unchanged, no stack allocated arguments
69		movq %r13, %rax
70		movq %rax, %rdi
71		# statement primitives do not return results (but will clobber r0/rax)
72		call _printi
73		# Stack pointer unchanged, no stack allocated arguments
74		leaq .L.str3(%rip), %rax
75		pushq %rax
76		popq %rax
77		movq %rax, %rax
78		movq %rax, %rdi
79		# statement primitives do not return results (but will clobber r0/rax)
80		call _prints
81		# Stack pointer unchanged, no stack allocated arguments
82		movq %r12, %rax
83		movq %rax, %rdi
84		call wacc_intToFixedPoint
85		movq %rax, %r11
86		# Stack pointer unchanged, no stack allocated arguments
87		movq %r11, %rax
88		movq %rax, %r14
89		# Stack pointer unchanged, no stack allocated arguments
90		movq %r14, %rax
91		movq %rax, %rdi
92		movq %r13, %rax
93		movq %rax, %rsi
94		call wacc_divideByInt
95		movq %rax, %r11
96		# Stack pointer unchanged, no stack allocated arguments
97		movq %r11, %rax
98		movq %rax, %r14
99		# Stack pointer unchanged, no stack allocated arguments
100		movq %r14, %rax
101		movq %rax, %rdi
102		movq %r13, %rax
103		movq %rax, %rsi
104		call wacc_multiplyByInt
105		movq %rax, %r11
106		# Stack pointer unchanged, no stack allocated arguments
107		movq %r11, %rax
108		movq %rax, %r14
109		# Stack pointer unchanged, no stack allocated arguments
110		movq %r14, %rax
111		movq %rax, %rdi
112		call wacc_fixedPointToIntRoundNear
113		movq %rax, %r11
114		# Stack pointer unchanged, no stack allocated arguments
115		movq %r11, %rax
116		movq %rax, %r15
117		# Stack pointer unchanged, no stack allocated arguments
118		movq %r15, %rax
119		movq %rax, %rdi
120		# statement primitives do not return results (but will clobber r0/rax)
121		call _printi
122		call _println
123		# Stack pointer unchanged, no stack allocated variables
124		movq $0, %rax
125		# popq {%rbx, %r12, %r13, %r14, %r15}
126		movq (%rsp), %rbx
127		movq 8(%rsp), %r12
128		movq 16(%rsp), %r13
129		movq 24(%rsp), %r14
130		movq 32(%rsp), %r15
131		addq $40, %rsp
132		popq %rbp
133		ret
134	
135	wacc_q:
136		pushq %rbp
137		movq %rsp, %rbp
138		movq $14, %rax
139		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
140		movq %rbp, %rsp
141		popq %rbp
142		ret
143		# 'ere be dragons: this is 100% dead code, functions always end in returns!
144	
145	wacc_power:
146		pushq %rbp
147		pushq %r12
148		movq %rsp, %rbp
149		# Stack pointer unchanged, no stack allocated variables
150		movq $1, %rax
151		movq %rax, %r12
152		jmp .L0
153	.L1:
154		movl %r12d, %eax
155		imull %edi, %eax
156		jo _errOverflow
157		movslq %eax, %rax
158		pushq %rax
159		popq %rax
160		movq %rax, %rax
161		movq %rax, %r12
162		movl %esi, %eax
163		subl $1, %eax
164		jo _errOverflow
165		movslq %eax, %rax
166		pushq %rax
167		popq %rax
168		movq %rax, %rax
169		movq %rax, %rsi
170	.L0:
171		cmpq $0, %rsi
172		jg .L1
173		movq %r12, %rax
174		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
175		movq %rbp, %rsp
176		popq %r12
177		popq %rbp
178		ret
179		# Stack pointer unchanged, no stack allocated variables
180		# 'ere be dragons: this is 100% dead code, functions always end in returns!
181	
182	wacc_f:
183		pushq %rbp
184		# pushq {%r12, %r13}
185		subq $16, %rsp
186		movq %r12, (%rsp)
187		movq %r13, 8(%rsp)
188		movq %rsp, %rbp
189		# Stack pointer unchanged, no stack allocated variables
190		# Stack pointer unchanged, no stack allocated arguments
191		call wacc_q
192		movq %rax, %r11
193		# Stack pointer unchanged, no stack allocated arguments
194		movq %r11, %rax
195		movq %rax, %r12
196		# Stack pointer unchanged, no stack allocated arguments
197		movq $2, %rax
198		movq %rax, %rdi
199		movq %r12, %rax
200		movq %rax, %rsi
201		call wacc_power
202		movq %rax, %r11
203		# Stack pointer unchanged, no stack allocated arguments
204		movq %r11, %rax
205		movq %rax, %r13
206		movq %r13, %rax
207		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
208		movq %rbp, %rsp
209		# popq {%r12, %r13}
210		movq (%rsp), %r12
211		movq 8(%rsp), %r13
212		addq $16, %rsp
213		popq %rbp
214		ret
215		# Stack pointer unchanged, no stack allocated variables
216		# 'ere be dragons: this is 100% dead code, functions always end in returns!
217	
218	wacc_intToFixedPoint:
219		pushq %rbp
220		pushq %r12
221		movq %rsp, %rbp
222		# Stack pointer unchanged, no stack allocated variables
223		pushq %rdi
224		# Set up R11 as a temporary second base pointer for the caller saved things
225		movq %rsp, %r11
226		# Stack pointer unchanged, no stack allocated arguments
227		call wacc_f
228		movq %rax, %r11
229		# Stack pointer unchanged, no stack allocated arguments
230		popq %rdi
231		movq %r11, %rax
232		movq %rax, %r12
233		movl %edi, %eax
234		imull %r12d, %eax
235		jo _errOverflow
236		movslq %eax, %rax
237		pushq %rax
238		popq %rax
239		movq %rax, %rax
240		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
241		movq %rbp, %rsp
242		popq %r12
243		popq %rbp
244		ret
245		# Stack pointer unchanged, no stack allocated variables
246		# 'ere be dragons: this is 100% dead code, functions always end in returns!
247	
248	wacc_fixedPointToIntRoundDown:
249		pushq %rbp
250		pushq %r12
251		movq %rsp, %rbp
252		# Stack pointer unchanged, no stack allocated variables
253		pushq %rdi
254		# Set up R11 as a temporary second base pointer for the caller saved things
255		movq %rsp, %r11
256		# Stack pointer unchanged, no stack allocated arguments
257		call wacc_f
258		movq %rax, %r11
259		# Stack pointer unchanged, no stack allocated arguments
260		popq %rdi
261		movq %r11, %rax
262		movq %rax, %r12
263		movl %edi, %eax
264		cmpl $0, %r12d
265		je _errDivZero
266		# sign extend EAX into EDX
267		cltd
268		idivl %r12d
269		movl %eax, %eax
270		movl %eax, %eax
271		movslq %eax, %rax
272		pushq %rax
273		popq %rax
274		movq %rax, %rax
275		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
276		movq %rbp, %rsp
277		popq %r12
278		popq %rbp
279		ret
280		# Stack pointer unchanged, no stack allocated variables
281		# 'ere be dragons: this is 100% dead code, functions always end in returns!
282	
283	wacc_fixedPointToIntRoundNear:
284		pushq %rbp
285		pushq %r12
286		movq %rsp, %rbp
287		# Stack pointer unchanged, no stack allocated variables
288		pushq %rdi
289		# Set up R11 as a temporary second base pointer for the caller saved things
290		movq %rsp, %r11
291		# Stack pointer unchanged, no stack allocated arguments
292		call wacc_f
293		movq %rax, %r11
294		# Stack pointer unchanged, no stack allocated arguments
295		popq %rdi
296		movq %r11, %rax
297		movq %rax, %r12
298		cmpq $0, %rdi
299		jge .L2
300		movq $2, %rbx
301		movl %r12d, %eax
302		cmpl $0, %ebx
303		je _errDivZero
304		# sign extend EAX into EDX
305		cltd
306		idivl %ebx
307		movl %eax, %eax
308		movl %eax, %eax
309		movslq %eax, %rax
310		pushq %rax
311		popq %rbx
312		movl %edi, %eax
313		subl %ebx, %eax
314		jo _errOverflow
315		movslq %eax, %rax
316		pushq %rax
317		popq %rax
318		movl %eax, %eax
319		cmpl $0, %r12d
320		je _errDivZero
321		# sign extend EAX into EDX
322		cltd
323		idivl %r12d
324		movl %eax, %eax
325		movl %eax, %eax
326		movslq %eax, %rax
327		pushq %rax
328		popq %rax
329		movq %rax, %rax
330		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
331		movq %rbp, %rsp
332		popq %r12
333		popq %rbp
334		ret
335		jmp .L3
336	.L2:
337		movq $2, %rbx
338		movl %r12d, %eax
339		cmpl $0, %ebx
340		je _errDivZero
341		# sign extend EAX into EDX
342		cltd
343		idivl %ebx
344		movl %eax, %eax
345		movl %eax, %eax
346		movslq %eax, %rax
347		pushq %rax
348		popq %rbx
349		movl %edi, %eax
350		addl %ebx, %eax
351		jo _errOverflow
352		movslq %eax, %rax
353		pushq %rax
354		popq %rax
355		movl %eax, %eax
356		cmpl $0, %r12d
357		je _errDivZero
358		# sign extend EAX into EDX
359		cltd
360		idivl %r12d
361		movl %eax, %eax
362		movl %eax, %eax
363		movslq %eax, %rax
364		pushq %rax
365		popq %rax
366		movq %rax, %rax
367		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
368		movq %rbp, %rsp
369		popq %r12
370		popq %rbp
371		ret
372	.L3:
373		# Stack pointer unchanged, no stack allocated variables
374		# 'ere be dragons: this is 100% dead code, functions always end in returns!
375	
376	wacc_add:
377		pushq %rbp
378		movq %rsp, %rbp
379		movl %edi, %eax
380		addl %esi, %eax
381		jo _errOverflow
382		movslq %eax, %rax
383		pushq %rax
384		popq %rax
385		movq %rax, %rax
386		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
387		movq %rbp, %rsp
388		popq %rbp
389		ret
390		# 'ere be dragons: this is 100% dead code, functions always end in returns!
391	
392	wacc_subtract:
393		pushq %rbp
394		movq %rsp, %rbp
395		movl %edi, %eax
396		subl %esi, %eax
397		jo _errOverflow
398		movslq %eax, %rax
399		pushq %rax
400		popq %rax
401		movq %rax, %rax
402		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
403		movq %rbp, %rsp
404		popq %rbp
405		ret
406		# 'ere be dragons: this is 100% dead code, functions always end in returns!
407	
408	wacc_addByInt:
409		pushq %rbp
410		pushq %r12
411		movq %rsp, %rbp
412		# Stack pointer unchanged, no stack allocated variables
413		# pushq {%rsi, %rdi}
414		subq $16, %rsp
415		movq %rsi, (%rsp)
416		movq %rdi, 8(%rsp)
417		# Set up R11 as a temporary second base pointer for the caller saved things
418		movq %rsp, %r11
419		# Stack pointer unchanged, no stack allocated arguments
420		call wacc_f
421		movq %rax, %r11
422		# Stack pointer unchanged, no stack allocated arguments
423		# popq {%rsi, %rdi}
424		movq (%rsp), %rsi
425		movq 8(%rsp), %rdi
426		addq $16, %rsp
427		movq %r11, %rax
428		movq %rax, %r12
429		movl %esi, %eax
430		imull %r12d, %eax
431		jo _errOverflow
432		movslq %eax, %rax
433		pushq %rax
434		popq %rbx
435		movl %edi, %eax
436		addl %ebx, %eax
437		jo _errOverflow
438		movslq %eax, %rax
439		pushq %rax
440		popq %rax
441		movq %rax, %rax
442		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
443		movq %rbp, %rsp
444		popq %r12
445		popq %rbp
446		ret
447		# Stack pointer unchanged, no stack allocated variables
448		# 'ere be dragons: this is 100% dead code, functions always end in returns!
449	
450	wacc_subtractByInt:
451		pushq %rbp
452		pushq %r12
453		movq %rsp, %rbp
454		# Stack pointer unchanged, no stack allocated variables
455		# pushq {%rsi, %rdi}
456		subq $16, %rsp
457		movq %rsi, (%rsp)
458		movq %rdi, 8(%rsp)
459		# Set up R11 as a temporary second base pointer for the caller saved things
460		movq %rsp, %r11
461		# Stack pointer unchanged, no stack allocated arguments
462		call wacc_f
463		movq %rax, %r11
464		# Stack pointer unchanged, no stack allocated arguments
465		# popq {%rsi, %rdi}
466		movq (%rsp), %rsi
467		movq 8(%rsp), %rdi
468		addq $16, %rsp
469		movq %r11, %rax
470		movq %rax, %r12
471		movl %esi, %eax
472		imull %r12d, %eax
473		jo _errOverflow
474		movslq %eax, %rax
475		pushq %rax
476		popq %rbx
477		movl %edi, %eax
478		subl %ebx, %eax
479		jo _errOverflow
480		movslq %eax, %rax
481		pushq %rax
482		popq %rax
483		movq %rax, %rax
484		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
485		movq %rbp, %rsp
486		popq %r12
487		popq %rbp
488		ret
489		# Stack pointer unchanged, no stack allocated variables
490		# 'ere be dragons: this is 100% dead code, functions always end in returns!
491	
492	wacc_multiply:
493		pushq %rbp
494		pushq %r12
495		movq %rsp, %rbp
496		# Stack pointer unchanged, no stack allocated variables
497		# pushq {%rsi, %rdi}
498		subq $16, %rsp
499		movq %rsi, (%rsp)
500		movq %rdi, 8(%rsp)
501		# Set up R11 as a temporary second base pointer for the caller saved things
502		movq %rsp, %r11
503		# Stack pointer unchanged, no stack allocated arguments
504		call wacc_f
505		movq %rax, %r11
506		# Stack pointer unchanged, no stack allocated arguments
507		# popq {%rsi, %rdi}
508		movq (%rsp), %rsi
509		movq 8(%rsp), %rdi
510		addq $16, %rsp
511		movq %r11, %rax
512		movq %rax, %r12
513		movl %edi, %eax
514		imull %esi, %eax
515		jo _errOverflow
516		movslq %eax, %rax
517		pushq %rax
518		popq %rax
519		movl %eax, %eax
520		cmpl $0, %r12d
521		je _errDivZero
522		# sign extend EAX into EDX
523		cltd
524		idivl %r12d
525		movl %eax, %eax
526		movl %eax, %eax
527		movslq %eax, %rax
528		pushq %rax
529		popq %rax
530		movq %rax, %rax
531		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
532		movq %rbp, %rsp
533		popq %r12
534		popq %rbp
535		ret
536		# Stack pointer unchanged, no stack allocated variables
537		# 'ere be dragons: this is 100% dead code, functions always end in returns!
538	
539	wacc_multiplyByInt:
540		pushq %rbp
541		movq %rsp, %rbp
542		movl %edi, %eax
543		imull %esi, %eax
544		jo _errOverflow
545		movslq %eax, %rax
546		pushq %rax
547		popq %rax
548		movq %rax, %rax
549		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
550		movq %rbp, %rsp
551		popq %rbp
552		ret
553		# 'ere be dragons: this is 100% dead code, functions always end in returns!
554	
555	wacc_divide:
556		pushq %rbp
557		pushq %r12
558		movq %rsp, %rbp
559		# Stack pointer unchanged, no stack allocated variables
560		# pushq {%rsi, %rdi}
561		subq $16, %rsp
562		movq %rsi, (%rsp)
563		movq %rdi, 8(%rsp)
564		# Set up R11 as a temporary second base pointer for the caller saved things
565		movq %rsp, %r11
566		# Stack pointer unchanged, no stack allocated arguments
567		call wacc_f
568		movq %rax, %r11
569		# Stack pointer unchanged, no stack allocated arguments
570		# popq {%rsi, %rdi}
571		movq (%rsp), %rsi
572		movq 8(%rsp), %rdi
573		addq $16, %rsp
574		movq %r11, %rax
575		movq %rax, %r12
576		movl %edi, %eax
577		imull %r12d, %eax
578		jo _errOverflow
579		movslq %eax, %rax
580		pushq %rax
581		popq %rax
582		movl %eax, %eax
583		cmpl $0, %esi
584		je _errDivZero
585		# sign extend EAX into EDX
586		cltd
587		idivl %esi
588		movl %eax, %eax
589		movl %eax, %eax
590		movslq %eax, %rax
591		pushq %rax
592		popq %rax
593		movq %rax, %rax
594		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
595		movq %rbp, %rsp
596		popq %r12
597		popq %rbp
598		ret
599		# Stack pointer unchanged, no stack allocated variables
600		# 'ere be dragons: this is 100% dead code, functions always end in returns!
601	
602	wacc_divideByInt:
603		pushq %rbp
604		movq %rsp, %rbp
605		movl %edi, %eax
606		cmpl $0, %esi
607		je _errDivZero
608		# sign extend EAX into EDX
609		cltd
610		idivl %esi
611		movl %eax, %eax
612		movl %eax, %eax
613		movslq %eax, %rax
614		pushq %rax
615		popq %rax
616		movq %rax, %rax
617		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
618		movq %rbp, %rsp
619		popq %rbp
620		ret
621		# 'ere be dragons: this is 100% dead code, functions always end in returns!
622	
623	.section .rodata
624	# length of .L._prints_str0
625		.int 4
626	.L._prints_str0:
627		.asciz "%.*s"
628	.text
629	_prints:
630		pushq %rbp
631		movq %rsp, %rbp
632		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
633		andq $-16, %rsp
634		movq %rdi, %rdx
635		movl -4(%rdi), %esi
636		leaq .L._prints_str0(%rip), %rdi
637		# on x86, al represents the number of SIMD registers used as variadic arguments
638		movb $0, %al
639		call printf@plt
640		movq $0, %rdi
641		call fflush@plt
642		movq %rbp, %rsp
643		popq %rbp
644		ret
645	
646	.section .rodata
647	# length of .L._printi_str0
648		.int 2
649	.L._printi_str0:
650		.asciz "%d"
651	.text
652	_printi:
653		pushq %rbp
654		movq %rsp, %rbp
655		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
656		andq $-16, %rsp
657		movl %edi, %esi
658		leaq .L._printi_str0(%rip), %rdi
659		# on x86, al represents the number of SIMD registers used as variadic arguments
660		movb $0, %al
661		call printf@plt
662		movq $0, %rdi
663		call fflush@plt
664		movq %rbp, %rsp
665		popq %rbp
666		ret
667	
668	.section .rodata
669	# length of .L._println_str0
670		.int 0
671	.L._println_str0:
672		.asciz ""
673	.text
674	_println:
675		pushq %rbp
676		movq %rsp, %rbp
677		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
678		andq $-16, %rsp
679		leaq .L._println_str0(%rip), %rdi
680		call puts@plt
681		movq $0, %rdi
682		call fflush@plt
683		movq %rbp, %rsp
684		popq %rbp
685		ret
686	
687	_exit:
688		pushq %rbp
689		movq %rsp, %rbp
690		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
691		andq $-16, %rsp
692		call exit@plt
693		movq %rbp, %rsp
694		popq %rbp
695		ret
696	
697	.section .rodata
698	# length of .L._errDivZero_str0
699		.int 40
700	.L._errDivZero_str0:
701		.asciz "fatal error: division or modulo by zero\n"
702	.text
703	_errDivZero:
704		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
705		andq $-16, %rsp
706		leaq .L._errDivZero_str0(%rip), %rdi
707		call _prints
708		movb $-1, %dil
709		call exit@plt
710	
711	.section .rodata
712	# length of .L._errOverflow_str0
713		.int 52
714	.L._errOverflow_str0:
715		.asciz "fatal error: integer overflow or underflow occurred\n"
716	.text
717	_errOverflow:
718		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
719		andq $-16, %rsp
720		leaq .L._errOverflow_str0(%rip), %rdi
721		call _prints
722		movb $-1, %dil
723		call exit@plt
===========================================================
-- Finished

