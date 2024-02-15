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
1	// length of .L.str0
2		.word 24
3	.L.str0:
4		.asciz "Using fixed-point real: "
5	// length of .L.str1
6		.word 3
7	.L.str1:
8		.asciz " / "
9	// length of .L.str2
10		.word 3
11	.L.str2:
12		.asciz " * "
13	// length of .L.str3
14		.word 3
15	.L.str3:
16		.asciz " = "
17	.align 4
18	.text
19	.global main
20	main:
21		// push {fp, lr}
22		stp fp, lr, [sp, #-16]!
23		// push {x19, x20, x21, x22}
24		stp x19, x20, [sp, #-32]!
25		stp x21, x22, [sp, #16]
26		mov fp, sp
27		// Stack pointer unchanged, no stack allocated variables
28		mov x8, #10
29		mov x19, x8
30		mov x8, #3
31		mov x20, x8
32		// Stack pointer unchanged, no stack allocated arguments
33		adrp x8, .L.str0
34		add x8, x8, :lo12:.L.str0
35		// push {x8}
36		stp x8, xzr, [sp, #-16]!
37		// pop {x8}
38		ldp x8, xzr, [sp], #16
39		mov x8, x8
40		mov x0, x8
41		// statement primitives do not return results (but will clobber r0/rax)
42		bl _prints
43		// Stack pointer unchanged, no stack allocated arguments
44		mov x8, x19
45		mov x0, x8
46		// statement primitives do not return results (but will clobber r0/rax)
47		bl _printi
48		// Stack pointer unchanged, no stack allocated arguments
49		adrp x8, .L.str1
50		add x8, x8, :lo12:.L.str1
51		// push {x8}
52		stp x8, xzr, [sp, #-16]!
53		// pop {x8}
54		ldp x8, xzr, [sp], #16
55		mov x8, x8
56		mov x0, x8
57		// statement primitives do not return results (but will clobber r0/rax)
58		bl _prints
59		// Stack pointer unchanged, no stack allocated arguments
60		mov x8, x20
61		mov x0, x8
62		// statement primitives do not return results (but will clobber r0/rax)
63		bl _printi
64		// Stack pointer unchanged, no stack allocated arguments
65		adrp x8, .L.str2
66		add x8, x8, :lo12:.L.str2
67		// push {x8}
68		stp x8, xzr, [sp, #-16]!
69		// pop {x8}
70		ldp x8, xzr, [sp], #16
71		mov x8, x8
72		mov x0, x8
73		// statement primitives do not return results (but will clobber r0/rax)
74		bl _prints
75		// Stack pointer unchanged, no stack allocated arguments
76		mov x8, x20
77		mov x0, x8
78		// statement primitives do not return results (but will clobber r0/rax)
79		bl _printi
80		// Stack pointer unchanged, no stack allocated arguments
81		adrp x8, .L.str3
82		add x8, x8, :lo12:.L.str3
83		// push {x8}
84		stp x8, xzr, [sp, #-16]!
85		// pop {x8}
86		ldp x8, xzr, [sp], #16
87		mov x8, x8
88		mov x0, x8
89		// statement primitives do not return results (but will clobber r0/rax)
90		bl _prints
91		// Stack pointer unchanged, no stack allocated arguments
92		mov x8, x19
93		mov x0, x8
94		bl wacc_intToFixedPoint
95		mov x16, x0
96		// Stack pointer unchanged, no stack allocated arguments
97		mov x8, x16
98		mov x21, x8
99		// Stack pointer unchanged, no stack allocated arguments
100		mov x8, x21
101		mov x0, x8
102		mov x8, x20
103		mov x1, x8
104		bl wacc_divideByInt
105		mov x16, x0
106		// Stack pointer unchanged, no stack allocated arguments
107		mov x8, x16
108		mov x21, x8
109		// Stack pointer unchanged, no stack allocated arguments
110		mov x8, x21
111		mov x0, x8
112		mov x8, x20
113		mov x1, x8
114		bl wacc_multiplyByInt
115		mov x16, x0
116		// Stack pointer unchanged, no stack allocated arguments
117		mov x8, x16
118		mov x21, x8
119		// Stack pointer unchanged, no stack allocated arguments
120		mov x8, x21
121		mov x0, x8
122		bl wacc_fixedPointToIntRoundNear
123		mov x16, x0
124		// Stack pointer unchanged, no stack allocated arguments
125		mov x8, x16
126		mov x22, x8
127		// Stack pointer unchanged, no stack allocated arguments
128		mov x8, x22
129		mov x0, x8
130		// statement primitives do not return results (but will clobber r0/rax)
131		bl _printi
132		bl _println
133		// Stack pointer unchanged, no stack allocated variables
134		mov x0, #0
135		// pop {x19, x20, x21, x22}
136		ldp x21, x22, [sp, #16]
137		ldp x19, x20, [sp], #32
138		// pop {fp, lr}
139		ldp fp, lr, [sp], #16
140		ret
141	
142	wacc_q:
143		// push {fp, lr}
144		stp fp, lr, [sp, #-16]!
145		mov fp, sp
146		mov x0, #14
147		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
148		mov sp, fp
149		// pop {fp, lr}
150		ldp fp, lr, [sp], #16
151		ret
152		// 'ere be dragons: this is 100% dead code, functions always end in returns!
153	
154	wacc_power:
155		// push {fp, lr}
156		stp fp, lr, [sp, #-16]!
157		// push {x19}
158		stp x19, xzr, [sp, #-16]!
159		mov fp, sp
160		// Stack pointer unchanged, no stack allocated variables
161		mov x8, #1
162		mov x19, x8
163		b .L0
164	.L1:
165		smull x8, w19, w0
166		// take the 31st bit, sign extend it to 64 bits
167		sbfx x9, x8, #31, #1
168		// now take the top 32 bits of the result, shift and sign extend to 64 bits
169		cmp x9, x8, asr #32
170		// if they are not equal then overflow occured
171		b.ne _errOverflow
172		sxtw x8, w8
173		// push {x8}
174		stp x8, xzr, [sp, #-16]!
175		// pop {x8}
176		ldp x8, xzr, [sp], #16
177		mov x8, x8
178		mov x19, x8
179		subs w8, w1, #1
180		b.vs _errOverflow
181		sxtw x8, w8
182		// push {x8}
183		stp x8, xzr, [sp, #-16]!
184		// pop {x8}
185		ldp x8, xzr, [sp], #16
186		mov x8, x8
187		mov x1, x8
188	.L0:
189		cmp x1, #0
190		b.gt .L1
191		mov x0, x19
192		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
193		mov sp, fp
194		// pop {x19}
195		ldp x19, xzr, [sp], #16
196		// pop {fp, lr}
197		ldp fp, lr, [sp], #16
198		ret
199		// Stack pointer unchanged, no stack allocated variables
200		// 'ere be dragons: this is 100% dead code, functions always end in returns!
201	
202	wacc_f:
203		// push {fp, lr}
204		stp fp, lr, [sp, #-16]!
205		// push {x19, x20}
206		stp x19, x20, [sp, #-16]!
207		mov fp, sp
208		// Stack pointer unchanged, no stack allocated variables
209		// Stack pointer unchanged, no stack allocated arguments
210		bl wacc_q
211		mov x16, x0
212		// Stack pointer unchanged, no stack allocated arguments
213		mov x8, x16
214		mov x19, x8
215		// Stack pointer unchanged, no stack allocated arguments
216		mov x8, #2
217		mov x0, x8
218		mov x8, x19
219		mov x1, x8
220		bl wacc_power
221		mov x16, x0
222		// Stack pointer unchanged, no stack allocated arguments
223		mov x8, x16
224		mov x20, x8
225		mov x0, x20
226		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
227		mov sp, fp
228		// pop {x19, x20}
229		ldp x19, x20, [sp], #16
230		// pop {fp, lr}
231		ldp fp, lr, [sp], #16
232		ret
233		// Stack pointer unchanged, no stack allocated variables
234		// 'ere be dragons: this is 100% dead code, functions always end in returns!
235	
236	wacc_intToFixedPoint:
237		// push {fp, lr}
238		stp fp, lr, [sp, #-16]!
239		// push {x19}
240		stp x19, xzr, [sp, #-16]!
241		mov fp, sp
242		// Stack pointer unchanged, no stack allocated variables
243		// push {x0}
244		stp x0, xzr, [sp, #-16]!
245		// Set up X16 as a temporary second base pointer for the caller saved things
246		mov x16, sp
247		// Stack pointer unchanged, no stack allocated arguments
248		bl wacc_f
249		mov x16, x0
250		// Stack pointer unchanged, no stack allocated arguments
251		// pop {x0}
252		ldp x0, xzr, [sp], #16
253		mov x8, x16
254		mov x19, x8
255		smull x8, w0, w19
256		// take the 31st bit, sign extend it to 64 bits
257		sbfx x9, x8, #31, #1
258		// now take the top 32 bits of the result, shift and sign extend to 64 bits
259		cmp x9, x8, asr #32
260		// if they are not equal then overflow occured
261		b.ne _errOverflow
262		sxtw x8, w8
263		// push {x8}
264		stp x8, xzr, [sp, #-16]!
265		// pop {x0}
266		ldp x0, xzr, [sp], #16
267		mov x0, x0
268		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
269		mov sp, fp
270		// pop {x19}
271		ldp x19, xzr, [sp], #16
272		// pop {fp, lr}
273		ldp fp, lr, [sp], #16
274		ret
275		// Stack pointer unchanged, no stack allocated variables
276		// 'ere be dragons: this is 100% dead code, functions always end in returns!
277	
278	wacc_fixedPointToIntRoundDown:
279		// push {fp, lr}
280		stp fp, lr, [sp, #-16]!
281		// push {x19}
282		stp x19, xzr, [sp, #-16]!
283		mov fp, sp
284		// Stack pointer unchanged, no stack allocated variables
285		// push {x0}
286		stp x0, xzr, [sp, #-16]!
287		// Set up X16 as a temporary second base pointer for the caller saved things
288		mov x16, sp
289		// Stack pointer unchanged, no stack allocated arguments
290		bl wacc_f
291		mov x16, x0
292		// Stack pointer unchanged, no stack allocated arguments
293		// pop {x0}
294		ldp x0, xzr, [sp], #16
295		mov x8, x16
296		mov x19, x8
297		cbz x19, _errDivZero
298		sdiv w8, w0, w19
299		sxtw x8, w8
300		// push {x8}
301		stp x8, xzr, [sp, #-16]!
302		// pop {x0}
303		ldp x0, xzr, [sp], #16
304		mov x0, x0
305		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
306		mov sp, fp
307		// pop {x19}
308		ldp x19, xzr, [sp], #16
309		// pop {fp, lr}
310		ldp fp, lr, [sp], #16
311		ret
312		// Stack pointer unchanged, no stack allocated variables
313		// 'ere be dragons: this is 100% dead code, functions always end in returns!
314	
315	wacc_fixedPointToIntRoundNear:
316		// push {fp, lr}
317		stp fp, lr, [sp, #-16]!
318		// push {x19}
319		stp x19, xzr, [sp, #-16]!
320		mov fp, sp
321		// Stack pointer unchanged, no stack allocated variables
322		// push {x0}
323		stp x0, xzr, [sp, #-16]!
324		// Set up X16 as a temporary second base pointer for the caller saved things
325		mov x16, sp
326		// Stack pointer unchanged, no stack allocated arguments
327		bl wacc_f
328		mov x16, x0
329		// Stack pointer unchanged, no stack allocated arguments
330		// pop {x0}
331		ldp x0, xzr, [sp], #16
332		mov x8, x16
333		mov x19, x8
334		cmp x0, #0
335		b.ge .L2
336		mov w9, #2
337		cbz x9, _errDivZero
338		sdiv w8, w19, w9
339		sxtw x8, w8
340		// push {x8}
341		stp x8, xzr, [sp, #-16]!
342		// pop {x9}
343		ldp x9, xzr, [sp], #16
344		subs w8, w0, w9
345		b.vs _errOverflow
346		sxtw x8, w8
347		// push {x8}
348		stp x8, xzr, [sp, #-16]!
349		// pop {x8}
350		ldp x8, xzr, [sp], #16
351		cbz x19, _errDivZero
352		sdiv w8, w8, w19
353		sxtw x8, w8
354		// push {x8}
355		stp x8, xzr, [sp, #-16]!
356		// pop {x0}
357		ldp x0, xzr, [sp], #16
358		mov x0, x0
359		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
360		mov sp, fp
361		// pop {x19}
362		ldp x19, xzr, [sp], #16
363		// pop {fp, lr}
364		ldp fp, lr, [sp], #16
365		ret
366		b .L3
367	.L2:
368		mov w9, #2
369		cbz x9, _errDivZero
370		sdiv w8, w19, w9
371		sxtw x8, w8
372		// push {x8}
373		stp x8, xzr, [sp, #-16]!
374		// pop {x9}
375		ldp x9, xzr, [sp], #16
376		adds w8, w0, w9
377		b.vs _errOverflow
378		sxtw x8, w8
379		// push {x8}
380		stp x8, xzr, [sp, #-16]!
381		// pop {x8}
382		ldp x8, xzr, [sp], #16
383		cbz x19, _errDivZero
384		sdiv w8, w8, w19
385		sxtw x8, w8
386		// push {x8}
387		stp x8, xzr, [sp, #-16]!
388		// pop {x0}
389		ldp x0, xzr, [sp], #16
390		mov x0, x0
391		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
392		mov sp, fp
393		// pop {x19}
394		ldp x19, xzr, [sp], #16
395		// pop {fp, lr}
396		ldp fp, lr, [sp], #16
397		ret
398	.L3:
399		// Stack pointer unchanged, no stack allocated variables
400		// 'ere be dragons: this is 100% dead code, functions always end in returns!
401	
402	wacc_add:
403		// push {fp, lr}
404		stp fp, lr, [sp, #-16]!
405		mov fp, sp
406		adds w8, w0, w1
407		b.vs _errOverflow
408		sxtw x8, w8
409		// push {x8}
410		stp x8, xzr, [sp, #-16]!
411		// pop {x0}
412		ldp x0, xzr, [sp], #16
413		mov x0, x0
414		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
415		mov sp, fp
416		// pop {fp, lr}
417		ldp fp, lr, [sp], #16
418		ret
419		// 'ere be dragons: this is 100% dead code, functions always end in returns!
420	
421	wacc_subtract:
422		// push {fp, lr}
423		stp fp, lr, [sp, #-16]!
424		mov fp, sp
425		subs w8, w0, w1
426		b.vs _errOverflow
427		sxtw x8, w8
428		// push {x8}
429		stp x8, xzr, [sp, #-16]!
430		// pop {x0}
431		ldp x0, xzr, [sp], #16
432		mov x0, x0
433		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
434		mov sp, fp
435		// pop {fp, lr}
436		ldp fp, lr, [sp], #16
437		ret
438		// 'ere be dragons: this is 100% dead code, functions always end in returns!
439	
440	wacc_addByInt:
441		// push {fp, lr}
442		stp fp, lr, [sp, #-16]!
443		// push {x19}
444		stp x19, xzr, [sp, #-16]!
445		mov fp, sp
446		// Stack pointer unchanged, no stack allocated variables
447		// push {x0, x1}
448		stp x0, x1, [sp, #-16]!
449		// Set up X16 as a temporary second base pointer for the caller saved things
450		mov x16, sp
451		// Stack pointer unchanged, no stack allocated arguments
452		bl wacc_f
453		mov x16, x0
454		// Stack pointer unchanged, no stack allocated arguments
455		// pop {x0, x1}
456		ldp x0, x1, [sp], #16
457		mov x8, x16
458		mov x19, x8
459		smull x8, w1, w19
460		// take the 31st bit, sign extend it to 64 bits
461		sbfx x9, x8, #31, #1
462		// now take the top 32 bits of the result, shift and sign extend to 64 bits
463		cmp x9, x8, asr #32
464		// if they are not equal then overflow occured
465		b.ne _errOverflow
466		sxtw x8, w8
467		// push {x8}
468		stp x8, xzr, [sp, #-16]!
469		// pop {x9}
470		ldp x9, xzr, [sp], #16
471		adds w8, w0, w9
472		b.vs _errOverflow
473		sxtw x8, w8
474		// push {x8}
475		stp x8, xzr, [sp, #-16]!
476		// pop {x0}
477		ldp x0, xzr, [sp], #16
478		mov x0, x0
479		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
480		mov sp, fp
481		// pop {x19}
482		ldp x19, xzr, [sp], #16
483		// pop {fp, lr}
484		ldp fp, lr, [sp], #16
485		ret
486		// Stack pointer unchanged, no stack allocated variables
487		// 'ere be dragons: this is 100% dead code, functions always end in returns!
488	
489	wacc_subtractByInt:
490		// push {fp, lr}
491		stp fp, lr, [sp, #-16]!
492		// push {x19}
493		stp x19, xzr, [sp, #-16]!
494		mov fp, sp
495		// Stack pointer unchanged, no stack allocated variables
496		// push {x0, x1}
497		stp x0, x1, [sp, #-16]!
498		// Set up X16 as a temporary second base pointer for the caller saved things
499		mov x16, sp
500		// Stack pointer unchanged, no stack allocated arguments
501		bl wacc_f
502		mov x16, x0
503		// Stack pointer unchanged, no stack allocated arguments
504		// pop {x0, x1}
505		ldp x0, x1, [sp], #16
506		mov x8, x16
507		mov x19, x8
508		smull x8, w1, w19
509		// take the 31st bit, sign extend it to 64 bits
510		sbfx x9, x8, #31, #1
511		// now take the top 32 bits of the result, shift and sign extend to 64 bits
512		cmp x9, x8, asr #32
513		// if they are not equal then overflow occured
514		b.ne _errOverflow
515		sxtw x8, w8
516		// push {x8}
517		stp x8, xzr, [sp, #-16]!
518		// pop {x9}
519		ldp x9, xzr, [sp], #16
520		subs w8, w0, w9
521		b.vs _errOverflow
522		sxtw x8, w8
523		// push {x8}
524		stp x8, xzr, [sp, #-16]!
525		// pop {x0}
526		ldp x0, xzr, [sp], #16
527		mov x0, x0
528		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
529		mov sp, fp
530		// pop {x19}
531		ldp x19, xzr, [sp], #16
532		// pop {fp, lr}
533		ldp fp, lr, [sp], #16
534		ret
535		// Stack pointer unchanged, no stack allocated variables
536		// 'ere be dragons: this is 100% dead code, functions always end in returns!
537	
538	wacc_multiply:
539		// push {fp, lr}
540		stp fp, lr, [sp, #-16]!
541		// push {x19}
542		stp x19, xzr, [sp, #-16]!
543		mov fp, sp
544		// Stack pointer unchanged, no stack allocated variables
545		// push {x0, x1}
546		stp x0, x1, [sp, #-16]!
547		// Set up X16 as a temporary second base pointer for the caller saved things
548		mov x16, sp
549		// Stack pointer unchanged, no stack allocated arguments
550		bl wacc_f
551		mov x16, x0
552		// Stack pointer unchanged, no stack allocated arguments
553		// pop {x0, x1}
554		ldp x0, x1, [sp], #16
555		mov x8, x16
556		mov x19, x8
557		smull x8, w0, w1
558		// take the 31st bit, sign extend it to 64 bits
559		sbfx x9, x8, #31, #1
560		// now take the top 32 bits of the result, shift and sign extend to 64 bits
561		cmp x9, x8, asr #32
562		// if they are not equal then overflow occured
563		b.ne _errOverflow
564		sxtw x8, w8
565		// push {x8}
566		stp x8, xzr, [sp, #-16]!
567		// pop {x8}
568		ldp x8, xzr, [sp], #16
569		cbz x19, _errDivZero
570		sdiv w8, w8, w19
571		sxtw x8, w8
572		// push {x8}
573		stp x8, xzr, [sp, #-16]!
574		// pop {x0}
575		ldp x0, xzr, [sp], #16
576		mov x0, x0
577		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
578		mov sp, fp
579		// pop {x19}
580		ldp x19, xzr, [sp], #16
581		// pop {fp, lr}
582		ldp fp, lr, [sp], #16
583		ret
584		// Stack pointer unchanged, no stack allocated variables
585		// 'ere be dragons: this is 100% dead code, functions always end in returns!
586	
587	wacc_multiplyByInt:
588		// push {fp, lr}
589		stp fp, lr, [sp, #-16]!
590		mov fp, sp
591		smull x8, w0, w1
592		// take the 31st bit, sign extend it to 64 bits
593		sbfx x9, x8, #31, #1
594		// now take the top 32 bits of the result, shift and sign extend to 64 bits
595		cmp x9, x8, asr #32
596		// if they are not equal then overflow occured
597		b.ne _errOverflow
598		sxtw x8, w8
599		// push {x8}
600		stp x8, xzr, [sp, #-16]!
601		// pop {x0}
602		ldp x0, xzr, [sp], #16
603		mov x0, x0
604		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
605		mov sp, fp
606		// pop {fp, lr}
607		ldp fp, lr, [sp], #16
608		ret
609		// 'ere be dragons: this is 100% dead code, functions always end in returns!
610	
611	wacc_divide:
612		// push {fp, lr}
613		stp fp, lr, [sp, #-16]!
614		// push {x19}
615		stp x19, xzr, [sp, #-16]!
616		mov fp, sp
617		// Stack pointer unchanged, no stack allocated variables
618		// push {x0, x1}
619		stp x0, x1, [sp, #-16]!
620		// Set up X16 as a temporary second base pointer for the caller saved things
621		mov x16, sp
622		// Stack pointer unchanged, no stack allocated arguments
623		bl wacc_f
624		mov x16, x0
625		// Stack pointer unchanged, no stack allocated arguments
626		// pop {x0, x1}
627		ldp x0, x1, [sp], #16
628		mov x8, x16
629		mov x19, x8
630		smull x8, w0, w19
631		// take the 31st bit, sign extend it to 64 bits
632		sbfx x9, x8, #31, #1
633		// now take the top 32 bits of the result, shift and sign extend to 64 bits
634		cmp x9, x8, asr #32
635		// if they are not equal then overflow occured
636		b.ne _errOverflow
637		sxtw x8, w8
638		// push {x8}
639		stp x8, xzr, [sp, #-16]!
640		// pop {x8}
641		ldp x8, xzr, [sp], #16
642		cbz x1, _errDivZero
643		sdiv w8, w8, w1
644		sxtw x8, w8
645		// push {x8}
646		stp x8, xzr, [sp, #-16]!
647		// pop {x0}
648		ldp x0, xzr, [sp], #16
649		mov x0, x0
650		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
651		mov sp, fp
652		// pop {x19}
653		ldp x19, xzr, [sp], #16
654		// pop {fp, lr}
655		ldp fp, lr, [sp], #16
656		ret
657		// Stack pointer unchanged, no stack allocated variables
658		// 'ere be dragons: this is 100% dead code, functions always end in returns!
659	
660	wacc_divideByInt:
661		// push {fp, lr}
662		stp fp, lr, [sp, #-16]!
663		mov fp, sp
664		cbz x1, _errDivZero
665		sdiv w8, w0, w1
666		sxtw x8, w8
667		// push {x8}
668		stp x8, xzr, [sp, #-16]!
669		// pop {x0}
670		ldp x0, xzr, [sp], #16
671		mov x0, x0
672		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
673		mov sp, fp
674		// pop {fp, lr}
675		ldp fp, lr, [sp], #16
676		ret
677		// 'ere be dragons: this is 100% dead code, functions always end in returns!
678	
679	// length of .L._prints_str0
680		.word 4
681	.L._prints_str0:
682		.asciz "%.*s"
683	.align 4
684	_prints:
685		// push {lr}
686		stp lr, xzr, [sp, #-16]!
687		mov x2, x0
688		ldrsw x1, [x0, #-4]
689		adr x0, .L._prints_str0
690		bl printf
691		mov x0, #0
692		bl fflush
693		// pop {lr}
694		ldp lr, xzr, [sp], #16
695		ret
696	
697	// length of .L._printi_str0
698		.word 2
699	.L._printi_str0:
700		.asciz "%d"
701	.align 4
702	_printi:
703		// push {lr}
704		stp lr, xzr, [sp, #-16]!
705		mov x1, x0
706		adr x0, .L._printi_str0
707		bl printf
708		mov x0, #0
709		bl fflush
710		// pop {lr}
711		ldp lr, xzr, [sp], #16
712		ret
713	
714	// length of .L._println_str0
715		.word 0
716	.L._println_str0:
717		.asciz ""
718	.align 4
719	_println:
720		// push {lr}
721		stp lr, xzr, [sp, #-16]!
722		adr x0, .L._println_str0
723		bl puts
724		mov x0, #0
725		bl fflush
726		// pop {lr}
727		ldp lr, xzr, [sp], #16
728		ret
729	
730	// length of .L._errDivZero_str0
731		.word 40
732	.L._errDivZero_str0:
733		.asciz "fatal error: division or modulo by zero\n"
734	.align 4
735	_errDivZero:
736		adr x0, .L._errDivZero_str0
737		bl _prints
738		mov w0, #-1
739		bl exit
740	
741	// length of .L._errOverflow_str0
742		.word 52
743	.L._errOverflow_str0:
744		.asciz "fatal error: integer overflow or underflow occurred\n"
745	.align 4
746	_errOverflow:
747		adr x0, .L._errOverflow_str0
748		bl _prints
749		mov w0, #-1
750		bl exit
===========================================================
-- Finished

