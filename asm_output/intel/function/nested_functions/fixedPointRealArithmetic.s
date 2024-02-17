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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 24
5	.L.str0:
6		.asciz "Using fixed-point real: "
7	# length of .L.str1
8		.int 3
9	.L.str1:
10		.asciz " / "
11	# length of .L.str2
12		.int 3
13	.L.str2:
14		.asciz " * "
15	# length of .L.str3
16		.int 3
17	.L.str3:
18		.asciz " = "
19	.text
20	main:
21		push rbp
22		# push {rbx, r12, r13, r14, r15}
23		sub rsp, 40
24		mov qword ptr [rsp], rbx
25		mov qword ptr [rsp + 8], r12
26		mov qword ptr [rsp + 16], r13
27		mov qword ptr [rsp + 24], r14
28		mov qword ptr [rsp + 32], r15
29		mov rbp, rsp
30		# Stack pointer unchanged, no stack allocated variables
31		mov rax, 10
32		mov r12, rax
33		mov rax, 3
34		mov r13, rax
35		# Stack pointer unchanged, no stack allocated arguments
36		lea rax, [rip + .L.str0]
37		push rax
38		pop rax
39		mov rax, rax
40		mov rdi, rax
41		# statement primitives do not return results (but will clobber r0/rax)
42		call _prints
43		# Stack pointer unchanged, no stack allocated arguments
44		mov rax, r12
45		mov rdi, rax
46		# statement primitives do not return results (but will clobber r0/rax)
47		call _printi
48		# Stack pointer unchanged, no stack allocated arguments
49		lea rax, [rip + .L.str1]
50		push rax
51		pop rax
52		mov rax, rax
53		mov rdi, rax
54		# statement primitives do not return results (but will clobber r0/rax)
55		call _prints
56		# Stack pointer unchanged, no stack allocated arguments
57		mov rax, r13
58		mov rdi, rax
59		# statement primitives do not return results (but will clobber r0/rax)
60		call _printi
61		# Stack pointer unchanged, no stack allocated arguments
62		lea rax, [rip + .L.str2]
63		push rax
64		pop rax
65		mov rax, rax
66		mov rdi, rax
67		# statement primitives do not return results (but will clobber r0/rax)
68		call _prints
69		# Stack pointer unchanged, no stack allocated arguments
70		mov rax, r13
71		mov rdi, rax
72		# statement primitives do not return results (but will clobber r0/rax)
73		call _printi
74		# Stack pointer unchanged, no stack allocated arguments
75		lea rax, [rip + .L.str3]
76		push rax
77		pop rax
78		mov rax, rax
79		mov rdi, rax
80		# statement primitives do not return results (but will clobber r0/rax)
81		call _prints
82		# Stack pointer unchanged, no stack allocated arguments
83		mov rax, r12
84		mov rdi, rax
85		call wacc_intToFixedPoint
86		mov r11, rax
87		# Stack pointer unchanged, no stack allocated arguments
88		mov rax, r11
89		mov r14, rax
90		# Stack pointer unchanged, no stack allocated arguments
91		mov rax, r14
92		mov rdi, rax
93		mov rax, r13
94		mov rsi, rax
95		call wacc_divideByInt
96		mov r11, rax
97		# Stack pointer unchanged, no stack allocated arguments
98		mov rax, r11
99		mov r14, rax
100		# Stack pointer unchanged, no stack allocated arguments
101		mov rax, r14
102		mov rdi, rax
103		mov rax, r13
104		mov rsi, rax
105		call wacc_multiplyByInt
106		mov r11, rax
107		# Stack pointer unchanged, no stack allocated arguments
108		mov rax, r11
109		mov r14, rax
110		# Stack pointer unchanged, no stack allocated arguments
111		mov rax, r14
112		mov rdi, rax
113		call wacc_fixedPointToIntRoundNear
114		mov r11, rax
115		# Stack pointer unchanged, no stack allocated arguments
116		mov rax, r11
117		mov r15, rax
118		# Stack pointer unchanged, no stack allocated arguments
119		mov rax, r15
120		mov rdi, rax
121		# statement primitives do not return results (but will clobber r0/rax)
122		call _printi
123		call _println
124		# Stack pointer unchanged, no stack allocated variables
125		mov rax, 0
126		# pop {rbx, r12, r13, r14, r15}
127		mov rbx, qword ptr [rsp]
128		mov r12, qword ptr [rsp + 8]
129		mov r13, qword ptr [rsp + 16]
130		mov r14, qword ptr [rsp + 24]
131		mov r15, qword ptr [rsp + 32]
132		add rsp, 40
133		pop rbp
134		ret
135	
136	wacc_q:
137		push rbp
138		mov rbp, rsp
139		mov rax, 14
140		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
141		mov rsp, rbp
142		pop rbp
143		ret
144		# 'ere be dragons: this is 100% dead code, functions always end in returns!
145	
146	wacc_power:
147		push rbp
148		push r12
149		mov rbp, rsp
150		# Stack pointer unchanged, no stack allocated variables
151		mov rax, 1
152		mov r12, rax
153		jmp .L0
154	.L1:
155		mov eax, r12d
156		imul eax, edi
157		jo _errOverflow
158		movsx rax, eax
159		push rax
160		pop rax
161		mov rax, rax
162		mov r12, rax
163		mov eax, esi
164		sub eax, 1
165		jo _errOverflow
166		movsx rax, eax
167		push rax
168		pop rax
169		mov rax, rax
170		mov rsi, rax
171	.L0:
172		cmp rsi, 0
173		jg .L1
174		mov rax, r12
175		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
176		mov rsp, rbp
177		pop r12
178		pop rbp
179		ret
180		# Stack pointer unchanged, no stack allocated variables
181		# 'ere be dragons: this is 100% dead code, functions always end in returns!
182	
183	wacc_f:
184		push rbp
185		# push {r12, r13}
186		sub rsp, 16
187		mov qword ptr [rsp], r12
188		mov qword ptr [rsp + 8], r13
189		mov rbp, rsp
190		# Stack pointer unchanged, no stack allocated variables
191		# Stack pointer unchanged, no stack allocated arguments
192		call wacc_q
193		mov r11, rax
194		# Stack pointer unchanged, no stack allocated arguments
195		mov rax, r11
196		mov r12, rax
197		# Stack pointer unchanged, no stack allocated arguments
198		mov rax, 2
199		mov rdi, rax
200		mov rax, r12
201		mov rsi, rax
202		call wacc_power
203		mov r11, rax
204		# Stack pointer unchanged, no stack allocated arguments
205		mov rax, r11
206		mov r13, rax
207		mov rax, r13
208		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
209		mov rsp, rbp
210		# pop {r12, r13}
211		mov r12, qword ptr [rsp]
212		mov r13, qword ptr [rsp + 8]
213		add rsp, 16
214		pop rbp
215		ret
216		# Stack pointer unchanged, no stack allocated variables
217		# 'ere be dragons: this is 100% dead code, functions always end in returns!
218	
219	wacc_intToFixedPoint:
220		push rbp
221		push r12
222		mov rbp, rsp
223		# Stack pointer unchanged, no stack allocated variables
224		push rdi
225		# Set up R11 as a temporary second base pointer for the caller saved things
226		mov r11, rsp
227		# Stack pointer unchanged, no stack allocated arguments
228		call wacc_f
229		mov r11, rax
230		# Stack pointer unchanged, no stack allocated arguments
231		pop rdi
232		mov rax, r11
233		mov r12, rax
234		mov eax, edi
235		imul eax, r12d
236		jo _errOverflow
237		movsx rax, eax
238		push rax
239		pop rax
240		mov rax, rax
241		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
242		mov rsp, rbp
243		pop r12
244		pop rbp
245		ret
246		# Stack pointer unchanged, no stack allocated variables
247		# 'ere be dragons: this is 100% dead code, functions always end in returns!
248	
249	wacc_fixedPointToIntRoundDown:
250		push rbp
251		push r12
252		mov rbp, rsp
253		# Stack pointer unchanged, no stack allocated variables
254		push rdi
255		# Set up R11 as a temporary second base pointer for the caller saved things
256		mov r11, rsp
257		# Stack pointer unchanged, no stack allocated arguments
258		call wacc_f
259		mov r11, rax
260		# Stack pointer unchanged, no stack allocated arguments
261		pop rdi
262		mov rax, r11
263		mov r12, rax
264		mov eax, edi
265		cmp r12d, 0
266		je _errDivZero
267		# sign extend EAX into EDX
268		cdq
269		idiv r12d
270		mov eax, eax
271		mov eax, eax
272		movsx rax, eax
273		push rax
274		pop rax
275		mov rax, rax
276		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
277		mov rsp, rbp
278		pop r12
279		pop rbp
280		ret
281		# Stack pointer unchanged, no stack allocated variables
282		# 'ere be dragons: this is 100% dead code, functions always end in returns!
283	
284	wacc_fixedPointToIntRoundNear:
285		push rbp
286		push r12
287		mov rbp, rsp
288		# Stack pointer unchanged, no stack allocated variables
289		push rdi
290		# Set up R11 as a temporary second base pointer for the caller saved things
291		mov r11, rsp
292		# Stack pointer unchanged, no stack allocated arguments
293		call wacc_f
294		mov r11, rax
295		# Stack pointer unchanged, no stack allocated arguments
296		pop rdi
297		mov rax, r11
298		mov r12, rax
299		cmp rdi, 0
300		jge .L2
301		mov rbx, 2
302		mov eax, r12d
303		cmp ebx, 0
304		je _errDivZero
305		# sign extend EAX into EDX
306		cdq
307		idiv ebx
308		mov eax, eax
309		mov eax, eax
310		movsx rax, eax
311		push rax
312		pop rbx
313		mov eax, edi
314		sub eax, ebx
315		jo _errOverflow
316		movsx rax, eax
317		push rax
318		pop rax
319		mov eax, eax
320		cmp r12d, 0
321		je _errDivZero
322		# sign extend EAX into EDX
323		cdq
324		idiv r12d
325		mov eax, eax
326		mov eax, eax
327		movsx rax, eax
328		push rax
329		pop rax
330		mov rax, rax
331		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
332		mov rsp, rbp
333		pop r12
334		pop rbp
335		ret
336		jmp .L3
337	.L2:
338		mov rbx, 2
339		mov eax, r12d
340		cmp ebx, 0
341		je _errDivZero
342		# sign extend EAX into EDX
343		cdq
344		idiv ebx
345		mov eax, eax
346		mov eax, eax
347		movsx rax, eax
348		push rax
349		pop rbx
350		mov eax, edi
351		add eax, ebx
352		jo _errOverflow
353		movsx rax, eax
354		push rax
355		pop rax
356		mov eax, eax
357		cmp r12d, 0
358		je _errDivZero
359		# sign extend EAX into EDX
360		cdq
361		idiv r12d
362		mov eax, eax
363		mov eax, eax
364		movsx rax, eax
365		push rax
366		pop rax
367		mov rax, rax
368		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
369		mov rsp, rbp
370		pop r12
371		pop rbp
372		ret
373	.L3:
374		# Stack pointer unchanged, no stack allocated variables
375		# 'ere be dragons: this is 100% dead code, functions always end in returns!
376	
377	wacc_add:
378		push rbp
379		mov rbp, rsp
380		mov eax, edi
381		add eax, esi
382		jo _errOverflow
383		movsx rax, eax
384		push rax
385		pop rax
386		mov rax, rax
387		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
388		mov rsp, rbp
389		pop rbp
390		ret
391		# 'ere be dragons: this is 100% dead code, functions always end in returns!
392	
393	wacc_subtract:
394		push rbp
395		mov rbp, rsp
396		mov eax, edi
397		sub eax, esi
398		jo _errOverflow
399		movsx rax, eax
400		push rax
401		pop rax
402		mov rax, rax
403		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
404		mov rsp, rbp
405		pop rbp
406		ret
407		# 'ere be dragons: this is 100% dead code, functions always end in returns!
408	
409	wacc_addByInt:
410		push rbp
411		push r12
412		mov rbp, rsp
413		# Stack pointer unchanged, no stack allocated variables
414		# push {rsi, rdi}
415		sub rsp, 16
416		mov qword ptr [rsp], rsi
417		mov qword ptr [rsp + 8], rdi
418		# Set up R11 as a temporary second base pointer for the caller saved things
419		mov r11, rsp
420		# Stack pointer unchanged, no stack allocated arguments
421		call wacc_f
422		mov r11, rax
423		# Stack pointer unchanged, no stack allocated arguments
424		# pop {rsi, rdi}
425		mov rsi, qword ptr [rsp]
426		mov rdi, qword ptr [rsp + 8]
427		add rsp, 16
428		mov rax, r11
429		mov r12, rax
430		mov eax, esi
431		imul eax, r12d
432		jo _errOverflow
433		movsx rax, eax
434		push rax
435		pop rbx
436		mov eax, edi
437		add eax, ebx
438		jo _errOverflow
439		movsx rax, eax
440		push rax
441		pop rax
442		mov rax, rax
443		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
444		mov rsp, rbp
445		pop r12
446		pop rbp
447		ret
448		# Stack pointer unchanged, no stack allocated variables
449		# 'ere be dragons: this is 100% dead code, functions always end in returns!
450	
451	wacc_subtractByInt:
452		push rbp
453		push r12
454		mov rbp, rsp
455		# Stack pointer unchanged, no stack allocated variables
456		# push {rsi, rdi}
457		sub rsp, 16
458		mov qword ptr [rsp], rsi
459		mov qword ptr [rsp + 8], rdi
460		# Set up R11 as a temporary second base pointer for the caller saved things
461		mov r11, rsp
462		# Stack pointer unchanged, no stack allocated arguments
463		call wacc_f
464		mov r11, rax
465		# Stack pointer unchanged, no stack allocated arguments
466		# pop {rsi, rdi}
467		mov rsi, qword ptr [rsp]
468		mov rdi, qword ptr [rsp + 8]
469		add rsp, 16
470		mov rax, r11
471		mov r12, rax
472		mov eax, esi
473		imul eax, r12d
474		jo _errOverflow
475		movsx rax, eax
476		push rax
477		pop rbx
478		mov eax, edi
479		sub eax, ebx
480		jo _errOverflow
481		movsx rax, eax
482		push rax
483		pop rax
484		mov rax, rax
485		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
486		mov rsp, rbp
487		pop r12
488		pop rbp
489		ret
490		# Stack pointer unchanged, no stack allocated variables
491		# 'ere be dragons: this is 100% dead code, functions always end in returns!
492	
493	wacc_multiply:
494		push rbp
495		push r12
496		mov rbp, rsp
497		# Stack pointer unchanged, no stack allocated variables
498		# push {rsi, rdi}
499		sub rsp, 16
500		mov qword ptr [rsp], rsi
501		mov qword ptr [rsp + 8], rdi
502		# Set up R11 as a temporary second base pointer for the caller saved things
503		mov r11, rsp
504		# Stack pointer unchanged, no stack allocated arguments
505		call wacc_f
506		mov r11, rax
507		# Stack pointer unchanged, no stack allocated arguments
508		# pop {rsi, rdi}
509		mov rsi, qword ptr [rsp]
510		mov rdi, qword ptr [rsp + 8]
511		add rsp, 16
512		mov rax, r11
513		mov r12, rax
514		mov eax, edi
515		imul eax, esi
516		jo _errOverflow
517		movsx rax, eax
518		push rax
519		pop rax
520		mov eax, eax
521		cmp r12d, 0
522		je _errDivZero
523		# sign extend EAX into EDX
524		cdq
525		idiv r12d
526		mov eax, eax
527		mov eax, eax
528		movsx rax, eax
529		push rax
530		pop rax
531		mov rax, rax
532		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
533		mov rsp, rbp
534		pop r12
535		pop rbp
536		ret
537		# Stack pointer unchanged, no stack allocated variables
538		# 'ere be dragons: this is 100% dead code, functions always end in returns!
539	
540	wacc_multiplyByInt:
541		push rbp
542		mov rbp, rsp
543		mov eax, edi
544		imul eax, esi
545		jo _errOverflow
546		movsx rax, eax
547		push rax
548		pop rax
549		mov rax, rax
550		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
551		mov rsp, rbp
552		pop rbp
553		ret
554		# 'ere be dragons: this is 100% dead code, functions always end in returns!
555	
556	wacc_divide:
557		push rbp
558		push r12
559		mov rbp, rsp
560		# Stack pointer unchanged, no stack allocated variables
561		# push {rsi, rdi}
562		sub rsp, 16
563		mov qword ptr [rsp], rsi
564		mov qword ptr [rsp + 8], rdi
565		# Set up R11 as a temporary second base pointer for the caller saved things
566		mov r11, rsp
567		# Stack pointer unchanged, no stack allocated arguments
568		call wacc_f
569		mov r11, rax
570		# Stack pointer unchanged, no stack allocated arguments
571		# pop {rsi, rdi}
572		mov rsi, qword ptr [rsp]
573		mov rdi, qword ptr [rsp + 8]
574		add rsp, 16
575		mov rax, r11
576		mov r12, rax
577		mov eax, edi
578		imul eax, r12d
579		jo _errOverflow
580		movsx rax, eax
581		push rax
582		pop rax
583		mov eax, eax
584		cmp esi, 0
585		je _errDivZero
586		# sign extend EAX into EDX
587		cdq
588		idiv esi
589		mov eax, eax
590		mov eax, eax
591		movsx rax, eax
592		push rax
593		pop rax
594		mov rax, rax
595		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
596		mov rsp, rbp
597		pop r12
598		pop rbp
599		ret
600		# Stack pointer unchanged, no stack allocated variables
601		# 'ere be dragons: this is 100% dead code, functions always end in returns!
602	
603	wacc_divideByInt:
604		push rbp
605		mov rbp, rsp
606		mov eax, edi
607		cmp esi, 0
608		je _errDivZero
609		# sign extend EAX into EDX
610		cdq
611		idiv esi
612		mov eax, eax
613		mov eax, eax
614		movsx rax, eax
615		push rax
616		pop rax
617		mov rax, rax
618		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
619		mov rsp, rbp
620		pop rbp
621		ret
622		# 'ere be dragons: this is 100% dead code, functions always end in returns!
623	
624	.section .rodata
625	# length of .L._prints_str0
626		.int 4
627	.L._prints_str0:
628		.asciz "%.*s"
629	.text
630	_prints:
631		push rbp
632		mov rbp, rsp
633		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
634		and rsp, -16
635		mov rdx, rdi
636		mov esi, dword ptr [rdi - 4]
637		lea rdi, [rip + .L._prints_str0]
638		# on x86, al represents the number of SIMD registers used as variadic arguments
639		mov al, 0
640		call printf@plt
641		mov rdi, 0
642		call fflush@plt
643		mov rsp, rbp
644		pop rbp
645		ret
646	
647	.section .rodata
648	# length of .L._printi_str0
649		.int 2
650	.L._printi_str0:
651		.asciz "%d"
652	.text
653	_printi:
654		push rbp
655		mov rbp, rsp
656		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
657		and rsp, -16
658		mov esi, edi
659		lea rdi, [rip + .L._printi_str0]
660		# on x86, al represents the number of SIMD registers used as variadic arguments
661		mov al, 0
662		call printf@plt
663		mov rdi, 0
664		call fflush@plt
665		mov rsp, rbp
666		pop rbp
667		ret
668	
669	.section .rodata
670	# length of .L._println_str0
671		.int 0
672	.L._println_str0:
673		.asciz ""
674	.text
675	_println:
676		push rbp
677		mov rbp, rsp
678		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
679		and rsp, -16
680		lea rdi, [rip + .L._println_str0]
681		call puts@plt
682		mov rdi, 0
683		call fflush@plt
684		mov rsp, rbp
685		pop rbp
686		ret
687	
688	_exit:
689		push rbp
690		mov rbp, rsp
691		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
692		and rsp, -16
693		call exit@plt
694		mov rsp, rbp
695		pop rbp
696		ret
697	
698	.section .rodata
699	# length of .L._errDivZero_str0
700		.int 40
701	.L._errDivZero_str0:
702		.asciz "fatal error: division or modulo by zero\n"
703	.text
704	_errDivZero:
705		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
706		and rsp, -16
707		lea rdi, [rip + .L._errDivZero_str0]
708		call _prints
709		mov dil, -1
710		call exit@plt
711	
712	.section .rodata
713	# length of .L._errOverflow_str0
714		.int 52
715	.L._errOverflow_str0:
716		.asciz "fatal error: integer overflow or underflow occurred\n"
717	.text
718	_errOverflow:
719		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
720		and rsp, -16
721		lea rdi, [rip + .L._errOverflow_str0]
722		call _prints
723		mov dil, -1
724		call exit@plt
===========================================================
-- Finished

