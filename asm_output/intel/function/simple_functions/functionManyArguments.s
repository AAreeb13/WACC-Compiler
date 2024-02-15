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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 5
5	.L.str0:
6		.asciz "a is "
7	# length of .L.str1
8		.int 5
9	.L.str1:
10		.asciz "b is "
11	# length of .L.str2
12		.int 5
13	.L.str2:
14		.asciz "c is "
15	# length of .L.str3
16		.int 5
17	.L.str3:
18		.asciz "d is "
19	# length of .L.str4
20		.int 5
21	.L.str4:
22		.asciz "e is "
23	# length of .L.str5
24		.int 5
25	.L.str5:
26		.asciz "f is "
27	# length of .L.str6
28		.int 5
29	.L.str6:
30		.asciz "hello"
31	# length of .L.str7
32		.int 10
33	.L.str7:
34		.asciz "answer is "
35	.text
36	main:
37		push rbp
38		# push {rbx, r12, r13, r14}
39		sub rsp, 32
40		mov qword ptr [rsp], rbx
41		mov qword ptr [rsp + 8], r12
42		mov qword ptr [rsp + 16], r13
43		mov qword ptr [rsp + 24], r14
44		mov rbp, rsp
45		# Stack pointer unchanged, no stack allocated variables
46		# 2 element array
47		mov edi, 6
48		call _malloc
49		mov r11, rax
50		# array pointers are shifted forwards by 4 bytes (to account for size)
51		mov r11, r11
52		add r11, 4
53		mov rax, 2
54		mov dword ptr [r11 - 4], eax
55		mov rax, 0
56		mov byte ptr [r11], al
57		mov rax, 1
58		mov byte ptr [r11 + 1], al
59		mov rax, r11
60		mov r12, rax
61		# 2 element array
62		mov edi, 12
63		call _malloc
64		mov r11, rax
65		# array pointers are shifted forwards by 4 bytes (to account for size)
66		mov r11, r11
67		add r11, 4
68		mov rax, 2
69		mov dword ptr [r11 - 4], eax
70		mov rax, 1
71		mov dword ptr [r11], eax
72		mov rax, 2
73		mov dword ptr [r11 + 4], eax
74		mov rax, r11
75		mov r13, rax
76		# Stack pointer unchanged, no stack allocated arguments
77		mov rax, 42
78		mov rdi, rax
79		mov rax, 1
80		mov rsi, rax
81		mov rax, 117
82		mov rdx, rax
83		lea rax, [rip + .L.str6]
84		push rax
85		pop rax
86		mov rax, rax
87		mov rcx, rax
88		mov rax, r12
89		mov r8, rax
90		mov rax, r13
91		mov r9, rax
92		call wacc_doSomething
93		mov r11, rax
94		# Stack pointer unchanged, no stack allocated arguments
95		mov rax, r11
96		mov r14, rax
97		# Stack pointer unchanged, no stack allocated arguments
98		lea rax, [rip + .L.str7]
99		push rax
100		pop rax
101		mov rax, rax
102		mov rdi, rax
103		# statement primitives do not return results (but will clobber r0/rax)
104		call _prints
105		# Stack pointer unchanged, no stack allocated arguments
106		mov rax, r14
107		mov rdi, rax
108		# statement primitives do not return results (but will clobber r0/rax)
109		call _printc
110		call _println
111		# Stack pointer unchanged, no stack allocated variables
112		mov rax, 0
113		# pop {rbx, r12, r13, r14}
114		mov rbx, qword ptr [rsp]
115		mov r12, qword ptr [rsp + 8]
116		mov r13, qword ptr [rsp + 16]
117		mov r14, qword ptr [rsp + 24]
118		add rsp, 32
119		pop rbp
120		ret
121	
122	wacc_doSomething:
123		push rbp
124		mov rbp, rsp
125		# push {rcx, rdx, rsi, rdi, r8, r9}
126		sub rsp, 48
127		mov qword ptr [rsp], rcx
128		mov qword ptr [rsp + 8], rdx
129		mov qword ptr [rsp + 16], rsi
130		mov qword ptr [rsp + 24], rdi
131		mov qword ptr [rsp + 32], r8
132		mov qword ptr [rsp + 40], r9
133		# Set up R11 as a temporary second base pointer for the caller saved things
134		mov r11, rsp
135		# Stack pointer unchanged, no stack allocated arguments
136		lea rax, [rip + .L.str0]
137		push rax
138		pop rax
139		mov rax, rax
140		mov rdi, rax
141		# statement primitives do not return results (but will clobber r0/rax)
142		call _prints
143		# pop {rcx, rdx, rsi, rdi, r8, r9}
144		mov rcx, qword ptr [rsp]
145		mov rdx, qword ptr [rsp + 8]
146		mov rsi, qword ptr [rsp + 16]
147		mov rdi, qword ptr [rsp + 24]
148		mov r8, qword ptr [rsp + 32]
149		mov r9, qword ptr [rsp + 40]
150		add rsp, 48
151		# push {rcx, rdx, rsi, rdi, r8, r9}
152		sub rsp, 48
153		mov qword ptr [rsp], rcx
154		mov qword ptr [rsp + 8], rdx
155		mov qword ptr [rsp + 16], rsi
156		mov qword ptr [rsp + 24], rdi
157		mov qword ptr [rsp + 32], r8
158		mov qword ptr [rsp + 40], r9
159		# Set up R11 as a temporary second base pointer for the caller saved things
160		mov r11, rsp
161		# Stack pointer unchanged, no stack allocated arguments
162		mov rax, rdi
163		mov rdi, rax
164		# statement primitives do not return results (but will clobber r0/rax)
165		call _printi
166		call _println
167		# pop {rcx, rdx, rsi, rdi, r8, r9}
168		mov rcx, qword ptr [rsp]
169		mov rdx, qword ptr [rsp + 8]
170		mov rsi, qword ptr [rsp + 16]
171		mov rdi, qword ptr [rsp + 24]
172		mov r8, qword ptr [rsp + 32]
173		mov r9, qword ptr [rsp + 40]
174		add rsp, 48
175		# push {rcx, rdx, rsi, rdi, r8, r9}
176		sub rsp, 48
177		mov qword ptr [rsp], rcx
178		mov qword ptr [rsp + 8], rdx
179		mov qword ptr [rsp + 16], rsi
180		mov qword ptr [rsp + 24], rdi
181		mov qword ptr [rsp + 32], r8
182		mov qword ptr [rsp + 40], r9
183		# Set up R11 as a temporary second base pointer for the caller saved things
184		mov r11, rsp
185		# Stack pointer unchanged, no stack allocated arguments
186		lea rax, [rip + .L.str1]
187		push rax
188		pop rax
189		mov rax, rax
190		mov rdi, rax
191		# statement primitives do not return results (but will clobber r0/rax)
192		call _prints
193		# pop {rcx, rdx, rsi, rdi, r8, r9}
194		mov rcx, qword ptr [rsp]
195		mov rdx, qword ptr [rsp + 8]
196		mov rsi, qword ptr [rsp + 16]
197		mov rdi, qword ptr [rsp + 24]
198		mov r8, qword ptr [rsp + 32]
199		mov r9, qword ptr [rsp + 40]
200		add rsp, 48
201		# push {rcx, rdx, rsi, rdi, r8, r9}
202		sub rsp, 48
203		mov qword ptr [rsp], rcx
204		mov qword ptr [rsp + 8], rdx
205		mov qword ptr [rsp + 16], rsi
206		mov qword ptr [rsp + 24], rdi
207		mov qword ptr [rsp + 32], r8
208		mov qword ptr [rsp + 40], r9
209		# Set up R11 as a temporary second base pointer for the caller saved things
210		mov r11, rsp
211		# Stack pointer unchanged, no stack allocated arguments
212		mov rax, rsi
213		mov rdi, rax
214		# statement primitives do not return results (but will clobber r0/rax)
215		call _printb
216		call _println
217		# pop {rcx, rdx, rsi, rdi, r8, r9}
218		mov rcx, qword ptr [rsp]
219		mov rdx, qword ptr [rsp + 8]
220		mov rsi, qword ptr [rsp + 16]
221		mov rdi, qword ptr [rsp + 24]
222		mov r8, qword ptr [rsp + 32]
223		mov r9, qword ptr [rsp + 40]
224		add rsp, 48
225		# push {rcx, rdx, rsi, rdi, r8, r9}
226		sub rsp, 48
227		mov qword ptr [rsp], rcx
228		mov qword ptr [rsp + 8], rdx
229		mov qword ptr [rsp + 16], rsi
230		mov qword ptr [rsp + 24], rdi
231		mov qword ptr [rsp + 32], r8
232		mov qword ptr [rsp + 40], r9
233		# Set up R11 as a temporary second base pointer for the caller saved things
234		mov r11, rsp
235		# Stack pointer unchanged, no stack allocated arguments
236		lea rax, [rip + .L.str2]
237		push rax
238		pop rax
239		mov rax, rax
240		mov rdi, rax
241		# statement primitives do not return results (but will clobber r0/rax)
242		call _prints
243		# pop {rcx, rdx, rsi, rdi, r8, r9}
244		mov rcx, qword ptr [rsp]
245		mov rdx, qword ptr [rsp + 8]
246		mov rsi, qword ptr [rsp + 16]
247		mov rdi, qword ptr [rsp + 24]
248		mov r8, qword ptr [rsp + 32]
249		mov r9, qword ptr [rsp + 40]
250		add rsp, 48
251		# push {rcx, rdx, rsi, rdi, r8, r9}
252		sub rsp, 48
253		mov qword ptr [rsp], rcx
254		mov qword ptr [rsp + 8], rdx
255		mov qword ptr [rsp + 16], rsi
256		mov qword ptr [rsp + 24], rdi
257		mov qword ptr [rsp + 32], r8
258		mov qword ptr [rsp + 40], r9
259		# Set up R11 as a temporary second base pointer for the caller saved things
260		mov r11, rsp
261		# Stack pointer unchanged, no stack allocated arguments
262		mov rax, rdx
263		mov rdi, rax
264		# statement primitives do not return results (but will clobber r0/rax)
265		call _printc
266		call _println
267		# pop {rcx, rdx, rsi, rdi, r8, r9}
268		mov rcx, qword ptr [rsp]
269		mov rdx, qword ptr [rsp + 8]
270		mov rsi, qword ptr [rsp + 16]
271		mov rdi, qword ptr [rsp + 24]
272		mov r8, qword ptr [rsp + 32]
273		mov r9, qword ptr [rsp + 40]
274		add rsp, 48
275		# push {rcx, rdx, rsi, rdi, r8, r9}
276		sub rsp, 48
277		mov qword ptr [rsp], rcx
278		mov qword ptr [rsp + 8], rdx
279		mov qword ptr [rsp + 16], rsi
280		mov qword ptr [rsp + 24], rdi
281		mov qword ptr [rsp + 32], r8
282		mov qword ptr [rsp + 40], r9
283		# Set up R11 as a temporary second base pointer for the caller saved things
284		mov r11, rsp
285		# Stack pointer unchanged, no stack allocated arguments
286		lea rax, [rip + .L.str3]
287		push rax
288		pop rax
289		mov rax, rax
290		mov rdi, rax
291		# statement primitives do not return results (but will clobber r0/rax)
292		call _prints
293		# pop {rcx, rdx, rsi, rdi, r8, r9}
294		mov rcx, qword ptr [rsp]
295		mov rdx, qword ptr [rsp + 8]
296		mov rsi, qword ptr [rsp + 16]
297		mov rdi, qword ptr [rsp + 24]
298		mov r8, qword ptr [rsp + 32]
299		mov r9, qword ptr [rsp + 40]
300		add rsp, 48
301		# push {rcx, rdx, rsi, rdi, r8, r9}
302		sub rsp, 48
303		mov qword ptr [rsp], rcx
304		mov qword ptr [rsp + 8], rdx
305		mov qword ptr [rsp + 16], rsi
306		mov qword ptr [rsp + 24], rdi
307		mov qword ptr [rsp + 32], r8
308		mov qword ptr [rsp + 40], r9
309		# Set up R11 as a temporary second base pointer for the caller saved things
310		mov r11, rsp
311		# Stack pointer unchanged, no stack allocated arguments
312		mov rax, rcx
313		mov rdi, rax
314		# statement primitives do not return results (but will clobber r0/rax)
315		call _prints
316		call _println
317		# pop {rcx, rdx, rsi, rdi, r8, r9}
318		mov rcx, qword ptr [rsp]
319		mov rdx, qword ptr [rsp + 8]
320		mov rsi, qword ptr [rsp + 16]
321		mov rdi, qword ptr [rsp + 24]
322		mov r8, qword ptr [rsp + 32]
323		mov r9, qword ptr [rsp + 40]
324		add rsp, 48
325		# push {rcx, rdx, rsi, rdi, r8, r9}
326		sub rsp, 48
327		mov qword ptr [rsp], rcx
328		mov qword ptr [rsp + 8], rdx
329		mov qword ptr [rsp + 16], rsi
330		mov qword ptr [rsp + 24], rdi
331		mov qword ptr [rsp + 32], r8
332		mov qword ptr [rsp + 40], r9
333		# Set up R11 as a temporary second base pointer for the caller saved things
334		mov r11, rsp
335		# Stack pointer unchanged, no stack allocated arguments
336		lea rax, [rip + .L.str4]
337		push rax
338		pop rax
339		mov rax, rax
340		mov rdi, rax
341		# statement primitives do not return results (but will clobber r0/rax)
342		call _prints
343		# pop {rcx, rdx, rsi, rdi, r8, r9}
344		mov rcx, qword ptr [rsp]
345		mov rdx, qword ptr [rsp + 8]
346		mov rsi, qword ptr [rsp + 16]
347		mov rdi, qword ptr [rsp + 24]
348		mov r8, qword ptr [rsp + 32]
349		mov r9, qword ptr [rsp + 40]
350		add rsp, 48
351		# push {rcx, rdx, rsi, rdi, r8, r9}
352		sub rsp, 48
353		mov qword ptr [rsp], rcx
354		mov qword ptr [rsp + 8], rdx
355		mov qword ptr [rsp + 16], rsi
356		mov qword ptr [rsp + 24], rdi
357		mov qword ptr [rsp + 32], r8
358		mov qword ptr [rsp + 40], r9
359		# Set up R11 as a temporary second base pointer for the caller saved things
360		mov r11, rsp
361		# Stack pointer unchanged, no stack allocated arguments
362		mov rax, r8
363		mov rdi, rax
364		# statement primitives do not return results (but will clobber r0/rax)
365		call _printp
366		call _println
367		# pop {rcx, rdx, rsi, rdi, r8, r9}
368		mov rcx, qword ptr [rsp]
369		mov rdx, qword ptr [rsp + 8]
370		mov rsi, qword ptr [rsp + 16]
371		mov rdi, qword ptr [rsp + 24]
372		mov r8, qword ptr [rsp + 32]
373		mov r9, qword ptr [rsp + 40]
374		add rsp, 48
375		# push {rcx, rdx, rsi, rdi, r8, r9}
376		sub rsp, 48
377		mov qword ptr [rsp], rcx
378		mov qword ptr [rsp + 8], rdx
379		mov qword ptr [rsp + 16], rsi
380		mov qword ptr [rsp + 24], rdi
381		mov qword ptr [rsp + 32], r8
382		mov qword ptr [rsp + 40], r9
383		# Set up R11 as a temporary second base pointer for the caller saved things
384		mov r11, rsp
385		# Stack pointer unchanged, no stack allocated arguments
386		lea rax, [rip + .L.str5]
387		push rax
388		pop rax
389		mov rax, rax
390		mov rdi, rax
391		# statement primitives do not return results (but will clobber r0/rax)
392		call _prints
393		# pop {rcx, rdx, rsi, rdi, r8, r9}
394		mov rcx, qword ptr [rsp]
395		mov rdx, qword ptr [rsp + 8]
396		mov rsi, qword ptr [rsp + 16]
397		mov rdi, qword ptr [rsp + 24]
398		mov r8, qword ptr [rsp + 32]
399		mov r9, qword ptr [rsp + 40]
400		add rsp, 48
401		# push {rcx, rdx, rsi, rdi, r8, r9}
402		sub rsp, 48
403		mov qword ptr [rsp], rcx
404		mov qword ptr [rsp + 8], rdx
405		mov qword ptr [rsp + 16], rsi
406		mov qword ptr [rsp + 24], rdi
407		mov qword ptr [rsp + 32], r8
408		mov qword ptr [rsp + 40], r9
409		# Set up R11 as a temporary second base pointer for the caller saved things
410		mov r11, rsp
411		# Stack pointer unchanged, no stack allocated arguments
412		mov rax, r9
413		mov rdi, rax
414		# statement primitives do not return results (but will clobber r0/rax)
415		call _printp
416		call _println
417		# pop {rcx, rdx, rsi, rdi, r8, r9}
418		mov rcx, qword ptr [rsp]
419		mov rdx, qword ptr [rsp + 8]
420		mov rsi, qword ptr [rsp + 16]
421		mov rdi, qword ptr [rsp + 24]
422		mov r8, qword ptr [rsp + 32]
423		mov r9, qword ptr [rsp + 40]
424		add rsp, 48
425		mov rax, 103
426		# reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
427		mov rsp, rbp
428		pop rbp
429		ret
430		# 'ere be dragons: this is 100% dead code, functions always end in returns!
431	
432	.section .rodata
433	# length of .L._prints_str0
434		.int 4
435	.L._prints_str0:
436		.asciz "%.*s"
437	.text
438	_prints:
439		push rbp
440		mov rbp, rsp
441		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
442		and rsp, -16
443		mov rdx, rdi
444		mov esi, dword ptr [rdi - 4]
445		lea rdi, [rip + .L._prints_str0]
446		# on x86, al represents the number of SIMD registers used as variadic arguments
447		mov al, 0
448		call printf@plt
449		mov rdi, 0
450		call fflush@plt
451		mov rsp, rbp
452		pop rbp
453		ret
454	
455	.section .rodata
456	# length of .L._printp_str0
457		.int 2
458	.L._printp_str0:
459		.asciz "%p"
460	.text
461	_printp:
462		push rbp
463		mov rbp, rsp
464		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
465		and rsp, -16
466		mov rsi, rdi
467		lea rdi, [rip + .L._printp_str0]
468		# on x86, al represents the number of SIMD registers used as variadic arguments
469		mov al, 0
470		call printf@plt
471		mov rdi, 0
472		call fflush@plt
473		mov rsp, rbp
474		pop rbp
475		ret
476	
477	_malloc:
478		push rbp
479		mov rbp, rsp
480		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
481		and rsp, -16
482		call malloc@plt
483		cmp rax, 0
484		je _errOutOfMemory
485		mov rsp, rbp
486		pop rbp
487		ret
488	
489	.section .rodata
490	# length of .L._printi_str0
491		.int 2
492	.L._printi_str0:
493		.asciz "%d"
494	.text
495	_printi:
496		push rbp
497		mov rbp, rsp
498		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
499		and rsp, -16
500		mov esi, edi
501		lea rdi, [rip + .L._printi_str0]
502		# on x86, al represents the number of SIMD registers used as variadic arguments
503		mov al, 0
504		call printf@plt
505		mov rdi, 0
506		call fflush@plt
507		mov rsp, rbp
508		pop rbp
509		ret
510	
511	.section .rodata
512	# length of .L._printb_str0
513		.int 5
514	.L._printb_str0:
515		.asciz "false"
516	# length of .L._printb_str1
517		.int 4
518	.L._printb_str1:
519		.asciz "true"
520	# length of .L._printb_str2
521		.int 4
522	.L._printb_str2:
523		.asciz "%.*s"
524	.text
525	_printb:
526		push rbp
527		mov rbp, rsp
528		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
529		and rsp, -16
530		cmp dil, 0
531		jne .L_printb0
532		lea rdx, [rip + .L._printb_str0]
533		jmp .L_printb1
534	.L_printb0:
535		lea rdx, [rip + .L._printb_str1]
536	.L_printb1:
537		mov esi, dword ptr [rdx - 4]
538		lea rdi, [rip + .L._printb_str2]
539		# on x86, al represents the number of SIMD registers used as variadic arguments
540		mov al, 0
541		call printf@plt
542		mov rdi, 0
543		call fflush@plt
544		mov rsp, rbp
545		pop rbp
546		ret
547	
548	.section .rodata
549	# length of .L._printc_str0
550		.int 2
551	.L._printc_str0:
552		.asciz "%c"
553	.text
554	_printc:
555		push rbp
556		mov rbp, rsp
557		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
558		and rsp, -16
559		mov sil, dil
560		lea rdi, [rip + .L._printc_str0]
561		# on x86, al represents the number of SIMD registers used as variadic arguments
562		mov al, 0
563		call printf@plt
564		mov rdi, 0
565		call fflush@plt
566		mov rsp, rbp
567		pop rbp
568		ret
569	
570	.section .rodata
571	# length of .L._println_str0
572		.int 0
573	.L._println_str0:
574		.asciz ""
575	.text
576	_println:
577		push rbp
578		mov rbp, rsp
579		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
580		and rsp, -16
581		lea rdi, [rip + .L._println_str0]
582		call puts@plt
583		mov rdi, 0
584		call fflush@plt
585		mov rsp, rbp
586		pop rbp
587		ret
588	
589	_exit:
590		push rbp
591		mov rbp, rsp
592		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
593		and rsp, -16
594		call exit@plt
595		mov rsp, rbp
596		pop rbp
597		ret
598	
599	.section .rodata
600	# length of .L._errOutOfMemory_str0
601		.int 27
602	.L._errOutOfMemory_str0:
603		.asciz "fatal error: out of memory\n"
604	.text
605	_errOutOfMemory:
606		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
607		and rsp, -16
608		lea rdi, [rip + .L._errOutOfMemory_str0]
609		call _prints
610		mov dil, -1
611		call exit@plt
===========================================================
-- Finished

