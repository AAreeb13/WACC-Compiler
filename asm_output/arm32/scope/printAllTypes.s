./valid/scope/printAllTypes.wacc
calling the reference compiler on ./valid/scope/printAllTypes.wacc
-- Test: printAllTypes.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Tests scoping with most variable types

# Output:
# ( [1, 2, 3] , [a, b, c] )
# [ #addrs# = (a, true), #addrs# = (b, false) ]
# 1, 2
# array, of, strings
# true, false, true
# xyz
# 1, 2, 3
# this is a string
# true
# x
# 5
#

# Program:

begin

  string comma = ", ";
  int x = 5;
  begin
    char x = 'x';
    begin
      bool x = true;
      begin
        string x = "this is a string";
        begin
          int[] x = [1,2,3];
          begin
            char[] x = ['x', 'y', 'z'];
            begin
              bool[] x = [true, false, true];
              begin
                string[] x = ["array", "of", "strings"];
                begin
                  pair(int, int) x = newpair(1, 2);
                  begin
                    pair(char, bool) y = newpair('a', true);
                    pair(char, bool) z = newpair('b', false);
                    pair(char, bool)[] x = [y, z];
                    begin
                      int[] y = [1, 2, 3];
                      char[] z = ['a', 'b', 'c'];
                      pair(int[], char[]) x = newpair(y, z);
                      begin
                        skip
                      end;
                      int[] a = fst x;
                      char[] b = snd x;
                      print "( [";
                      print a[0];
                      print comma;
                      print a[1];
                      print comma;
                      print a[2];
                      print "] , [";
                      print b[0];
                      print comma;
                      print b[1];
                      print comma;
                      print b[2];
                      println "] )" # ( [fst x] , [snd x] )
                    end;
                    pair(char, bool) a = x[0];
                    char aa = fst a;
                    bool ab = snd a;
                    pair(char, bool) b = x[1];
                    char ba = fst b;
                    bool bb = snd b;
                    print "[ ";
                    print a;
                    print " = (";
                    print aa;
                    print comma;
                    print ab;
                    print "), ";
                    print b;
                    print " = (";
                    print ba;
                    print comma;
                    print bb;
                    println ") ]" # [ x[0], x[1] ]
                  end;
                  int y = fst x;
                  int z = snd x;
                  print y;
                  print comma;
                  println z # fst x, snd x
                end;
                string a = x[0];
                string b = x[1];
                string c = x[2];
                print a;
                print comma;
                print b;
                print comma;
                println c # string[]
              end;
              print x[0];
              print comma;
              print x[1];
              print comma;
              println x[2] # bool[]
            end;
            println x # char[]
          end;
          int a = x[0];
          int b = x[1];
          int c = x[2];
          print a;
          print comma;
          print b;
          print comma;
          println c # int[]
        end;
        println x # string
      end;
      println x # bool
    end;
    println x # char
  end;
  println x # int

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printAllTypes.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 2
3	.L.str0:
4		.asciz ", "
5	@ length of .L.str1
6		.word 16
7	.L.str1:
8		.asciz "this is a string"
9	@ length of .L.str2
10		.word 5
11	.L.str2:
12		.asciz "array"
13	@ length of .L.str3
14		.word 2
15	.L.str3:
16		.asciz "of"
17	@ length of .L.str4
18		.word 7
19	.L.str4:
20		.asciz "strings"
21	@ length of .L.str5
22		.word 3
23	.L.str5:
24		.asciz "( ["
25	@ length of .L.str6
26		.word 5
27	.L.str6:
28		.asciz "] , ["
29	@ length of .L.str7
30		.word 3
31	.L.str7:
32		.asciz "] )"
33	@ length of .L.str8
34		.word 2
35	.L.str8:
36		.asciz "[ "
37	@ length of .L.str9
38		.word 4
39	.L.str9:
40		.asciz " = ("
41	@ length of .L.str10
42		.word 3
43	.L.str10:
44		.asciz "), "
45	@ length of .L.str11
46		.word 3
47	.L.str11:
48		.asciz ") ]"
49	.align 4
50	.text
51	.global main
52	main:
53		push {fp, lr}
54		push {r4, r5, r6, r7, r8, r10, r12}
55		mov fp, sp
56		@ Stack pointer unchanged, no stack allocated variables
57		ldr r8, =.L.str0
58		push {r8}
59		pop {r8}
60		mov r8, r8
61		mov r4, r8
62		mov r8, #5
63		mov r5, r8
64		@ Stack pointer unchanged, no stack allocated variables
65		mov r8, #120
66		mov r6, r8
67		@ Stack pointer unchanged, no stack allocated variables
68		mov r8, #1
69		mov r7, r8
70		@ Stack pointer unchanged, no stack allocated variables
71		ldr r8, =.L.str1
72		push {r8}
73		pop {r8}
74		mov r8, r8
75		mov r0, r8
76		@ Stack padded to a multiple of the required alignment
77		sub sp, sp, #8
78		@ 3 element array
79		push {r0, r1, r2, r3}
80		mov r0, #16
81		bl _malloc
82		mov r12, r0
83		pop {r0, r1, r2, r3}
84		@ array pointers are shifted forwards by 4 bytes (to account for size)
85		add r12, r12, #4
86		mov r8, #3
87		str r8, [r12, #-4]
88		mov r8, #1
89		str r8, [r12, #0]
90		mov r8, #2
91		str r8, [r12, #4]
92		mov r8, #3
93		str r8, [r12, #8]
94		mov r8, r12
95		mov r1, r8
96		@ Parent scope overallocated enough to fix scope, no adjustment required
97		@ 3 element array
98		push {r0, r1, r2, r3}
99		mov r0, #7
100		bl _malloc
101		mov r12, r0
102		pop {r0, r1, r2, r3}
103		@ array pointers are shifted forwards by 4 bytes (to account for size)
104		add r12, r12, #4
105		mov r8, #3
106		str r8, [r12, #-4]
107		mov r8, #120
108		strb r8, [r12, #0]
109		mov r8, #121
110		strb r8, [r12, #1]
111		mov r8, #122
112		strb r8, [r12, #2]
113		mov r8, r12
114		str r8, [fp, #-8]
115		@ Stack padded to a multiple of the required alignment
116		sub sp, sp, #8
117		@ 3 element array
118		push {r0, r1, r2, r3}
119		mov r0, #7
120		bl _malloc
121		mov r12, r0
122		pop {r0, r1, r2, r3}
123		@ array pointers are shifted forwards by 4 bytes (to account for size)
124		add r12, r12, #4
125		mov r8, #3
126		str r8, [r12, #-4]
127		mov r8, #1
128		strb r8, [r12, #0]
129		mov r8, #0
130		strb r8, [r12, #1]
131		mov r8, #1
132		strb r8, [r12, #2]
133		mov r8, r12
134		str r8, [fp, #-12]
135		@ Stack padded to a multiple of the required alignment
136		sub sp, sp, #16
137		@ 3 element array
138		push {r0, r1, r2, r3}
139		mov r0, #16
140		bl _malloc
141		mov r12, r0
142		pop {r0, r1, r2, r3}
143		@ array pointers are shifted forwards by 4 bytes (to account for size)
144		add r12, r12, #4
145		mov r8, #3
146		str r8, [r12, #-4]
147		ldr r8, =.L.str2
148		push {r8}
149		pop {r8}
150		mov r8, r8
151		str r8, [r12, #0]
152		ldr r8, =.L.str3
153		push {r8}
154		pop {r8}
155		mov r8, r8
156		str r8, [r12, #4]
157		ldr r8, =.L.str4
158		push {r8}
159		pop {r8}
160		mov r8, r8
161		str r8, [r12, #8]
162		mov r8, r12
163		str r8, [fp, #-28]
164		@ Stack padded to a multiple of the required alignment
165		sub sp, sp, #12
166		push {r0, r1, r2, r3}
167		mov r0, #8
168		bl _malloc
169		mov r12, r0
170		pop {r0, r1, r2, r3}
171		mov r8, #1
172		str r8, [r12, #0]
173		mov r8, #2
174		str r8, [r12, #4]
175		mov r8, r12
176		str r8, [fp, #-40]
177		@ Stack padded to a multiple of the required alignment
178		sub sp, sp, #24
179		push {r0, r1, r2, r3}
180		mov r0, #8
181		bl _malloc
182		mov r12, r0
183		pop {r0, r1, r2, r3}
184		mov r8, #97
185		str r8, [r12, #0]
186		mov r8, #1
187		str r8, [r12, #4]
188		mov r8, r12
189		str r8, [fp, #-64]
190		push {r0, r1, r2, r3}
191		mov r0, #8
192		bl _malloc
193		mov r12, r0
194		pop {r0, r1, r2, r3}
195		mov r8, #98
196		str r8, [r12, #0]
197		mov r8, #0
198		str r8, [r12, #4]
199		mov r8, r12
200		str r8, [fp, #-60]
201		@ 2 element array
202		push {r0, r1, r2, r3}
203		mov r0, #12
204		bl _malloc
205		mov r12, r0
206		pop {r0, r1, r2, r3}
207		@ array pointers are shifted forwards by 4 bytes (to account for size)
208		add r12, r12, #4
209		mov r8, #2
210		str r8, [r12, #-4]
211		ldr r8, [fp, #-64]
212		mov r8, r8
213		str r8, [r12, #0]
214		ldr r8, [fp, #-60]
215		mov r8, r8
216		str r8, [r12, #4]
217		mov r8, r12
218		str r8, [fp, #-56]
219		@ Stack padded to a multiple of the required alignment
220		sub sp, sp, #20
221		@ 3 element array
222		push {r0, r1, r2, r3}
223		mov r0, #16
224		bl _malloc
225		mov r12, r0
226		pop {r0, r1, r2, r3}
227		@ array pointers are shifted forwards by 4 bytes (to account for size)
228		add r12, r12, #4
229		mov r8, #3
230		str r8, [r12, #-4]
231		mov r8, #1
232		str r8, [r12, #0]
233		mov r8, #2
234		str r8, [r12, #4]
235		mov r8, #3
236		str r8, [r12, #8]
237		mov r8, r12
238		str r8, [fp, #-84]
239		@ 3 element array
240		push {r0, r1, r2, r3}
241		mov r0, #7
242		bl _malloc
243		mov r12, r0
244		pop {r0, r1, r2, r3}
245		@ array pointers are shifted forwards by 4 bytes (to account for size)
246		add r12, r12, #4
247		mov r8, #3
248		str r8, [r12, #-4]
249		mov r8, #97
250		strb r8, [r12, #0]
251		mov r8, #98
252		strb r8, [r12, #1]
253		mov r8, #99
254		strb r8, [r12, #2]
255		mov r8, r12
256		str r8, [fp, #-80]
257		push {r0, r1, r2, r3}
258		mov r0, #8
259		bl _malloc
260		mov r12, r0
261		pop {r0, r1, r2, r3}
262		ldr r8, [fp, #-84]
263		mov r8, r8
264		str r8, [r12, #0]
265		ldr r8, [fp, #-80]
266		mov r8, r8
267		str r8, [r12, #4]
268		mov r8, r12
269		str r8, [fp, #-76]
270		ldr r8, [fp, #-76]
271		cmp r8, #0
272		bleq _errNull
273		ldr r8, [r8, #0]
274		mov r8, r8
275		str r8, [fp, #-72]
276		ldr r8, [fp, #-76]
277		cmp r8, #0
278		bleq _errNull
279		ldr r8, [r8, #4]
280		mov r8, r8
281		str r8, [fp, #-68]
282		push {r0, r1, r2, r3}
283		@ Set up R12 as a temporary second base pointer for the caller saved things
284		mov r12, sp
285		@ Stack pointer unchanged, no stack allocated arguments
286		ldr r8, =.L.str5
287		push {r8}
288		pop {r8}
289		mov r8, r8
290		mov r0, r8
291		@ statement primitives do not return results (but will clobber r0/rax)
292		bl _prints
293		pop {r0, r1, r2, r3}
294		push {r0, r1, r2, r3}
295		@ Set up R12 as a temporary second base pointer for the caller saved things
296		mov r12, sp
297		@ Stack pointer unchanged, no stack allocated arguments
298		mov r10, #0
299		ldr r8, [fp, #-72]
300		push {r3}
301		mov r3, r8
302		bl _arrLoad4
303		mov r8, r3
304		pop {r3}
305		mov r8, r8
306		mov r0, r8
307		@ statement primitives do not return results (but will clobber r0/rax)
308		bl _printi
309		pop {r0, r1, r2, r3}
310		push {r0, r1, r2, r3}
311		@ Set up R12 as a temporary second base pointer for the caller saved things
312		mov r12, sp
313		@ Stack pointer unchanged, no stack allocated arguments
314		mov r8, r4
315		mov r0, r8
316		@ statement primitives do not return results (but will clobber r0/rax)
317		bl _prints
318		pop {r0, r1, r2, r3}
319		push {r0, r1, r2, r3}
320		@ Set up R12 as a temporary second base pointer for the caller saved things
321		mov r12, sp
322		@ Stack pointer unchanged, no stack allocated arguments
323		mov r10, #1
324		ldr r8, [fp, #-72]
325		push {r3}
326		mov r3, r8
327		bl _arrLoad4
328		mov r8, r3
329		pop {r3}
330		mov r8, r8
331		mov r0, r8
332		@ statement primitives do not return results (but will clobber r0/rax)
333		bl _printi
334		pop {r0, r1, r2, r3}
335		push {r0, r1, r2, r3}
336		@ Set up R12 as a temporary second base pointer for the caller saved things
337		mov r12, sp
338		@ Stack pointer unchanged, no stack allocated arguments
339		mov r8, r4
340		mov r0, r8
341		@ statement primitives do not return results (but will clobber r0/rax)
342		bl _prints
343		pop {r0, r1, r2, r3}
344		push {r0, r1, r2, r3}
345		@ Set up R12 as a temporary second base pointer for the caller saved things
346		mov r12, sp
347		@ Stack pointer unchanged, no stack allocated arguments
348		mov r10, #2
349		ldr r8, [fp, #-72]
350		push {r3}
351		mov r3, r8
352		bl _arrLoad4
353		mov r8, r3
354		pop {r3}
355		mov r8, r8
356		mov r0, r8
357		@ statement primitives do not return results (but will clobber r0/rax)
358		bl _printi
359		pop {r0, r1, r2, r3}
360		push {r0, r1, r2, r3}
361		@ Set up R12 as a temporary second base pointer for the caller saved things
362		mov r12, sp
363		@ Stack pointer unchanged, no stack allocated arguments
364		ldr r8, =.L.str6
365		push {r8}
366		pop {r8}
367		mov r8, r8
368		mov r0, r8
369		@ statement primitives do not return results (but will clobber r0/rax)
370		bl _prints
371		pop {r0, r1, r2, r3}
372		push {r0, r1, r2, r3}
373		@ Set up R12 as a temporary second base pointer for the caller saved things
374		mov r12, sp
375		@ Stack pointer unchanged, no stack allocated arguments
376		mov r10, #0
377		ldr r8, [fp, #-68]
378		push {r3}
379		mov r3, r8
380		bl _arrLoad1
381		mov r8, r3
382		pop {r3}
383		mov r8, r8
384		mov r0, r8
385		@ statement primitives do not return results (but will clobber r0/rax)
386		bl _printc
387		pop {r0, r1, r2, r3}
388		push {r0, r1, r2, r3}
389		@ Set up R12 as a temporary second base pointer for the caller saved things
390		mov r12, sp
391		@ Stack pointer unchanged, no stack allocated arguments
392		mov r8, r4
393		mov r0, r8
394		@ statement primitives do not return results (but will clobber r0/rax)
395		bl _prints
396		pop {r0, r1, r2, r3}
397		push {r0, r1, r2, r3}
398		@ Set up R12 as a temporary second base pointer for the caller saved things
399		mov r12, sp
400		@ Stack pointer unchanged, no stack allocated arguments
401		mov r10, #1
402		ldr r8, [fp, #-68]
403		push {r3}
404		mov r3, r8
405		bl _arrLoad1
406		mov r8, r3
407		pop {r3}
408		mov r8, r8
409		mov r0, r8
410		@ statement primitives do not return results (but will clobber r0/rax)
411		bl _printc
412		pop {r0, r1, r2, r3}
413		push {r0, r1, r2, r3}
414		@ Set up R12 as a temporary second base pointer for the caller saved things
415		mov r12, sp
416		@ Stack pointer unchanged, no stack allocated arguments
417		mov r8, r4
418		mov r0, r8
419		@ statement primitives do not return results (but will clobber r0/rax)
420		bl _prints
421		pop {r0, r1, r2, r3}
422		push {r0, r1, r2, r3}
423		@ Set up R12 as a temporary second base pointer for the caller saved things
424		mov r12, sp
425		@ Stack pointer unchanged, no stack allocated arguments
426		mov r10, #2
427		ldr r8, [fp, #-68]
428		push {r3}
429		mov r3, r8
430		bl _arrLoad1
431		mov r8, r3
432		pop {r3}
433		mov r8, r8
434		mov r0, r8
435		@ statement primitives do not return results (but will clobber r0/rax)
436		bl _printc
437		pop {r0, r1, r2, r3}
438		push {r0, r1, r2, r3}
439		@ Set up R12 as a temporary second base pointer for the caller saved things
440		mov r12, sp
441		@ Stack pointer unchanged, no stack allocated arguments
442		ldr r8, =.L.str7
443		push {r8}
444		pop {r8}
445		mov r8, r8
446		mov r0, r8
447		@ statement primitives do not return results (but will clobber r0/rax)
448		bl _prints
449		bl _println
450		pop {r0, r1, r2, r3}
451		@ Stack padded to a multiple of the required alignment
452		add sp, sp, #20
453		mov r10, #0
454		ldr r8, [fp, #-56]
455		push {r3}
456		mov r3, r8
457		bl _arrLoad4
458		mov r8, r3
459		pop {r3}
460		mov r8, r8
461		str r8, [fp, #-52]
462		ldr r8, [fp, #-52]
463		cmp r8, #0
464		bleq _errNull
465		ldr r8, [r8, #0]
466		mov r8, r8
467		strb r8, [fp, #-48]
468		ldr r8, [fp, #-52]
469		cmp r8, #0
470		bleq _errNull
471		ldr r8, [r8, #4]
472		mov r8, r8
473		strb r8, [fp, #-47]
474		mov r10, #1
475		ldr r8, [fp, #-56]
476		push {r3}
477		mov r3, r8
478		bl _arrLoad4
479		mov r8, r3
480		pop {r3}
481		mov r8, r8
482		str r8, [fp, #-46]
483		ldr r8, [fp, #-46]
484		cmp r8, #0
485		bleq _errNull
486		ldr r8, [r8, #0]
487		mov r8, r8
488		strb r8, [fp, #-42]
489		ldr r8, [fp, #-46]
490		cmp r8, #0
491		bleq _errNull
492		ldr r8, [r8, #4]
493		mov r8, r8
494		strb r8, [fp, #-41]
495		push {r0, r1, r2, r3}
496		@ Set up R12 as a temporary second base pointer for the caller saved things
497		mov r12, sp
498		@ Stack pointer unchanged, no stack allocated arguments
499		ldr r8, =.L.str8
500		push {r8}
501		pop {r8}
502		mov r8, r8
503		mov r0, r8
504		@ statement primitives do not return results (but will clobber r0/rax)
505		bl _prints
506		pop {r0, r1, r2, r3}
507		push {r0, r1, r2, r3}
508		@ Set up R12 as a temporary second base pointer for the caller saved things
509		mov r12, sp
510		@ Stack pointer unchanged, no stack allocated arguments
511		ldr r8, [fp, #-52]
512		mov r8, r8
513		mov r0, r8
514		@ statement primitives do not return results (but will clobber r0/rax)
515		bl _printp
516		pop {r0, r1, r2, r3}
517		push {r0, r1, r2, r3}
518		@ Set up R12 as a temporary second base pointer for the caller saved things
519		mov r12, sp
520		@ Stack pointer unchanged, no stack allocated arguments
521		ldr r8, =.L.str9
522		push {r8}
523		pop {r8}
524		mov r8, r8
525		mov r0, r8
526		@ statement primitives do not return results (but will clobber r0/rax)
527		bl _prints
528		pop {r0, r1, r2, r3}
529		push {r0, r1, r2, r3}
530		@ Set up R12 as a temporary second base pointer for the caller saved things
531		mov r12, sp
532		@ Stack pointer unchanged, no stack allocated arguments
533		ldrsb r8, [fp, #-48]
534		mov r8, r8
535		mov r0, r8
536		@ statement primitives do not return results (but will clobber r0/rax)
537		bl _printc
538		pop {r0, r1, r2, r3}
539		push {r0, r1, r2, r3}
540		@ Set up R12 as a temporary second base pointer for the caller saved things
541		mov r12, sp
542		@ Stack pointer unchanged, no stack allocated arguments
543		mov r8, r4
544		mov r0, r8
545		@ statement primitives do not return results (but will clobber r0/rax)
546		bl _prints
547		pop {r0, r1, r2, r3}
548		push {r0, r1, r2, r3}
549		@ Set up R12 as a temporary second base pointer for the caller saved things
550		mov r12, sp
551		@ Stack pointer unchanged, no stack allocated arguments
552		ldrsb r8, [fp, #-47]
553		mov r8, r8
554		mov r0, r8
555		@ statement primitives do not return results (but will clobber r0/rax)
556		bl _printb
557		pop {r0, r1, r2, r3}
558		push {r0, r1, r2, r3}
559		@ Set up R12 as a temporary second base pointer for the caller saved things
560		mov r12, sp
561		@ Stack pointer unchanged, no stack allocated arguments
562		ldr r8, =.L.str10
563		push {r8}
564		pop {r8}
565		mov r8, r8
566		mov r0, r8
567		@ statement primitives do not return results (but will clobber r0/rax)
568		bl _prints
569		pop {r0, r1, r2, r3}
570		push {r0, r1, r2, r3}
571		@ Set up R12 as a temporary second base pointer for the caller saved things
572		mov r12, sp
573		@ Stack pointer unchanged, no stack allocated arguments
574		ldr r8, [fp, #-46]
575		mov r8, r8
576		mov r0, r8
577		@ statement primitives do not return results (but will clobber r0/rax)
578		bl _printp
579		pop {r0, r1, r2, r3}
580		push {r0, r1, r2, r3}
581		@ Set up R12 as a temporary second base pointer for the caller saved things
582		mov r12, sp
583		@ Stack pointer unchanged, no stack allocated arguments
584		ldr r8, =.L.str9
585		push {r8}
586		pop {r8}
587		mov r8, r8
588		mov r0, r8
589		@ statement primitives do not return results (but will clobber r0/rax)
590		bl _prints
591		pop {r0, r1, r2, r3}
592		push {r0, r1, r2, r3}
593		@ Set up R12 as a temporary second base pointer for the caller saved things
594		mov r12, sp
595		@ Stack pointer unchanged, no stack allocated arguments
596		ldrsb r8, [fp, #-42]
597		mov r8, r8
598		mov r0, r8
599		@ statement primitives do not return results (but will clobber r0/rax)
600		bl _printc
601		pop {r0, r1, r2, r3}
602		push {r0, r1, r2, r3}
603		@ Set up R12 as a temporary second base pointer for the caller saved things
604		mov r12, sp
605		@ Stack pointer unchanged, no stack allocated arguments
606		mov r8, r4
607		mov r0, r8
608		@ statement primitives do not return results (but will clobber r0/rax)
609		bl _prints
610		pop {r0, r1, r2, r3}
611		push {r0, r1, r2, r3}
612		@ Set up R12 as a temporary second base pointer for the caller saved things
613		mov r12, sp
614		@ Stack pointer unchanged, no stack allocated arguments
615		ldrsb r8, [fp, #-41]
616		mov r8, r8
617		mov r0, r8
618		@ statement primitives do not return results (but will clobber r0/rax)
619		bl _printb
620		pop {r0, r1, r2, r3}
621		push {r0, r1, r2, r3}
622		@ Set up R12 as a temporary second base pointer for the caller saved things
623		mov r12, sp
624		@ Stack pointer unchanged, no stack allocated arguments
625		ldr r8, =.L.str11
626		push {r8}
627		pop {r8}
628		mov r8, r8
629		mov r0, r8
630		@ statement primitives do not return results (but will clobber r0/rax)
631		bl _prints
632		bl _println
633		pop {r0, r1, r2, r3}
634		@ Stack padded to a multiple of the required alignment
635		add sp, sp, #24
636		ldr r8, [fp, #-40]
637		cmp r8, #0
638		bleq _errNull
639		ldr r8, [r8, #0]
640		mov r8, r8
641		str r8, [fp, #-36]
642		ldr r8, [fp, #-40]
643		cmp r8, #0
644		bleq _errNull
645		ldr r8, [r8, #4]
646		mov r8, r8
647		str r8, [fp, #-32]
648		push {r0, r1, r2, r3}
649		@ Set up R12 as a temporary second base pointer for the caller saved things
650		mov r12, sp
651		@ Stack pointer unchanged, no stack allocated arguments
652		ldr r8, [fp, #-36]
653		mov r8, r8
654		mov r0, r8
655		@ statement primitives do not return results (but will clobber r0/rax)
656		bl _printi
657		pop {r0, r1, r2, r3}
658		push {r0, r1, r2, r3}
659		@ Set up R12 as a temporary second base pointer for the caller saved things
660		mov r12, sp
661		@ Stack pointer unchanged, no stack allocated arguments
662		mov r8, r4
663		mov r0, r8
664		@ statement primitives do not return results (but will clobber r0/rax)
665		bl _prints
666		pop {r0, r1, r2, r3}
667		push {r0, r1, r2, r3}
668		@ Set up R12 as a temporary second base pointer for the caller saved things
669		mov r12, sp
670		@ Stack pointer unchanged, no stack allocated arguments
671		ldr r8, [fp, #-32]
672		mov r8, r8
673		mov r0, r8
674		@ statement primitives do not return results (but will clobber r0/rax)
675		bl _printi
676		bl _println
677		pop {r0, r1, r2, r3}
678		@ Stack padded to a multiple of the required alignment
679		add sp, sp, #12
680		mov r10, #0
681		ldr r8, [fp, #-28]
682		push {r3}
683		mov r3, r8
684		bl _arrLoad4
685		mov r8, r3
686		pop {r3}
687		mov r8, r8
688		str r8, [fp, #-24]
689		mov r10, #1
690		ldr r8, [fp, #-28]
691		push {r3}
692		mov r3, r8
693		bl _arrLoad4
694		mov r8, r3
695		pop {r3}
696		mov r8, r8
697		str r8, [fp, #-20]
698		mov r10, #2
699		ldr r8, [fp, #-28]
700		push {r3}
701		mov r3, r8
702		bl _arrLoad4
703		mov r8, r3
704		pop {r3}
705		mov r8, r8
706		str r8, [fp, #-16]
707		push {r0, r1, r2, r3}
708		@ Set up R12 as a temporary second base pointer for the caller saved things
709		mov r12, sp
710		@ Stack pointer unchanged, no stack allocated arguments
711		ldr r8, [fp, #-24]
712		mov r8, r8
713		mov r0, r8
714		@ statement primitives do not return results (but will clobber r0/rax)
715		bl _prints
716		pop {r0, r1, r2, r3}
717		push {r0, r1, r2, r3}
718		@ Set up R12 as a temporary second base pointer for the caller saved things
719		mov r12, sp
720		@ Stack pointer unchanged, no stack allocated arguments
721		mov r8, r4
722		mov r0, r8
723		@ statement primitives do not return results (but will clobber r0/rax)
724		bl _prints
725		pop {r0, r1, r2, r3}
726		push {r0, r1, r2, r3}
727		@ Set up R12 as a temporary second base pointer for the caller saved things
728		mov r12, sp
729		@ Stack pointer unchanged, no stack allocated arguments
730		ldr r8, [fp, #-20]
731		mov r8, r8
732		mov r0, r8
733		@ statement primitives do not return results (but will clobber r0/rax)
734		bl _prints
735		pop {r0, r1, r2, r3}
736		push {r0, r1, r2, r3}
737		@ Set up R12 as a temporary second base pointer for the caller saved things
738		mov r12, sp
739		@ Stack pointer unchanged, no stack allocated arguments
740		mov r8, r4
741		mov r0, r8
742		@ statement primitives do not return results (but will clobber r0/rax)
743		bl _prints
744		pop {r0, r1, r2, r3}
745		push {r0, r1, r2, r3}
746		@ Set up R12 as a temporary second base pointer for the caller saved things
747		mov r12, sp
748		@ Stack pointer unchanged, no stack allocated arguments
749		ldr r8, [fp, #-16]
750		mov r8, r8
751		mov r0, r8
752		@ statement primitives do not return results (but will clobber r0/rax)
753		bl _prints
754		bl _println
755		pop {r0, r1, r2, r3}
756		@ Stack padded to a multiple of the required alignment
757		add sp, sp, #16
758		push {r0, r1, r2, r3}
759		@ Set up R12 as a temporary second base pointer for the caller saved things
760		mov r12, sp
761		@ Stack pointer unchanged, no stack allocated arguments
762		mov r10, #0
763		ldr r8, [fp, #-12]
764		push {r3}
765		mov r3, r8
766		bl _arrLoad1
767		mov r8, r3
768		pop {r3}
769		mov r8, r8
770		mov r0, r8
771		@ statement primitives do not return results (but will clobber r0/rax)
772		bl _printb
773		pop {r0, r1, r2, r3}
774		push {r0, r1, r2, r3}
775		@ Set up R12 as a temporary second base pointer for the caller saved things
776		mov r12, sp
777		@ Stack pointer unchanged, no stack allocated arguments
778		mov r8, r4
779		mov r0, r8
780		@ statement primitives do not return results (but will clobber r0/rax)
781		bl _prints
782		pop {r0, r1, r2, r3}
783		push {r0, r1, r2, r3}
784		@ Set up R12 as a temporary second base pointer for the caller saved things
785		mov r12, sp
786		@ Stack pointer unchanged, no stack allocated arguments
787		mov r10, #1
788		ldr r8, [fp, #-12]
789		push {r3}
790		mov r3, r8
791		bl _arrLoad1
792		mov r8, r3
793		pop {r3}
794		mov r8, r8
795		mov r0, r8
796		@ statement primitives do not return results (but will clobber r0/rax)
797		bl _printb
798		pop {r0, r1, r2, r3}
799		push {r0, r1, r2, r3}
800		@ Set up R12 as a temporary second base pointer for the caller saved things
801		mov r12, sp
802		@ Stack pointer unchanged, no stack allocated arguments
803		mov r8, r4
804		mov r0, r8
805		@ statement primitives do not return results (but will clobber r0/rax)
806		bl _prints
807		pop {r0, r1, r2, r3}
808		push {r0, r1, r2, r3}
809		@ Set up R12 as a temporary second base pointer for the caller saved things
810		mov r12, sp
811		@ Stack pointer unchanged, no stack allocated arguments
812		mov r10, #2
813		ldr r8, [fp, #-12]
814		push {r3}
815		mov r3, r8
816		bl _arrLoad1
817		mov r8, r3
818		pop {r3}
819		mov r8, r8
820		mov r0, r8
821		@ statement primitives do not return results (but will clobber r0/rax)
822		bl _printb
823		bl _println
824		pop {r0, r1, r2, r3}
825		@ Stack padded to a multiple of the required alignment
826		add sp, sp, #8
827		push {r0, r1, r2, r3}
828		@ Set up R12 as a temporary second base pointer for the caller saved things
829		mov r12, sp
830		@ Stack pointer unchanged, no stack allocated arguments
831		ldr r8, [fp, #-8]
832		mov r8, r8
833		mov r0, r8
834		@ statement primitives do not return results (but will clobber r0/rax)
835		bl _prints
836		bl _println
837		pop {r0, r1, r2, r3}
838		@ Parent scope overallocated enough to fix scope, no adjustment required
839		mov r10, #0
840		push {r3}
841		mov r3, r1
842		bl _arrLoad4
843		mov r8, r3
844		pop {r3}
845		mov r8, r8
846		mov r2, r8
847		mov r10, #1
848		push {r3}
849		mov r3, r1
850		bl _arrLoad4
851		mov r8, r3
852		pop {r3}
853		mov r8, r8
854		mov r3, r8
855		mov r10, #2
856		push {r3}
857		mov r3, r1
858		bl _arrLoad4
859		mov r8, r3
860		pop {r3}
861		mov r8, r8
862		str r8, [fp, #-4]
863		push {r0, r1, r2, r3}
864		@ Set up R12 as a temporary second base pointer for the caller saved things
865		mov r12, sp
866		@ Stack pointer unchanged, no stack allocated arguments
867		mov r8, r2
868		mov r0, r8
869		@ statement primitives do not return results (but will clobber r0/rax)
870		bl _printi
871		pop {r0, r1, r2, r3}
872		push {r0, r1, r2, r3}
873		@ Set up R12 as a temporary second base pointer for the caller saved things
874		mov r12, sp
875		@ Stack pointer unchanged, no stack allocated arguments
876		mov r8, r4
877		mov r0, r8
878		@ statement primitives do not return results (but will clobber r0/rax)
879		bl _prints
880		pop {r0, r1, r2, r3}
881		push {r0, r1, r2, r3}
882		@ Set up R12 as a temporary second base pointer for the caller saved things
883		mov r12, sp
884		@ Stack pointer unchanged, no stack allocated arguments
885		mov r8, r3
886		mov r0, r8
887		@ statement primitives do not return results (but will clobber r0/rax)
888		bl _printi
889		pop {r0, r1, r2, r3}
890		push {r0, r1, r2, r3}
891		@ Set up R12 as a temporary second base pointer for the caller saved things
892		mov r12, sp
893		@ Stack pointer unchanged, no stack allocated arguments
894		mov r8, r4
895		mov r0, r8
896		@ statement primitives do not return results (but will clobber r0/rax)
897		bl _prints
898		pop {r0, r1, r2, r3}
899		push {r0, r1, r2, r3}
900		@ Set up R12 as a temporary second base pointer for the caller saved things
901		mov r12, sp
902		@ Stack pointer unchanged, no stack allocated arguments
903		ldr r8, [fp, #-4]
904		mov r8, r8
905		mov r0, r8
906		@ statement primitives do not return results (but will clobber r0/rax)
907		bl _printi
908		bl _println
909		pop {r0, r1, r2, r3}
910		@ Stack padded to a multiple of the required alignment
911		add sp, sp, #8
912		push {r0, r1, r2, r3}
913		@ Set up R12 as a temporary second base pointer for the caller saved things
914		mov r12, sp
915		@ Stack pointer unchanged, no stack allocated arguments
916		mov r8, r0
917		mov r0, r8
918		@ statement primitives do not return results (but will clobber r0/rax)
919		bl _prints
920		bl _println
921		pop {r0, r1, r2, r3}
922		@ Stack pointer unchanged, no stack allocated variables
923		push {r0, r1, r2, r3}
924		@ Set up R12 as a temporary second base pointer for the caller saved things
925		mov r12, sp
926		@ Stack pointer unchanged, no stack allocated arguments
927		mov r8, r7
928		mov r0, r8
929		@ statement primitives do not return results (but will clobber r0/rax)
930		bl _printb
931		bl _println
932		pop {r0, r1, r2, r3}
933		@ Stack pointer unchanged, no stack allocated variables
934		push {r0, r1, r2, r3}
935		@ Set up R12 as a temporary second base pointer for the caller saved things
936		mov r12, sp
937		@ Stack pointer unchanged, no stack allocated arguments
938		mov r8, r6
939		mov r0, r8
940		@ statement primitives do not return results (but will clobber r0/rax)
941		bl _printc
942		bl _println
943		pop {r0, r1, r2, r3}
944		@ Stack pointer unchanged, no stack allocated variables
945		push {r0, r1, r2, r3}
946		@ Set up R12 as a temporary second base pointer for the caller saved things
947		mov r12, sp
948		@ Stack pointer unchanged, no stack allocated arguments
949		mov r8, r5
950		mov r0, r8
951		@ statement primitives do not return results (but will clobber r0/rax)
952		bl _printi
953		bl _println
954		pop {r0, r1, r2, r3}
955		@ Stack pointer unchanged, no stack allocated variables
956		mov r0, #0
957		pop {r4, r5, r6, r7, r8, r10, r12}
958		pop {fp, pc}
959	
960	@ length of .L._prints_str0
961		.word 4
962	.L._prints_str0:
963		.asciz "%.*s"
964	.align 4
965	_prints:
966		push {fp, lr}
967		mov fp, sp
968		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
969		bic sp, sp, #0x7
970		mov r2, r0
971		ldr r1, [r0, #-4]
972		adr r0, .L._prints_str0
973		bl printf
974		mov r0, #0
975		bl fflush
976		mov sp, fp
977		pop {fp, pc}
978	
979	@ length of .L._printp_str0
980		.word 2
981	.L._printp_str0:
982		.asciz "%p"
983	.align 4
984	_printp:
985		push {fp, lr}
986		mov fp, sp
987		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
988		bic sp, sp, #0x7
989		mov r1, r0
990		adr r0, .L._printp_str0
991		bl printf
992		mov r0, #0
993		bl fflush
994		mov sp, fp
995		pop {fp, pc}
996	
997	_malloc:
998		push {fp, lr}
999		mov fp, sp
1000		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1001		bic sp, sp, #0x7
1002		bl malloc
1003		cmp r0, #0
1004		bleq _errOutOfMemory
1005		mov sp, fp
1006		pop {fp, pc}
1007	
1008	@ length of .L._printi_str0
1009		.word 2
1010	.L._printi_str0:
1011		.asciz "%d"
1012	.align 4
1013	_printi:
1014		push {fp, lr}
1015		mov fp, sp
1016		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1017		bic sp, sp, #0x7
1018		mov r1, r0
1019		adr r0, .L._printi_str0
1020		bl printf
1021		mov r0, #0
1022		bl fflush
1023		mov sp, fp
1024		pop {fp, pc}
1025	
1026	@ length of .L._printb_str0
1027		.word 5
1028	.L._printb_str0:
1029		.asciz "false"
1030	@ length of .L._printb_str1
1031		.word 4
1032	.L._printb_str1:
1033		.asciz "true"
1034	@ length of .L._printb_str2
1035		.word 4
1036	.L._printb_str2:
1037		.asciz "%.*s"
1038	.align 4
1039	_printb:
1040		push {fp, lr}
1041		mov fp, sp
1042		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1043		bic sp, sp, #0x7
1044		cmp r0, #0
1045		bne .L_printb0
1046		adr r2, .L._printb_str0
1047		b .L_printb1
1048	.L_printb0:
1049		adr r2, .L._printb_str1
1050	.L_printb1:
1051		ldr r1, [r2, #-4]
1052		adr r0, .L._printb_str2
1053		bl printf
1054		mov r0, #0
1055		bl fflush
1056		mov sp, fp
1057		pop {fp, pc}
1058	
1059	@ length of .L._printc_str0
1060		.word 2
1061	.L._printc_str0:
1062		.asciz "%c"
1063	.align 4
1064	_printc:
1065		push {fp, lr}
1066		mov fp, sp
1067		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1068		bic sp, sp, #0x7
1069		mov r1, r0
1070		adr r0, .L._printc_str0
1071		bl printf
1072		mov r0, #0
1073		bl fflush
1074		mov sp, fp
1075		pop {fp, pc}
1076	
1077	@ length of .L._println_str0
1078		.word 0
1079	.L._println_str0:
1080		.asciz ""
1081	.align 4
1082	_println:
1083		push {fp, lr}
1084		mov fp, sp
1085		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1086		bic sp, sp, #0x7
1087		adr r0, .L._println_str0
1088		bl puts
1089		mov r0, #0
1090		bl fflush
1091		mov sp, fp
1092		pop {fp, pc}
1093	
1094	_exit:
1095		push {fp, lr}
1096		mov fp, sp
1097		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1098		bic sp, sp, #0x7
1099		bl exit
1100		mov sp, fp
1101		pop {fp, pc}
1102	
1103	_arrLoad4:
1104		@ Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
1105		push {lr}
1106		cmp r10, #0
1107		movlt r1, r10
1108		bllt _errOutOfBounds
1109		ldr lr, [r3, #-4]
1110		cmp r10, lr
1111		movge r1, r10
1112		blge _errOutOfBounds
1113		ldr r3, [r3, r10, lsl #2]
1114		pop {pc}
1115	
1116	_arrLoad1:
1117		@ Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
1118		push {lr}
1119		cmp r10, #0
1120		movlt r1, r10
1121		bllt _errOutOfBounds
1122		ldr lr, [r3, #-4]
1123		cmp r10, lr
1124		movge r1, r10
1125		blge _errOutOfBounds
1126		ldrsb r3, [r3, r10]
1127		pop {pc}
1128	
1129	@ length of .L._errOutOfMemory_str0
1130		.word 27
1131	.L._errOutOfMemory_str0:
1132		.asciz "fatal error: out of memory\n"
1133	.align 4
1134	_errOutOfMemory:
1135		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1136		bic sp, sp, #0x7
1137		adr r0, .L._errOutOfMemory_str0
1138		bl _prints
1139		mov r0, #255
1140		bl _exit
1141	
1142	@ length of .L._errNull_str0
1143		.word 45
1144	.L._errNull_str0:
1145		.asciz "fatal error: null pair dereferenced or freed\n"
1146	.align 4
1147	_errNull:
1148		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1149		bic sp, sp, #0x7
1150		adr r0, .L._errNull_str0
1151		bl _prints
1152		mov r0, #255
1153		bl _exit
1154	
1155	@ length of .L._errOutOfBounds_str0
1156		.word 42
1157	.L._errOutOfBounds_str0:
1158		.asciz "fatal error: array index %d out of bounds\n"
1159	.align 4
1160	_errOutOfBounds:
1161		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
1162		bic sp, sp, #0x7
1163		adr r0, .L._errOutOfBounds_str0
1164		bl printf
1165		mov r0, #0
1166		bl fflush
1167		mov r0, #255
1168		bl _exit
===========================================================
-- Finished

