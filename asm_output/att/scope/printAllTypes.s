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
0	.globl main
1	.section .rodata
2	# length of .L.str0
3		.int 2
4	.L.str0:
5		.asciz ", "
6	# length of .L.str1
7		.int 16
8	.L.str1:
9		.asciz "this is a string"
10	# length of .L.str2
11		.int 5
12	.L.str2:
13		.asciz "array"
14	# length of .L.str3
15		.int 2
16	.L.str3:
17		.asciz "of"
18	# length of .L.str4
19		.int 7
20	.L.str4:
21		.asciz "strings"
22	# length of .L.str5
23		.int 3
24	.L.str5:
25		.asciz "( ["
26	# length of .L.str6
27		.int 5
28	.L.str6:
29		.asciz "] , ["
30	# length of .L.str7
31		.int 3
32	.L.str7:
33		.asciz "] )"
34	# length of .L.str8
35		.int 2
36	.L.str8:
37		.asciz "[ "
38	# length of .L.str9
39		.int 4
40	.L.str9:
41		.asciz " = ("
42	# length of .L.str10
43		.int 3
44	.L.str10:
45		.asciz "), "
46	# length of .L.str11
47		.int 3
48	.L.str11:
49		.asciz ") ]"
50	.text
51	main:
52		pushq %rbp
53		# pushq {%rbx, %r12, %r13, %r14, %r15}
54		subq $40, %rsp
55		movq %rbx, (%rsp)
56		movq %r12, 8(%rsp)
57		movq %r13, 16(%rsp)
58		movq %r14, 24(%rsp)
59		movq %r15, 32(%rsp)
60		movq %rsp, %rbp
61		# Stack pointer unchanged, no stack allocated variables
62		leaq .L.str0(%rip), %rax
63		pushq %rax
64		popq %rax
65		movq %rax, %rax
66		movq %rax, %r12
67		movq $5, %rax
68		movq %rax, %r13
69		# Stack pointer unchanged, no stack allocated variables
70		movq $120, %rax
71		movq %rax, %r14
72		# Stack pointer unchanged, no stack allocated variables
73		movq $1, %rax
74		movq %rax, %r15
75		# Stack pointer unchanged, no stack allocated variables
76		leaq .L.str1(%rip), %rax
77		pushq %rax
78		popq %rax
79		movq %rax, %rax
80		movq %rax, %rcx
81		# Stack pointer unchanged, no stack allocated variables
82		# 3 element array
83		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
84		subq $48, %rsp
85		movq %rcx, (%rsp)
86		movq %rdx, 8(%rsp)
87		movq %rsi, 16(%rsp)
88		movq %rdi, 24(%rsp)
89		movq %r8, 32(%rsp)
90		movq %r9, 40(%rsp)
91		movl $16, %edi
92		call _malloc
93		movq %rax, %r11
94		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
95		movq (%rsp), %rcx
96		movq 8(%rsp), %rdx
97		movq 16(%rsp), %rsi
98		movq 24(%rsp), %rdi
99		movq 32(%rsp), %r8
100		movq 40(%rsp), %r9
101		addq $48, %rsp
102		# array pointers are shifted forwards by 4 bytes (to account for size)
103		movq %r11, %r11
104		addq $4, %r11
105		movq $3, %rax
106		movl %eax, -4(%r11)
107		movq $1, %rax
108		movl %eax, (%r11)
109		movq $2, %rax
110		movl %eax, 4(%r11)
111		movq $3, %rax
112		movl %eax, 8(%r11)
113		movq %r11, %rax
114		movq %rax, %rdx
115		# Stack pointer unchanged, no stack allocated variables
116		# 3 element array
117		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
118		subq $48, %rsp
119		movq %rcx, (%rsp)
120		movq %rdx, 8(%rsp)
121		movq %rsi, 16(%rsp)
122		movq %rdi, 24(%rsp)
123		movq %r8, 32(%rsp)
124		movq %r9, 40(%rsp)
125		movl $7, %edi
126		call _malloc
127		movq %rax, %r11
128		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
129		movq (%rsp), %rcx
130		movq 8(%rsp), %rdx
131		movq 16(%rsp), %rsi
132		movq 24(%rsp), %rdi
133		movq 32(%rsp), %r8
134		movq 40(%rsp), %r9
135		addq $48, %rsp
136		# array pointers are shifted forwards by 4 bytes (to account for size)
137		movq %r11, %r11
138		addq $4, %r11
139		movq $3, %rax
140		movl %eax, -4(%r11)
141		movq $120, %rax
142		movb %al, (%r11)
143		movq $121, %rax
144		movb %al, 1(%r11)
145		movq $122, %rax
146		movb %al, 2(%r11)
147		movq %r11, %rax
148		movq %rax, %r9
149		subq $8, %rsp
150		# 3 element array
151		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
152		subq $48, %rsp
153		movq %rcx, (%rsp)
154		movq %rdx, 8(%rsp)
155		movq %rsi, 16(%rsp)
156		movq %rdi, 24(%rsp)
157		movq %r8, 32(%rsp)
158		movq %r9, 40(%rsp)
159		movl $7, %edi
160		call _malloc
161		movq %rax, %r11
162		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
163		movq (%rsp), %rcx
164		movq 8(%rsp), %rdx
165		movq 16(%rsp), %rsi
166		movq 24(%rsp), %rdi
167		movq 32(%rsp), %r8
168		movq 40(%rsp), %r9
169		addq $48, %rsp
170		# array pointers are shifted forwards by 4 bytes (to account for size)
171		movq %r11, %r11
172		addq $4, %r11
173		movq $3, %rax
174		movl %eax, -4(%r11)
175		movq $1, %rax
176		movb %al, (%r11)
177		movq $0, %rax
178		movb %al, 1(%r11)
179		movq $1, %rax
180		movb %al, 2(%r11)
181		movq %r11, %rax
182		movq %rax, -8(%rbp)
183		subq $32, %rsp
184		# 3 element array
185		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
186		subq $48, %rsp
187		movq %rcx, (%rsp)
188		movq %rdx, 8(%rsp)
189		movq %rsi, 16(%rsp)
190		movq %rdi, 24(%rsp)
191		movq %r8, 32(%rsp)
192		movq %r9, 40(%rsp)
193		movl $28, %edi
194		call _malloc
195		movq %rax, %r11
196		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
197		movq (%rsp), %rcx
198		movq 8(%rsp), %rdx
199		movq 16(%rsp), %rsi
200		movq 24(%rsp), %rdi
201		movq 32(%rsp), %r8
202		movq 40(%rsp), %r9
203		addq $48, %rsp
204		# array pointers are shifted forwards by 4 bytes (to account for size)
205		movq %r11, %r11
206		addq $4, %r11
207		movq $3, %rax
208		movl %eax, -4(%r11)
209		leaq .L.str2(%rip), %rax
210		pushq %rax
211		popq %rax
212		movq %rax, %rax
213		movq %rax, (%r11)
214		leaq .L.str3(%rip), %rax
215		pushq %rax
216		popq %rax
217		movq %rax, %rax
218		movq %rax, 8(%r11)
219		leaq .L.str4(%rip), %rax
220		pushq %rax
221		popq %rax
222		movq %rax, %rax
223		movq %rax, 16(%r11)
224		movq %r11, %rax
225		movq %rax, -40(%rbp)
226		subq $16, %rsp
227		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
228		subq $48, %rsp
229		movq %rcx, (%rsp)
230		movq %rdx, 8(%rsp)
231		movq %rsi, 16(%rsp)
232		movq %rdi, 24(%rsp)
233		movq %r8, 32(%rsp)
234		movq %r9, 40(%rsp)
235		movl $16, %edi
236		call _malloc
237		movq %rax, %r11
238		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
239		movq (%rsp), %rcx
240		movq 8(%rsp), %rdx
241		movq 16(%rsp), %rsi
242		movq 24(%rsp), %rdi
243		movq 32(%rsp), %r8
244		movq 40(%rsp), %r9
245		addq $48, %rsp
246		movq $1, %rax
247		movq %rax, (%r11)
248		movq $2, %rax
249		movq %rax, 8(%r11)
250		movq %r11, %rax
251		movq %rax, -56(%rbp)
252		subq $44, %rsp
253		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
254		subq $48, %rsp
255		movq %rcx, (%rsp)
256		movq %rdx, 8(%rsp)
257		movq %rsi, 16(%rsp)
258		movq %rdi, 24(%rsp)
259		movq %r8, 32(%rsp)
260		movq %r9, 40(%rsp)
261		movl $16, %edi
262		call _malloc
263		movq %rax, %r11
264		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
265		movq (%rsp), %rcx
266		movq 8(%rsp), %rdx
267		movq 16(%rsp), %rsi
268		movq 24(%rsp), %rdi
269		movq 32(%rsp), %r8
270		movq 40(%rsp), %r9
271		addq $48, %rsp
272		movq $97, %rax
273		movq %rax, (%r11)
274		movq $1, %rax
275		movq %rax, 8(%r11)
276		movq %r11, %rax
277		movq %rax, -100(%rbp)
278		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
279		subq $48, %rsp
280		movq %rcx, (%rsp)
281		movq %rdx, 8(%rsp)
282		movq %rsi, 16(%rsp)
283		movq %rdi, 24(%rsp)
284		movq %r8, 32(%rsp)
285		movq %r9, 40(%rsp)
286		movl $16, %edi
287		call _malloc
288		movq %rax, %r11
289		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
290		movq (%rsp), %rcx
291		movq 8(%rsp), %rdx
292		movq 16(%rsp), %rsi
293		movq 24(%rsp), %rdi
294		movq 32(%rsp), %r8
295		movq 40(%rsp), %r9
296		addq $48, %rsp
297		movq $98, %rax
298		movq %rax, (%r11)
299		movq $0, %rax
300		movq %rax, 8(%r11)
301		movq %r11, %rax
302		movq %rax, -92(%rbp)
303		# 2 element array
304		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
305		subq $48, %rsp
306		movq %rcx, (%rsp)
307		movq %rdx, 8(%rsp)
308		movq %rsi, 16(%rsp)
309		movq %rdi, 24(%rsp)
310		movq %r8, 32(%rsp)
311		movq %r9, 40(%rsp)
312		movl $20, %edi
313		call _malloc
314		movq %rax, %r11
315		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
316		movq (%rsp), %rcx
317		movq 8(%rsp), %rdx
318		movq 16(%rsp), %rsi
319		movq 24(%rsp), %rdi
320		movq 32(%rsp), %r8
321		movq 40(%rsp), %r9
322		addq $48, %rsp
323		# array pointers are shifted forwards by 4 bytes (to account for size)
324		movq %r11, %r11
325		addq $4, %r11
326		movq $2, %rax
327		movl %eax, -4(%r11)
328		movq -100(%rbp), %rax
329		movq %rax, %rax
330		movq %rax, (%r11)
331		movq -92(%rbp), %rax
332		movq %rax, %rax
333		movq %rax, 8(%r11)
334		movq %r11, %rax
335		movq %rax, -84(%rbp)
336		subq $40, %rsp
337		# 3 element array
338		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
339		subq $48, %rsp
340		movq %rcx, (%rsp)
341		movq %rdx, 8(%rsp)
342		movq %rsi, 16(%rsp)
343		movq %rdi, 24(%rsp)
344		movq %r8, 32(%rsp)
345		movq %r9, 40(%rsp)
346		movl $16, %edi
347		call _malloc
348		movq %rax, %r11
349		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
350		movq (%rsp), %rcx
351		movq 8(%rsp), %rdx
352		movq 16(%rsp), %rsi
353		movq 24(%rsp), %rdi
354		movq 32(%rsp), %r8
355		movq 40(%rsp), %r9
356		addq $48, %rsp
357		# array pointers are shifted forwards by 4 bytes (to account for size)
358		movq %r11, %r11
359		addq $4, %r11
360		movq $3, %rax
361		movl %eax, -4(%r11)
362		movq $1, %rax
363		movl %eax, (%r11)
364		movq $2, %rax
365		movl %eax, 4(%r11)
366		movq $3, %rax
367		movl %eax, 8(%r11)
368		movq %r11, %rax
369		movq %rax, -140(%rbp)
370		# 3 element array
371		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
372		subq $48, %rsp
373		movq %rcx, (%rsp)
374		movq %rdx, 8(%rsp)
375		movq %rsi, 16(%rsp)
376		movq %rdi, 24(%rsp)
377		movq %r8, 32(%rsp)
378		movq %r9, 40(%rsp)
379		movl $7, %edi
380		call _malloc
381		movq %rax, %r11
382		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
383		movq (%rsp), %rcx
384		movq 8(%rsp), %rdx
385		movq 16(%rsp), %rsi
386		movq 24(%rsp), %rdi
387		movq 32(%rsp), %r8
388		movq 40(%rsp), %r9
389		addq $48, %rsp
390		# array pointers are shifted forwards by 4 bytes (to account for size)
391		movq %r11, %r11
392		addq $4, %r11
393		movq $3, %rax
394		movl %eax, -4(%r11)
395		movq $97, %rax
396		movb %al, (%r11)
397		movq $98, %rax
398		movb %al, 1(%r11)
399		movq $99, %rax
400		movb %al, 2(%r11)
401		movq %r11, %rax
402		movq %rax, -132(%rbp)
403		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
404		subq $48, %rsp
405		movq %rcx, (%rsp)
406		movq %rdx, 8(%rsp)
407		movq %rsi, 16(%rsp)
408		movq %rdi, 24(%rsp)
409		movq %r8, 32(%rsp)
410		movq %r9, 40(%rsp)
411		movl $16, %edi
412		call _malloc
413		movq %rax, %r11
414		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
415		movq (%rsp), %rcx
416		movq 8(%rsp), %rdx
417		movq 16(%rsp), %rsi
418		movq 24(%rsp), %rdi
419		movq 32(%rsp), %r8
420		movq 40(%rsp), %r9
421		addq $48, %rsp
422		movq -140(%rbp), %rax
423		movq %rax, %rax
424		movq %rax, (%r11)
425		movq -132(%rbp), %rax
426		movq %rax, %rax
427		movq %rax, 8(%r11)
428		movq %r11, %rax
429		movq %rax, -124(%rbp)
430		movq -124(%rbp), %rax
431		cmpq $0, %rax
432		je _errNull
433		movq (%rax), %rax
434		movq %rax, %rax
435		movq %rax, -116(%rbp)
436		movq -124(%rbp), %rax
437		cmpq $0, %rax
438		je _errNull
439		movq 8(%rax), %rax
440		movq %rax, %rax
441		movq %rax, -108(%rbp)
442		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
443		subq $48, %rsp
444		movq %rcx, (%rsp)
445		movq %rdx, 8(%rsp)
446		movq %rsi, 16(%rsp)
447		movq %rdi, 24(%rsp)
448		movq %r8, 32(%rsp)
449		movq %r9, 40(%rsp)
450		# Set up R11 as a temporary second base pointer for the caller saved things
451		movq %rsp, %r11
452		# Stack pointer unchanged, no stack allocated arguments
453		leaq .L.str5(%rip), %rax
454		pushq %rax
455		popq %rax
456		movq %rax, %rax
457		movq %rax, %rdi
458		# statement primitives do not return results (but will clobber r0/rax)
459		call _prints
460		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
461		movq (%rsp), %rcx
462		movq 8(%rsp), %rdx
463		movq 16(%rsp), %rsi
464		movq 24(%rsp), %rdi
465		movq 32(%rsp), %r8
466		movq 40(%rsp), %r9
467		addq $48, %rsp
468		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
469		subq $48, %rsp
470		movq %rcx, (%rsp)
471		movq %rdx, 8(%rsp)
472		movq %rsi, 16(%rsp)
473		movq %rdi, 24(%rsp)
474		movq %r8, 32(%rsp)
475		movq %r9, 40(%rsp)
476		# Set up R11 as a temporary second base pointer for the caller saved things
477		movq %rsp, %r11
478		# Stack pointer unchanged, no stack allocated arguments
479		movl $0, %r10d
480		movq -116(%rbp), %rax
481		pushq %r9
482		movq %rax, %r9
483		call _arrLoad4
484		movl %r9d, %eax
485		popq %r9
486		movq %rax, %rax
487		movq %rax, %rdi
488		# statement primitives do not return results (but will clobber r0/rax)
489		call _printi
490		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
491		movq (%rsp), %rcx
492		movq 8(%rsp), %rdx
493		movq 16(%rsp), %rsi
494		movq 24(%rsp), %rdi
495		movq 32(%rsp), %r8
496		movq 40(%rsp), %r9
497		addq $48, %rsp
498		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
499		subq $48, %rsp
500		movq %rcx, (%rsp)
501		movq %rdx, 8(%rsp)
502		movq %rsi, 16(%rsp)
503		movq %rdi, 24(%rsp)
504		movq %r8, 32(%rsp)
505		movq %r9, 40(%rsp)
506		# Set up R11 as a temporary second base pointer for the caller saved things
507		movq %rsp, %r11
508		# Stack pointer unchanged, no stack allocated arguments
509		movq %r12, %rax
510		movq %rax, %rdi
511		# statement primitives do not return results (but will clobber r0/rax)
512		call _prints
513		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
514		movq (%rsp), %rcx
515		movq 8(%rsp), %rdx
516		movq 16(%rsp), %rsi
517		movq 24(%rsp), %rdi
518		movq 32(%rsp), %r8
519		movq 40(%rsp), %r9
520		addq $48, %rsp
521		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
522		subq $48, %rsp
523		movq %rcx, (%rsp)
524		movq %rdx, 8(%rsp)
525		movq %rsi, 16(%rsp)
526		movq %rdi, 24(%rsp)
527		movq %r8, 32(%rsp)
528		movq %r9, 40(%rsp)
529		# Set up R11 as a temporary second base pointer for the caller saved things
530		movq %rsp, %r11
531		# Stack pointer unchanged, no stack allocated arguments
532		movl $1, %r10d
533		movq -116(%rbp), %rax
534		pushq %r9
535		movq %rax, %r9
536		call _arrLoad4
537		movl %r9d, %eax
538		popq %r9
539		movq %rax, %rax
540		movq %rax, %rdi
541		# statement primitives do not return results (but will clobber r0/rax)
542		call _printi
543		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
544		movq (%rsp), %rcx
545		movq 8(%rsp), %rdx
546		movq 16(%rsp), %rsi
547		movq 24(%rsp), %rdi
548		movq 32(%rsp), %r8
549		movq 40(%rsp), %r9
550		addq $48, %rsp
551		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
552		subq $48, %rsp
553		movq %rcx, (%rsp)
554		movq %rdx, 8(%rsp)
555		movq %rsi, 16(%rsp)
556		movq %rdi, 24(%rsp)
557		movq %r8, 32(%rsp)
558		movq %r9, 40(%rsp)
559		# Set up R11 as a temporary second base pointer for the caller saved things
560		movq %rsp, %r11
561		# Stack pointer unchanged, no stack allocated arguments
562		movq %r12, %rax
563		movq %rax, %rdi
564		# statement primitives do not return results (but will clobber r0/rax)
565		call _prints
566		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
567		movq (%rsp), %rcx
568		movq 8(%rsp), %rdx
569		movq 16(%rsp), %rsi
570		movq 24(%rsp), %rdi
571		movq 32(%rsp), %r8
572		movq 40(%rsp), %r9
573		addq $48, %rsp
574		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
575		subq $48, %rsp
576		movq %rcx, (%rsp)
577		movq %rdx, 8(%rsp)
578		movq %rsi, 16(%rsp)
579		movq %rdi, 24(%rsp)
580		movq %r8, 32(%rsp)
581		movq %r9, 40(%rsp)
582		# Set up R11 as a temporary second base pointer for the caller saved things
583		movq %rsp, %r11
584		# Stack pointer unchanged, no stack allocated arguments
585		movl $2, %r10d
586		movq -116(%rbp), %rax
587		pushq %r9
588		movq %rax, %r9
589		call _arrLoad4
590		movl %r9d, %eax
591		popq %r9
592		movq %rax, %rax
593		movq %rax, %rdi
594		# statement primitives do not return results (but will clobber r0/rax)
595		call _printi
596		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
597		movq (%rsp), %rcx
598		movq 8(%rsp), %rdx
599		movq 16(%rsp), %rsi
600		movq 24(%rsp), %rdi
601		movq 32(%rsp), %r8
602		movq 40(%rsp), %r9
603		addq $48, %rsp
604		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
605		subq $48, %rsp
606		movq %rcx, (%rsp)
607		movq %rdx, 8(%rsp)
608		movq %rsi, 16(%rsp)
609		movq %rdi, 24(%rsp)
610		movq %r8, 32(%rsp)
611		movq %r9, 40(%rsp)
612		# Set up R11 as a temporary second base pointer for the caller saved things
613		movq %rsp, %r11
614		# Stack pointer unchanged, no stack allocated arguments
615		leaq .L.str6(%rip), %rax
616		pushq %rax
617		popq %rax
618		movq %rax, %rax
619		movq %rax, %rdi
620		# statement primitives do not return results (but will clobber r0/rax)
621		call _prints
622		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
623		movq (%rsp), %rcx
624		movq 8(%rsp), %rdx
625		movq 16(%rsp), %rsi
626		movq 24(%rsp), %rdi
627		movq 32(%rsp), %r8
628		movq 40(%rsp), %r9
629		addq $48, %rsp
630		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
631		subq $48, %rsp
632		movq %rcx, (%rsp)
633		movq %rdx, 8(%rsp)
634		movq %rsi, 16(%rsp)
635		movq %rdi, 24(%rsp)
636		movq %r8, 32(%rsp)
637		movq %r9, 40(%rsp)
638		# Set up R11 as a temporary second base pointer for the caller saved things
639		movq %rsp, %r11
640		# Stack pointer unchanged, no stack allocated arguments
641		movl $0, %r10d
642		movq -108(%rbp), %rax
643		pushq %r9
644		movq %rax, %r9
645		call _arrLoad1
646		movb %r9b, %al
647		popq %r9
648		movq %rax, %rax
649		movq %rax, %rdi
650		# statement primitives do not return results (but will clobber r0/rax)
651		call _printc
652		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
653		movq (%rsp), %rcx
654		movq 8(%rsp), %rdx
655		movq 16(%rsp), %rsi
656		movq 24(%rsp), %rdi
657		movq 32(%rsp), %r8
658		movq 40(%rsp), %r9
659		addq $48, %rsp
660		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
661		subq $48, %rsp
662		movq %rcx, (%rsp)
663		movq %rdx, 8(%rsp)
664		movq %rsi, 16(%rsp)
665		movq %rdi, 24(%rsp)
666		movq %r8, 32(%rsp)
667		movq %r9, 40(%rsp)
668		# Set up R11 as a temporary second base pointer for the caller saved things
669		movq %rsp, %r11
670		# Stack pointer unchanged, no stack allocated arguments
671		movq %r12, %rax
672		movq %rax, %rdi
673		# statement primitives do not return results (but will clobber r0/rax)
674		call _prints
675		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
676		movq (%rsp), %rcx
677		movq 8(%rsp), %rdx
678		movq 16(%rsp), %rsi
679		movq 24(%rsp), %rdi
680		movq 32(%rsp), %r8
681		movq 40(%rsp), %r9
682		addq $48, %rsp
683		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
684		subq $48, %rsp
685		movq %rcx, (%rsp)
686		movq %rdx, 8(%rsp)
687		movq %rsi, 16(%rsp)
688		movq %rdi, 24(%rsp)
689		movq %r8, 32(%rsp)
690		movq %r9, 40(%rsp)
691		# Set up R11 as a temporary second base pointer for the caller saved things
692		movq %rsp, %r11
693		# Stack pointer unchanged, no stack allocated arguments
694		movl $1, %r10d
695		movq -108(%rbp), %rax
696		pushq %r9
697		movq %rax, %r9
698		call _arrLoad1
699		movb %r9b, %al
700		popq %r9
701		movq %rax, %rax
702		movq %rax, %rdi
703		# statement primitives do not return results (but will clobber r0/rax)
704		call _printc
705		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
706		movq (%rsp), %rcx
707		movq 8(%rsp), %rdx
708		movq 16(%rsp), %rsi
709		movq 24(%rsp), %rdi
710		movq 32(%rsp), %r8
711		movq 40(%rsp), %r9
712		addq $48, %rsp
713		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
714		subq $48, %rsp
715		movq %rcx, (%rsp)
716		movq %rdx, 8(%rsp)
717		movq %rsi, 16(%rsp)
718		movq %rdi, 24(%rsp)
719		movq %r8, 32(%rsp)
720		movq %r9, 40(%rsp)
721		# Set up R11 as a temporary second base pointer for the caller saved things
722		movq %rsp, %r11
723		# Stack pointer unchanged, no stack allocated arguments
724		movq %r12, %rax
725		movq %rax, %rdi
726		# statement primitives do not return results (but will clobber r0/rax)
727		call _prints
728		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
729		movq (%rsp), %rcx
730		movq 8(%rsp), %rdx
731		movq 16(%rsp), %rsi
732		movq 24(%rsp), %rdi
733		movq 32(%rsp), %r8
734		movq 40(%rsp), %r9
735		addq $48, %rsp
736		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
737		subq $48, %rsp
738		movq %rcx, (%rsp)
739		movq %rdx, 8(%rsp)
740		movq %rsi, 16(%rsp)
741		movq %rdi, 24(%rsp)
742		movq %r8, 32(%rsp)
743		movq %r9, 40(%rsp)
744		# Set up R11 as a temporary second base pointer for the caller saved things
745		movq %rsp, %r11
746		# Stack pointer unchanged, no stack allocated arguments
747		movl $2, %r10d
748		movq -108(%rbp), %rax
749		pushq %r9
750		movq %rax, %r9
751		call _arrLoad1
752		movb %r9b, %al
753		popq %r9
754		movq %rax, %rax
755		movq %rax, %rdi
756		# statement primitives do not return results (but will clobber r0/rax)
757		call _printc
758		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
759		movq (%rsp), %rcx
760		movq 8(%rsp), %rdx
761		movq 16(%rsp), %rsi
762		movq 24(%rsp), %rdi
763		movq 32(%rsp), %r8
764		movq 40(%rsp), %r9
765		addq $48, %rsp
766		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
767		subq $48, %rsp
768		movq %rcx, (%rsp)
769		movq %rdx, 8(%rsp)
770		movq %rsi, 16(%rsp)
771		movq %rdi, 24(%rsp)
772		movq %r8, 32(%rsp)
773		movq %r9, 40(%rsp)
774		# Set up R11 as a temporary second base pointer for the caller saved things
775		movq %rsp, %r11
776		# Stack pointer unchanged, no stack allocated arguments
777		leaq .L.str7(%rip), %rax
778		pushq %rax
779		popq %rax
780		movq %rax, %rax
781		movq %rax, %rdi
782		# statement primitives do not return results (but will clobber r0/rax)
783		call _prints
784		call _println
785		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
786		movq (%rsp), %rcx
787		movq 8(%rsp), %rdx
788		movq 16(%rsp), %rsi
789		movq 24(%rsp), %rdi
790		movq 32(%rsp), %r8
791		movq 40(%rsp), %r9
792		addq $48, %rsp
793		addq $40, %rsp
794		movl $0, %r10d
795		movq -84(%rbp), %rax
796		pushq %r9
797		movq %rax, %r9
798		call _arrLoad8
799		movq %r9, %rax
800		popq %r9
801		movq %rax, %rax
802		movq %rax, -76(%rbp)
803		movq -76(%rbp), %rax
804		cmpq $0, %rax
805		je _errNull
806		movq (%rax), %rax
807		movq %rax, %rax
808		movb %al, -68(%rbp)
809		movq -76(%rbp), %rax
810		cmpq $0, %rax
811		je _errNull
812		movq 8(%rax), %rax
813		movq %rax, %rax
814		movb %al, -67(%rbp)
815		movl $1, %r10d
816		movq -84(%rbp), %rax
817		pushq %r9
818		movq %rax, %r9
819		call _arrLoad8
820		movq %r9, %rax
821		popq %r9
822		movq %rax, %rax
823		movq %rax, -66(%rbp)
824		movq -66(%rbp), %rax
825		cmpq $0, %rax
826		je _errNull
827		movq (%rax), %rax
828		movq %rax, %rax
829		movb %al, -58(%rbp)
830		movq -66(%rbp), %rax
831		cmpq $0, %rax
832		je _errNull
833		movq 8(%rax), %rax
834		movq %rax, %rax
835		movb %al, -57(%rbp)
836		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
837		subq $48, %rsp
838		movq %rcx, (%rsp)
839		movq %rdx, 8(%rsp)
840		movq %rsi, 16(%rsp)
841		movq %rdi, 24(%rsp)
842		movq %r8, 32(%rsp)
843		movq %r9, 40(%rsp)
844		# Set up R11 as a temporary second base pointer for the caller saved things
845		movq %rsp, %r11
846		# Stack pointer unchanged, no stack allocated arguments
847		leaq .L.str8(%rip), %rax
848		pushq %rax
849		popq %rax
850		movq %rax, %rax
851		movq %rax, %rdi
852		# statement primitives do not return results (but will clobber r0/rax)
853		call _prints
854		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
855		movq (%rsp), %rcx
856		movq 8(%rsp), %rdx
857		movq 16(%rsp), %rsi
858		movq 24(%rsp), %rdi
859		movq 32(%rsp), %r8
860		movq 40(%rsp), %r9
861		addq $48, %rsp
862		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
863		subq $48, %rsp
864		movq %rcx, (%rsp)
865		movq %rdx, 8(%rsp)
866		movq %rsi, 16(%rsp)
867		movq %rdi, 24(%rsp)
868		movq %r8, 32(%rsp)
869		movq %r9, 40(%rsp)
870		# Set up R11 as a temporary second base pointer for the caller saved things
871		movq %rsp, %r11
872		# Stack pointer unchanged, no stack allocated arguments
873		movq -76(%rbp), %rax
874		movq %rax, %rax
875		movq %rax, %rdi
876		# statement primitives do not return results (but will clobber r0/rax)
877		call _printp
878		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
879		movq (%rsp), %rcx
880		movq 8(%rsp), %rdx
881		movq 16(%rsp), %rsi
882		movq 24(%rsp), %rdi
883		movq 32(%rsp), %r8
884		movq 40(%rsp), %r9
885		addq $48, %rsp
886		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
887		subq $48, %rsp
888		movq %rcx, (%rsp)
889		movq %rdx, 8(%rsp)
890		movq %rsi, 16(%rsp)
891		movq %rdi, 24(%rsp)
892		movq %r8, 32(%rsp)
893		movq %r9, 40(%rsp)
894		# Set up R11 as a temporary second base pointer for the caller saved things
895		movq %rsp, %r11
896		# Stack pointer unchanged, no stack allocated arguments
897		leaq .L.str9(%rip), %rax
898		pushq %rax
899		popq %rax
900		movq %rax, %rax
901		movq %rax, %rdi
902		# statement primitives do not return results (but will clobber r0/rax)
903		call _prints
904		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
905		movq (%rsp), %rcx
906		movq 8(%rsp), %rdx
907		movq 16(%rsp), %rsi
908		movq 24(%rsp), %rdi
909		movq 32(%rsp), %r8
910		movq 40(%rsp), %r9
911		addq $48, %rsp
912		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
913		subq $48, %rsp
914		movq %rcx, (%rsp)
915		movq %rdx, 8(%rsp)
916		movq %rsi, 16(%rsp)
917		movq %rdi, 24(%rsp)
918		movq %r8, 32(%rsp)
919		movq %r9, 40(%rsp)
920		# Set up R11 as a temporary second base pointer for the caller saved things
921		movq %rsp, %r11
922		# Stack pointer unchanged, no stack allocated arguments
923		movsbq -68(%rbp), %rax
924		movq %rax, %rax
925		movq %rax, %rdi
926		# statement primitives do not return results (but will clobber r0/rax)
927		call _printc
928		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
929		movq (%rsp), %rcx
930		movq 8(%rsp), %rdx
931		movq 16(%rsp), %rsi
932		movq 24(%rsp), %rdi
933		movq 32(%rsp), %r8
934		movq 40(%rsp), %r9
935		addq $48, %rsp
936		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
937		subq $48, %rsp
938		movq %rcx, (%rsp)
939		movq %rdx, 8(%rsp)
940		movq %rsi, 16(%rsp)
941		movq %rdi, 24(%rsp)
942		movq %r8, 32(%rsp)
943		movq %r9, 40(%rsp)
944		# Set up R11 as a temporary second base pointer for the caller saved things
945		movq %rsp, %r11
946		# Stack pointer unchanged, no stack allocated arguments
947		movq %r12, %rax
948		movq %rax, %rdi
949		# statement primitives do not return results (but will clobber r0/rax)
950		call _prints
951		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
952		movq (%rsp), %rcx
953		movq 8(%rsp), %rdx
954		movq 16(%rsp), %rsi
955		movq 24(%rsp), %rdi
956		movq 32(%rsp), %r8
957		movq 40(%rsp), %r9
958		addq $48, %rsp
959		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
960		subq $48, %rsp
961		movq %rcx, (%rsp)
962		movq %rdx, 8(%rsp)
963		movq %rsi, 16(%rsp)
964		movq %rdi, 24(%rsp)
965		movq %r8, 32(%rsp)
966		movq %r9, 40(%rsp)
967		# Set up R11 as a temporary second base pointer for the caller saved things
968		movq %rsp, %r11
969		# Stack pointer unchanged, no stack allocated arguments
970		movsbq -67(%rbp), %rax
971		movq %rax, %rax
972		movq %rax, %rdi
973		# statement primitives do not return results (but will clobber r0/rax)
974		call _printb
975		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
976		movq (%rsp), %rcx
977		movq 8(%rsp), %rdx
978		movq 16(%rsp), %rsi
979		movq 24(%rsp), %rdi
980		movq 32(%rsp), %r8
981		movq 40(%rsp), %r9
982		addq $48, %rsp
983		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
984		subq $48, %rsp
985		movq %rcx, (%rsp)
986		movq %rdx, 8(%rsp)
987		movq %rsi, 16(%rsp)
988		movq %rdi, 24(%rsp)
989		movq %r8, 32(%rsp)
990		movq %r9, 40(%rsp)
991		# Set up R11 as a temporary second base pointer for the caller saved things
992		movq %rsp, %r11
993		# Stack pointer unchanged, no stack allocated arguments
994		leaq .L.str10(%rip), %rax
995		pushq %rax
996		popq %rax
997		movq %rax, %rax
998		movq %rax, %rdi
999		# statement primitives do not return results (but will clobber r0/rax)
1000		call _prints
1001		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1002		movq (%rsp), %rcx
1003		movq 8(%rsp), %rdx
1004		movq 16(%rsp), %rsi
1005		movq 24(%rsp), %rdi
1006		movq 32(%rsp), %r8
1007		movq 40(%rsp), %r9
1008		addq $48, %rsp
1009		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1010		subq $48, %rsp
1011		movq %rcx, (%rsp)
1012		movq %rdx, 8(%rsp)
1013		movq %rsi, 16(%rsp)
1014		movq %rdi, 24(%rsp)
1015		movq %r8, 32(%rsp)
1016		movq %r9, 40(%rsp)
1017		# Set up R11 as a temporary second base pointer for the caller saved things
1018		movq %rsp, %r11
1019		# Stack pointer unchanged, no stack allocated arguments
1020		movq -66(%rbp), %rax
1021		movq %rax, %rax
1022		movq %rax, %rdi
1023		# statement primitives do not return results (but will clobber r0/rax)
1024		call _printp
1025		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1026		movq (%rsp), %rcx
1027		movq 8(%rsp), %rdx
1028		movq 16(%rsp), %rsi
1029		movq 24(%rsp), %rdi
1030		movq 32(%rsp), %r8
1031		movq 40(%rsp), %r9
1032		addq $48, %rsp
1033		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1034		subq $48, %rsp
1035		movq %rcx, (%rsp)
1036		movq %rdx, 8(%rsp)
1037		movq %rsi, 16(%rsp)
1038		movq %rdi, 24(%rsp)
1039		movq %r8, 32(%rsp)
1040		movq %r9, 40(%rsp)
1041		# Set up R11 as a temporary second base pointer for the caller saved things
1042		movq %rsp, %r11
1043		# Stack pointer unchanged, no stack allocated arguments
1044		leaq .L.str9(%rip), %rax
1045		pushq %rax
1046		popq %rax
1047		movq %rax, %rax
1048		movq %rax, %rdi
1049		# statement primitives do not return results (but will clobber r0/rax)
1050		call _prints
1051		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1052		movq (%rsp), %rcx
1053		movq 8(%rsp), %rdx
1054		movq 16(%rsp), %rsi
1055		movq 24(%rsp), %rdi
1056		movq 32(%rsp), %r8
1057		movq 40(%rsp), %r9
1058		addq $48, %rsp
1059		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1060		subq $48, %rsp
1061		movq %rcx, (%rsp)
1062		movq %rdx, 8(%rsp)
1063		movq %rsi, 16(%rsp)
1064		movq %rdi, 24(%rsp)
1065		movq %r8, 32(%rsp)
1066		movq %r9, 40(%rsp)
1067		# Set up R11 as a temporary second base pointer for the caller saved things
1068		movq %rsp, %r11
1069		# Stack pointer unchanged, no stack allocated arguments
1070		movsbq -58(%rbp), %rax
1071		movq %rax, %rax
1072		movq %rax, %rdi
1073		# statement primitives do not return results (but will clobber r0/rax)
1074		call _printc
1075		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1076		movq (%rsp), %rcx
1077		movq 8(%rsp), %rdx
1078		movq 16(%rsp), %rsi
1079		movq 24(%rsp), %rdi
1080		movq 32(%rsp), %r8
1081		movq 40(%rsp), %r9
1082		addq $48, %rsp
1083		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1084		subq $48, %rsp
1085		movq %rcx, (%rsp)
1086		movq %rdx, 8(%rsp)
1087		movq %rsi, 16(%rsp)
1088		movq %rdi, 24(%rsp)
1089		movq %r8, 32(%rsp)
1090		movq %r9, 40(%rsp)
1091		# Set up R11 as a temporary second base pointer for the caller saved things
1092		movq %rsp, %r11
1093		# Stack pointer unchanged, no stack allocated arguments
1094		movq %r12, %rax
1095		movq %rax, %rdi
1096		# statement primitives do not return results (but will clobber r0/rax)
1097		call _prints
1098		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1099		movq (%rsp), %rcx
1100		movq 8(%rsp), %rdx
1101		movq 16(%rsp), %rsi
1102		movq 24(%rsp), %rdi
1103		movq 32(%rsp), %r8
1104		movq 40(%rsp), %r9
1105		addq $48, %rsp
1106		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1107		subq $48, %rsp
1108		movq %rcx, (%rsp)
1109		movq %rdx, 8(%rsp)
1110		movq %rsi, 16(%rsp)
1111		movq %rdi, 24(%rsp)
1112		movq %r8, 32(%rsp)
1113		movq %r9, 40(%rsp)
1114		# Set up R11 as a temporary second base pointer for the caller saved things
1115		movq %rsp, %r11
1116		# Stack pointer unchanged, no stack allocated arguments
1117		movsbq -57(%rbp), %rax
1118		movq %rax, %rax
1119		movq %rax, %rdi
1120		# statement primitives do not return results (but will clobber r0/rax)
1121		call _printb
1122		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1123		movq (%rsp), %rcx
1124		movq 8(%rsp), %rdx
1125		movq 16(%rsp), %rsi
1126		movq 24(%rsp), %rdi
1127		movq 32(%rsp), %r8
1128		movq 40(%rsp), %r9
1129		addq $48, %rsp
1130		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1131		subq $48, %rsp
1132		movq %rcx, (%rsp)
1133		movq %rdx, 8(%rsp)
1134		movq %rsi, 16(%rsp)
1135		movq %rdi, 24(%rsp)
1136		movq %r8, 32(%rsp)
1137		movq %r9, 40(%rsp)
1138		# Set up R11 as a temporary second base pointer for the caller saved things
1139		movq %rsp, %r11
1140		# Stack pointer unchanged, no stack allocated arguments
1141		leaq .L.str11(%rip), %rax
1142		pushq %rax
1143		popq %rax
1144		movq %rax, %rax
1145		movq %rax, %rdi
1146		# statement primitives do not return results (but will clobber r0/rax)
1147		call _prints
1148		call _println
1149		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1150		movq (%rsp), %rcx
1151		movq 8(%rsp), %rdx
1152		movq 16(%rsp), %rsi
1153		movq 24(%rsp), %rdi
1154		movq 32(%rsp), %r8
1155		movq 40(%rsp), %r9
1156		addq $48, %rsp
1157		addq $44, %rsp
1158		movq -56(%rbp), %rax
1159		cmpq $0, %rax
1160		je _errNull
1161		movq (%rax), %rax
1162		movq %rax, %rax
1163		movl %eax, -48(%rbp)
1164		movq -56(%rbp), %rax
1165		cmpq $0, %rax
1166		je _errNull
1167		movq 8(%rax), %rax
1168		movq %rax, %rax
1169		movl %eax, -44(%rbp)
1170		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1171		subq $48, %rsp
1172		movq %rcx, (%rsp)
1173		movq %rdx, 8(%rsp)
1174		movq %rsi, 16(%rsp)
1175		movq %rdi, 24(%rsp)
1176		movq %r8, 32(%rsp)
1177		movq %r9, 40(%rsp)
1178		# Set up R11 as a temporary second base pointer for the caller saved things
1179		movq %rsp, %r11
1180		# Stack pointer unchanged, no stack allocated arguments
1181		movslq -48(%rbp), %rax
1182		movq %rax, %rax
1183		movq %rax, %rdi
1184		# statement primitives do not return results (but will clobber r0/rax)
1185		call _printi
1186		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1187		movq (%rsp), %rcx
1188		movq 8(%rsp), %rdx
1189		movq 16(%rsp), %rsi
1190		movq 24(%rsp), %rdi
1191		movq 32(%rsp), %r8
1192		movq 40(%rsp), %r9
1193		addq $48, %rsp
1194		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1195		subq $48, %rsp
1196		movq %rcx, (%rsp)
1197		movq %rdx, 8(%rsp)
1198		movq %rsi, 16(%rsp)
1199		movq %rdi, 24(%rsp)
1200		movq %r8, 32(%rsp)
1201		movq %r9, 40(%rsp)
1202		# Set up R11 as a temporary second base pointer for the caller saved things
1203		movq %rsp, %r11
1204		# Stack pointer unchanged, no stack allocated arguments
1205		movq %r12, %rax
1206		movq %rax, %rdi
1207		# statement primitives do not return results (but will clobber r0/rax)
1208		call _prints
1209		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1210		movq (%rsp), %rcx
1211		movq 8(%rsp), %rdx
1212		movq 16(%rsp), %rsi
1213		movq 24(%rsp), %rdi
1214		movq 32(%rsp), %r8
1215		movq 40(%rsp), %r9
1216		addq $48, %rsp
1217		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1218		subq $48, %rsp
1219		movq %rcx, (%rsp)
1220		movq %rdx, 8(%rsp)
1221		movq %rsi, 16(%rsp)
1222		movq %rdi, 24(%rsp)
1223		movq %r8, 32(%rsp)
1224		movq %r9, 40(%rsp)
1225		# Set up R11 as a temporary second base pointer for the caller saved things
1226		movq %rsp, %r11
1227		# Stack pointer unchanged, no stack allocated arguments
1228		movslq -44(%rbp), %rax
1229		movq %rax, %rax
1230		movq %rax, %rdi
1231		# statement primitives do not return results (but will clobber r0/rax)
1232		call _printi
1233		call _println
1234		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1235		movq (%rsp), %rcx
1236		movq 8(%rsp), %rdx
1237		movq 16(%rsp), %rsi
1238		movq 24(%rsp), %rdi
1239		movq 32(%rsp), %r8
1240		movq 40(%rsp), %r9
1241		addq $48, %rsp
1242		addq $16, %rsp
1243		movl $0, %r10d
1244		movq -40(%rbp), %rax
1245		pushq %r9
1246		movq %rax, %r9
1247		call _arrLoad8
1248		movq %r9, %rax
1249		popq %r9
1250		movq %rax, %rax
1251		movq %rax, -32(%rbp)
1252		movl $1, %r10d
1253		movq -40(%rbp), %rax
1254		pushq %r9
1255		movq %rax, %r9
1256		call _arrLoad8
1257		movq %r9, %rax
1258		popq %r9
1259		movq %rax, %rax
1260		movq %rax, -24(%rbp)
1261		movl $2, %r10d
1262		movq -40(%rbp), %rax
1263		pushq %r9
1264		movq %rax, %r9
1265		call _arrLoad8
1266		movq %r9, %rax
1267		popq %r9
1268		movq %rax, %rax
1269		movq %rax, -16(%rbp)
1270		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1271		subq $48, %rsp
1272		movq %rcx, (%rsp)
1273		movq %rdx, 8(%rsp)
1274		movq %rsi, 16(%rsp)
1275		movq %rdi, 24(%rsp)
1276		movq %r8, 32(%rsp)
1277		movq %r9, 40(%rsp)
1278		# Set up R11 as a temporary second base pointer for the caller saved things
1279		movq %rsp, %r11
1280		# Stack pointer unchanged, no stack allocated arguments
1281		movq -32(%rbp), %rax
1282		movq %rax, %rax
1283		movq %rax, %rdi
1284		# statement primitives do not return results (but will clobber r0/rax)
1285		call _prints
1286		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1287		movq (%rsp), %rcx
1288		movq 8(%rsp), %rdx
1289		movq 16(%rsp), %rsi
1290		movq 24(%rsp), %rdi
1291		movq 32(%rsp), %r8
1292		movq 40(%rsp), %r9
1293		addq $48, %rsp
1294		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1295		subq $48, %rsp
1296		movq %rcx, (%rsp)
1297		movq %rdx, 8(%rsp)
1298		movq %rsi, 16(%rsp)
1299		movq %rdi, 24(%rsp)
1300		movq %r8, 32(%rsp)
1301		movq %r9, 40(%rsp)
1302		# Set up R11 as a temporary second base pointer for the caller saved things
1303		movq %rsp, %r11
1304		# Stack pointer unchanged, no stack allocated arguments
1305		movq %r12, %rax
1306		movq %rax, %rdi
1307		# statement primitives do not return results (but will clobber r0/rax)
1308		call _prints
1309		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1310		movq (%rsp), %rcx
1311		movq 8(%rsp), %rdx
1312		movq 16(%rsp), %rsi
1313		movq 24(%rsp), %rdi
1314		movq 32(%rsp), %r8
1315		movq 40(%rsp), %r9
1316		addq $48, %rsp
1317		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1318		subq $48, %rsp
1319		movq %rcx, (%rsp)
1320		movq %rdx, 8(%rsp)
1321		movq %rsi, 16(%rsp)
1322		movq %rdi, 24(%rsp)
1323		movq %r8, 32(%rsp)
1324		movq %r9, 40(%rsp)
1325		# Set up R11 as a temporary second base pointer for the caller saved things
1326		movq %rsp, %r11
1327		# Stack pointer unchanged, no stack allocated arguments
1328		movq -24(%rbp), %rax
1329		movq %rax, %rax
1330		movq %rax, %rdi
1331		# statement primitives do not return results (but will clobber r0/rax)
1332		call _prints
1333		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1334		movq (%rsp), %rcx
1335		movq 8(%rsp), %rdx
1336		movq 16(%rsp), %rsi
1337		movq 24(%rsp), %rdi
1338		movq 32(%rsp), %r8
1339		movq 40(%rsp), %r9
1340		addq $48, %rsp
1341		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1342		subq $48, %rsp
1343		movq %rcx, (%rsp)
1344		movq %rdx, 8(%rsp)
1345		movq %rsi, 16(%rsp)
1346		movq %rdi, 24(%rsp)
1347		movq %r8, 32(%rsp)
1348		movq %r9, 40(%rsp)
1349		# Set up R11 as a temporary second base pointer for the caller saved things
1350		movq %rsp, %r11
1351		# Stack pointer unchanged, no stack allocated arguments
1352		movq %r12, %rax
1353		movq %rax, %rdi
1354		# statement primitives do not return results (but will clobber r0/rax)
1355		call _prints
1356		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1357		movq (%rsp), %rcx
1358		movq 8(%rsp), %rdx
1359		movq 16(%rsp), %rsi
1360		movq 24(%rsp), %rdi
1361		movq 32(%rsp), %r8
1362		movq 40(%rsp), %r9
1363		addq $48, %rsp
1364		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1365		subq $48, %rsp
1366		movq %rcx, (%rsp)
1367		movq %rdx, 8(%rsp)
1368		movq %rsi, 16(%rsp)
1369		movq %rdi, 24(%rsp)
1370		movq %r8, 32(%rsp)
1371		movq %r9, 40(%rsp)
1372		# Set up R11 as a temporary second base pointer for the caller saved things
1373		movq %rsp, %r11
1374		# Stack pointer unchanged, no stack allocated arguments
1375		movq -16(%rbp), %rax
1376		movq %rax, %rax
1377		movq %rax, %rdi
1378		# statement primitives do not return results (but will clobber r0/rax)
1379		call _prints
1380		call _println
1381		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1382		movq (%rsp), %rcx
1383		movq 8(%rsp), %rdx
1384		movq 16(%rsp), %rsi
1385		movq 24(%rsp), %rdi
1386		movq 32(%rsp), %r8
1387		movq 40(%rsp), %r9
1388		addq $48, %rsp
1389		addq $32, %rsp
1390		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1391		subq $48, %rsp
1392		movq %rcx, (%rsp)
1393		movq %rdx, 8(%rsp)
1394		movq %rsi, 16(%rsp)
1395		movq %rdi, 24(%rsp)
1396		movq %r8, 32(%rsp)
1397		movq %r9, 40(%rsp)
1398		# Set up R11 as a temporary second base pointer for the caller saved things
1399		movq %rsp, %r11
1400		# Stack pointer unchanged, no stack allocated arguments
1401		movl $0, %r10d
1402		movq -8(%rbp), %rax
1403		pushq %r9
1404		movq %rax, %r9
1405		call _arrLoad1
1406		movb %r9b, %al
1407		popq %r9
1408		movq %rax, %rax
1409		movq %rax, %rdi
1410		# statement primitives do not return results (but will clobber r0/rax)
1411		call _printb
1412		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1413		movq (%rsp), %rcx
1414		movq 8(%rsp), %rdx
1415		movq 16(%rsp), %rsi
1416		movq 24(%rsp), %rdi
1417		movq 32(%rsp), %r8
1418		movq 40(%rsp), %r9
1419		addq $48, %rsp
1420		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1421		subq $48, %rsp
1422		movq %rcx, (%rsp)
1423		movq %rdx, 8(%rsp)
1424		movq %rsi, 16(%rsp)
1425		movq %rdi, 24(%rsp)
1426		movq %r8, 32(%rsp)
1427		movq %r9, 40(%rsp)
1428		# Set up R11 as a temporary second base pointer for the caller saved things
1429		movq %rsp, %r11
1430		# Stack pointer unchanged, no stack allocated arguments
1431		movq %r12, %rax
1432		movq %rax, %rdi
1433		# statement primitives do not return results (but will clobber r0/rax)
1434		call _prints
1435		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1436		movq (%rsp), %rcx
1437		movq 8(%rsp), %rdx
1438		movq 16(%rsp), %rsi
1439		movq 24(%rsp), %rdi
1440		movq 32(%rsp), %r8
1441		movq 40(%rsp), %r9
1442		addq $48, %rsp
1443		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1444		subq $48, %rsp
1445		movq %rcx, (%rsp)
1446		movq %rdx, 8(%rsp)
1447		movq %rsi, 16(%rsp)
1448		movq %rdi, 24(%rsp)
1449		movq %r8, 32(%rsp)
1450		movq %r9, 40(%rsp)
1451		# Set up R11 as a temporary second base pointer for the caller saved things
1452		movq %rsp, %r11
1453		# Stack pointer unchanged, no stack allocated arguments
1454		movl $1, %r10d
1455		movq -8(%rbp), %rax
1456		pushq %r9
1457		movq %rax, %r9
1458		call _arrLoad1
1459		movb %r9b, %al
1460		popq %r9
1461		movq %rax, %rax
1462		movq %rax, %rdi
1463		# statement primitives do not return results (but will clobber r0/rax)
1464		call _printb
1465		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1466		movq (%rsp), %rcx
1467		movq 8(%rsp), %rdx
1468		movq 16(%rsp), %rsi
1469		movq 24(%rsp), %rdi
1470		movq 32(%rsp), %r8
1471		movq 40(%rsp), %r9
1472		addq $48, %rsp
1473		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1474		subq $48, %rsp
1475		movq %rcx, (%rsp)
1476		movq %rdx, 8(%rsp)
1477		movq %rsi, 16(%rsp)
1478		movq %rdi, 24(%rsp)
1479		movq %r8, 32(%rsp)
1480		movq %r9, 40(%rsp)
1481		# Set up R11 as a temporary second base pointer for the caller saved things
1482		movq %rsp, %r11
1483		# Stack pointer unchanged, no stack allocated arguments
1484		movq %r12, %rax
1485		movq %rax, %rdi
1486		# statement primitives do not return results (but will clobber r0/rax)
1487		call _prints
1488		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1489		movq (%rsp), %rcx
1490		movq 8(%rsp), %rdx
1491		movq 16(%rsp), %rsi
1492		movq 24(%rsp), %rdi
1493		movq 32(%rsp), %r8
1494		movq 40(%rsp), %r9
1495		addq $48, %rsp
1496		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1497		subq $48, %rsp
1498		movq %rcx, (%rsp)
1499		movq %rdx, 8(%rsp)
1500		movq %rsi, 16(%rsp)
1501		movq %rdi, 24(%rsp)
1502		movq %r8, 32(%rsp)
1503		movq %r9, 40(%rsp)
1504		# Set up R11 as a temporary second base pointer for the caller saved things
1505		movq %rsp, %r11
1506		# Stack pointer unchanged, no stack allocated arguments
1507		movl $2, %r10d
1508		movq -8(%rbp), %rax
1509		pushq %r9
1510		movq %rax, %r9
1511		call _arrLoad1
1512		movb %r9b, %al
1513		popq %r9
1514		movq %rax, %rax
1515		movq %rax, %rdi
1516		# statement primitives do not return results (but will clobber r0/rax)
1517		call _printb
1518		call _println
1519		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1520		movq (%rsp), %rcx
1521		movq 8(%rsp), %rdx
1522		movq 16(%rsp), %rsi
1523		movq 24(%rsp), %rdi
1524		movq 32(%rsp), %r8
1525		movq 40(%rsp), %r9
1526		addq $48, %rsp
1527		addq $8, %rsp
1528		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1529		subq $48, %rsp
1530		movq %rcx, (%rsp)
1531		movq %rdx, 8(%rsp)
1532		movq %rsi, 16(%rsp)
1533		movq %rdi, 24(%rsp)
1534		movq %r8, 32(%rsp)
1535		movq %r9, 40(%rsp)
1536		# Set up R11 as a temporary second base pointer for the caller saved things
1537		movq %rsp, %r11
1538		# Stack pointer unchanged, no stack allocated arguments
1539		movq %r9, %rax
1540		movq %rax, %rdi
1541		# statement primitives do not return results (but will clobber r0/rax)
1542		call _prints
1543		call _println
1544		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1545		movq (%rsp), %rcx
1546		movq 8(%rsp), %rdx
1547		movq 16(%rsp), %rsi
1548		movq 24(%rsp), %rdi
1549		movq 32(%rsp), %r8
1550		movq 40(%rsp), %r9
1551		addq $48, %rsp
1552		# Stack pointer unchanged, no stack allocated variables
1553		movl $0, %r10d
1554		pushq %r9
1555		movq %rdx, %r9
1556		call _arrLoad4
1557		movl %r9d, %eax
1558		popq %r9
1559		movq %rax, %rax
1560		movq %rax, %rsi
1561		movl $1, %r10d
1562		pushq %r9
1563		movq %rdx, %r9
1564		call _arrLoad4
1565		movl %r9d, %eax
1566		popq %r9
1567		movq %rax, %rax
1568		movq %rax, %rdi
1569		movl $2, %r10d
1570		pushq %r9
1571		movq %rdx, %r9
1572		call _arrLoad4
1573		movl %r9d, %eax
1574		popq %r9
1575		movq %rax, %rax
1576		movq %rax, %r8
1577		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1578		subq $48, %rsp
1579		movq %rcx, (%rsp)
1580		movq %rdx, 8(%rsp)
1581		movq %rsi, 16(%rsp)
1582		movq %rdi, 24(%rsp)
1583		movq %r8, 32(%rsp)
1584		movq %r9, 40(%rsp)
1585		# Set up R11 as a temporary second base pointer for the caller saved things
1586		movq %rsp, %r11
1587		# Stack pointer unchanged, no stack allocated arguments
1588		movq %rsi, %rax
1589		movq %rax, %rdi
1590		# statement primitives do not return results (but will clobber r0/rax)
1591		call _printi
1592		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1593		movq (%rsp), %rcx
1594		movq 8(%rsp), %rdx
1595		movq 16(%rsp), %rsi
1596		movq 24(%rsp), %rdi
1597		movq 32(%rsp), %r8
1598		movq 40(%rsp), %r9
1599		addq $48, %rsp
1600		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1601		subq $48, %rsp
1602		movq %rcx, (%rsp)
1603		movq %rdx, 8(%rsp)
1604		movq %rsi, 16(%rsp)
1605		movq %rdi, 24(%rsp)
1606		movq %r8, 32(%rsp)
1607		movq %r9, 40(%rsp)
1608		# Set up R11 as a temporary second base pointer for the caller saved things
1609		movq %rsp, %r11
1610		# Stack pointer unchanged, no stack allocated arguments
1611		movq %r12, %rax
1612		movq %rax, %rdi
1613		# statement primitives do not return results (but will clobber r0/rax)
1614		call _prints
1615		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1616		movq (%rsp), %rcx
1617		movq 8(%rsp), %rdx
1618		movq 16(%rsp), %rsi
1619		movq 24(%rsp), %rdi
1620		movq 32(%rsp), %r8
1621		movq 40(%rsp), %r9
1622		addq $48, %rsp
1623		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1624		subq $48, %rsp
1625		movq %rcx, (%rsp)
1626		movq %rdx, 8(%rsp)
1627		movq %rsi, 16(%rsp)
1628		movq %rdi, 24(%rsp)
1629		movq %r8, 32(%rsp)
1630		movq %r9, 40(%rsp)
1631		# Set up R11 as a temporary second base pointer for the caller saved things
1632		movq %rsp, %r11
1633		# Stack pointer unchanged, no stack allocated arguments
1634		movq %rdi, %rax
1635		movq %rax, %rdi
1636		# statement primitives do not return results (but will clobber r0/rax)
1637		call _printi
1638		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1639		movq (%rsp), %rcx
1640		movq 8(%rsp), %rdx
1641		movq 16(%rsp), %rsi
1642		movq 24(%rsp), %rdi
1643		movq 32(%rsp), %r8
1644		movq 40(%rsp), %r9
1645		addq $48, %rsp
1646		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1647		subq $48, %rsp
1648		movq %rcx, (%rsp)
1649		movq %rdx, 8(%rsp)
1650		movq %rsi, 16(%rsp)
1651		movq %rdi, 24(%rsp)
1652		movq %r8, 32(%rsp)
1653		movq %r9, 40(%rsp)
1654		# Set up R11 as a temporary second base pointer for the caller saved things
1655		movq %rsp, %r11
1656		# Stack pointer unchanged, no stack allocated arguments
1657		movq %r12, %rax
1658		movq %rax, %rdi
1659		# statement primitives do not return results (but will clobber r0/rax)
1660		call _prints
1661		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1662		movq (%rsp), %rcx
1663		movq 8(%rsp), %rdx
1664		movq 16(%rsp), %rsi
1665		movq 24(%rsp), %rdi
1666		movq 32(%rsp), %r8
1667		movq 40(%rsp), %r9
1668		addq $48, %rsp
1669		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1670		subq $48, %rsp
1671		movq %rcx, (%rsp)
1672		movq %rdx, 8(%rsp)
1673		movq %rsi, 16(%rsp)
1674		movq %rdi, 24(%rsp)
1675		movq %r8, 32(%rsp)
1676		movq %r9, 40(%rsp)
1677		# Set up R11 as a temporary second base pointer for the caller saved things
1678		movq %rsp, %r11
1679		# Stack pointer unchanged, no stack allocated arguments
1680		movq %r8, %rax
1681		movq %rax, %rdi
1682		# statement primitives do not return results (but will clobber r0/rax)
1683		call _printi
1684		call _println
1685		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1686		movq (%rsp), %rcx
1687		movq 8(%rsp), %rdx
1688		movq 16(%rsp), %rsi
1689		movq 24(%rsp), %rdi
1690		movq 32(%rsp), %r8
1691		movq 40(%rsp), %r9
1692		addq $48, %rsp
1693		# Stack pointer unchanged, no stack allocated variables
1694		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1695		subq $48, %rsp
1696		movq %rcx, (%rsp)
1697		movq %rdx, 8(%rsp)
1698		movq %rsi, 16(%rsp)
1699		movq %rdi, 24(%rsp)
1700		movq %r8, 32(%rsp)
1701		movq %r9, 40(%rsp)
1702		# Set up R11 as a temporary second base pointer for the caller saved things
1703		movq %rsp, %r11
1704		# Stack pointer unchanged, no stack allocated arguments
1705		movq %rcx, %rax
1706		movq %rax, %rdi
1707		# statement primitives do not return results (but will clobber r0/rax)
1708		call _prints
1709		call _println
1710		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1711		movq (%rsp), %rcx
1712		movq 8(%rsp), %rdx
1713		movq 16(%rsp), %rsi
1714		movq 24(%rsp), %rdi
1715		movq 32(%rsp), %r8
1716		movq 40(%rsp), %r9
1717		addq $48, %rsp
1718		# Stack pointer unchanged, no stack allocated variables
1719		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1720		subq $48, %rsp
1721		movq %rcx, (%rsp)
1722		movq %rdx, 8(%rsp)
1723		movq %rsi, 16(%rsp)
1724		movq %rdi, 24(%rsp)
1725		movq %r8, 32(%rsp)
1726		movq %r9, 40(%rsp)
1727		# Set up R11 as a temporary second base pointer for the caller saved things
1728		movq %rsp, %r11
1729		# Stack pointer unchanged, no stack allocated arguments
1730		movq %r15, %rax
1731		movq %rax, %rdi
1732		# statement primitives do not return results (but will clobber r0/rax)
1733		call _printb
1734		call _println
1735		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1736		movq (%rsp), %rcx
1737		movq 8(%rsp), %rdx
1738		movq 16(%rsp), %rsi
1739		movq 24(%rsp), %rdi
1740		movq 32(%rsp), %r8
1741		movq 40(%rsp), %r9
1742		addq $48, %rsp
1743		# Stack pointer unchanged, no stack allocated variables
1744		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1745		subq $48, %rsp
1746		movq %rcx, (%rsp)
1747		movq %rdx, 8(%rsp)
1748		movq %rsi, 16(%rsp)
1749		movq %rdi, 24(%rsp)
1750		movq %r8, 32(%rsp)
1751		movq %r9, 40(%rsp)
1752		# Set up R11 as a temporary second base pointer for the caller saved things
1753		movq %rsp, %r11
1754		# Stack pointer unchanged, no stack allocated arguments
1755		movq %r14, %rax
1756		movq %rax, %rdi
1757		# statement primitives do not return results (but will clobber r0/rax)
1758		call _printc
1759		call _println
1760		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1761		movq (%rsp), %rcx
1762		movq 8(%rsp), %rdx
1763		movq 16(%rsp), %rsi
1764		movq 24(%rsp), %rdi
1765		movq 32(%rsp), %r8
1766		movq 40(%rsp), %r9
1767		addq $48, %rsp
1768		# Stack pointer unchanged, no stack allocated variables
1769		# pushq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1770		subq $48, %rsp
1771		movq %rcx, (%rsp)
1772		movq %rdx, 8(%rsp)
1773		movq %rsi, 16(%rsp)
1774		movq %rdi, 24(%rsp)
1775		movq %r8, 32(%rsp)
1776		movq %r9, 40(%rsp)
1777		# Set up R11 as a temporary second base pointer for the caller saved things
1778		movq %rsp, %r11
1779		# Stack pointer unchanged, no stack allocated arguments
1780		movq %r13, %rax
1781		movq %rax, %rdi
1782		# statement primitives do not return results (but will clobber r0/rax)
1783		call _printi
1784		call _println
1785		# popq {%rcx, %rdx, %rsi, %rdi, %r8, %r9}
1786		movq (%rsp), %rcx
1787		movq 8(%rsp), %rdx
1788		movq 16(%rsp), %rsi
1789		movq 24(%rsp), %rdi
1790		movq 32(%rsp), %r8
1791		movq 40(%rsp), %r9
1792		addq $48, %rsp
1793		# Stack pointer unchanged, no stack allocated variables
1794		movq $0, %rax
1795		# popq {%rbx, %r12, %r13, %r14, %r15}
1796		movq (%rsp), %rbx
1797		movq 8(%rsp), %r12
1798		movq 16(%rsp), %r13
1799		movq 24(%rsp), %r14
1800		movq 32(%rsp), %r15
1801		addq $40, %rsp
1802		popq %rbp
1803		ret
1804	
1805	.section .rodata
1806	# length of .L._prints_str0
1807		.int 4
1808	.L._prints_str0:
1809		.asciz "%.*s"
1810	.text
1811	_prints:
1812		pushq %rbp
1813		movq %rsp, %rbp
1814		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1815		andq $-16, %rsp
1816		movq %rdi, %rdx
1817		movl -4(%rdi), %esi
1818		leaq .L._prints_str0(%rip), %rdi
1819		# on x86, al represents the number of SIMD registers used as variadic arguments
1820		movb $0, %al
1821		call printf@plt
1822		movq $0, %rdi
1823		call fflush@plt
1824		movq %rbp, %rsp
1825		popq %rbp
1826		ret
1827	
1828	.section .rodata
1829	# length of .L._printp_str0
1830		.int 2
1831	.L._printp_str0:
1832		.asciz "%p"
1833	.text
1834	_printp:
1835		pushq %rbp
1836		movq %rsp, %rbp
1837		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1838		andq $-16, %rsp
1839		movq %rdi, %rsi
1840		leaq .L._printp_str0(%rip), %rdi
1841		# on x86, al represents the number of SIMD registers used as variadic arguments
1842		movb $0, %al
1843		call printf@plt
1844		movq $0, %rdi
1845		call fflush@plt
1846		movq %rbp, %rsp
1847		popq %rbp
1848		ret
1849	
1850	_malloc:
1851		pushq %rbp
1852		movq %rsp, %rbp
1853		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1854		andq $-16, %rsp
1855		call malloc@plt
1856		cmpq $0, %rax
1857		je _errOutOfMemory
1858		movq %rbp, %rsp
1859		popq %rbp
1860		ret
1861	
1862	.section .rodata
1863	# length of .L._printi_str0
1864		.int 2
1865	.L._printi_str0:
1866		.asciz "%d"
1867	.text
1868	_printi:
1869		pushq %rbp
1870		movq %rsp, %rbp
1871		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1872		andq $-16, %rsp
1873		movl %edi, %esi
1874		leaq .L._printi_str0(%rip), %rdi
1875		# on x86, al represents the number of SIMD registers used as variadic arguments
1876		movb $0, %al
1877		call printf@plt
1878		movq $0, %rdi
1879		call fflush@plt
1880		movq %rbp, %rsp
1881		popq %rbp
1882		ret
1883	
1884	.section .rodata
1885	# length of .L._printb_str0
1886		.int 5
1887	.L._printb_str0:
1888		.asciz "false"
1889	# length of .L._printb_str1
1890		.int 4
1891	.L._printb_str1:
1892		.asciz "true"
1893	# length of .L._printb_str2
1894		.int 4
1895	.L._printb_str2:
1896		.asciz "%.*s"
1897	.text
1898	_printb:
1899		pushq %rbp
1900		movq %rsp, %rbp
1901		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1902		andq $-16, %rsp
1903		cmpb $0, %dil
1904		jne .L_printb0
1905		leaq .L._printb_str0(%rip), %rdx
1906		jmp .L_printb1
1907	.L_printb0:
1908		leaq .L._printb_str1(%rip), %rdx
1909	.L_printb1:
1910		movl -4(%rdx), %esi
1911		leaq .L._printb_str2(%rip), %rdi
1912		# on x86, al represents the number of SIMD registers used as variadic arguments
1913		movb $0, %al
1914		call printf@plt
1915		movq $0, %rdi
1916		call fflush@plt
1917		movq %rbp, %rsp
1918		popq %rbp
1919		ret
1920	
1921	.section .rodata
1922	# length of .L._printc_str0
1923		.int 2
1924	.L._printc_str0:
1925		.asciz "%c"
1926	.text
1927	_printc:
1928		pushq %rbp
1929		movq %rsp, %rbp
1930		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1931		andq $-16, %rsp
1932		movb %dil, %sil
1933		leaq .L._printc_str0(%rip), %rdi
1934		# on x86, al represents the number of SIMD registers used as variadic arguments
1935		movb $0, %al
1936		call printf@plt
1937		movq $0, %rdi
1938		call fflush@plt
1939		movq %rbp, %rsp
1940		popq %rbp
1941		ret
1942	
1943	.section .rodata
1944	# length of .L._println_str0
1945		.int 0
1946	.L._println_str0:
1947		.asciz ""
1948	.text
1949	_println:
1950		pushq %rbp
1951		movq %rsp, %rbp
1952		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1953		andq $-16, %rsp
1954		leaq .L._println_str0(%rip), %rdi
1955		call puts@plt
1956		movq $0, %rdi
1957		call fflush@plt
1958		movq %rbp, %rsp
1959		popq %rbp
1960		ret
1961	
1962	_exit:
1963		pushq %rbp
1964		movq %rsp, %rbp
1965		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1966		andq $-16, %rsp
1967		call exit@plt
1968		movq %rbp, %rsp
1969		popq %rbp
1970		ret
1971	
1972	_arrLoad8:
1973		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
1974		pushq %rbx
1975		cmpl $0, %r10d
1976		cmovl %r10d, %esi
1977		jl _errOutOfBounds
1978		movl -4(%r9), %ebx
1979		cmpl %ebx, %r10d
1980		cmovge %r10d, %esi
1981		jge _errOutOfBounds
1982		movq (%r9,%r10,8), %r9
1983		popq %rbx
1984		ret
1985	
1986	_arrLoad4:
1987		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
1988		pushq %rbx
1989		cmpl $0, %r10d
1990		cmovl %r10d, %esi
1991		jl _errOutOfBounds
1992		movl -4(%r9), %ebx
1993		cmpl %ebx, %r10d
1994		cmovge %r10d, %esi
1995		jge _errOutOfBounds
1996		movslq (%r9,%r10,4), %r9
1997		popq %rbx
1998		ret
1999	
2000	_arrLoad1:
2001		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
2002		pushq %rbx
2003		cmpl $0, %r10d
2004		cmovl %r10d, %esi
2005		jl _errOutOfBounds
2006		movl -4(%r9), %ebx
2007		cmpl %ebx, %r10d
2008		cmovge %r10d, %esi
2009		jge _errOutOfBounds
2010		movsbq (%r9,%r10), %r9
2011		popq %rbx
2012		ret
2013	
2014	.section .rodata
2015	# length of .L._errOutOfMemory_str0
2016		.int 27
2017	.L._errOutOfMemory_str0:
2018		.asciz "fatal error: out of memory\n"
2019	.text
2020	_errOutOfMemory:
2021		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2022		andq $-16, %rsp
2023		leaq .L._errOutOfMemory_str0(%rip), %rdi
2024		call _prints
2025		movb $-1, %dil
2026		call exit@plt
2027	
2028	.section .rodata
2029	# length of .L._errNull_str0
2030		.int 45
2031	.L._errNull_str0:
2032		.asciz "fatal error: null pair dereferenced or freed\n"
2033	.text
2034	_errNull:
2035		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2036		andq $-16, %rsp
2037		leaq .L._errNull_str0(%rip), %rdi
2038		call _prints
2039		movb $-1, %dil
2040		call exit@plt
2041	
2042	.section .rodata
2043	# length of .L._errOutOfBounds_str0
2044		.int 42
2045	.L._errOutOfBounds_str0:
2046		.asciz "fatal error: array index %d out of bounds\n"
2047	.text
2048	_errOutOfBounds:
2049		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2050		andq $-16, %rsp
2051		leaq .L._errOutOfBounds_str0(%rip), %rdi
2052		# on x86, al represents the number of SIMD registers used as variadic arguments
2053		movb $0, %al
2054		call printf@plt
2055		movq $0, %rdi
2056		call fflush@plt
2057		movb $-1, %dil
2058		call exit@plt
===========================================================
-- Finished

