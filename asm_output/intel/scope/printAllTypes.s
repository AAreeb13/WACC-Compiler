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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	# length of .L.str0
4		.int 2
5	.L.str0:
6		.asciz ", "
7	# length of .L.str1
8		.int 16
9	.L.str1:
10		.asciz "this is a string"
11	# length of .L.str2
12		.int 5
13	.L.str2:
14		.asciz "array"
15	# length of .L.str3
16		.int 2
17	.L.str3:
18		.asciz "of"
19	# length of .L.str4
20		.int 7
21	.L.str4:
22		.asciz "strings"
23	# length of .L.str5
24		.int 3
25	.L.str5:
26		.asciz "( ["
27	# length of .L.str6
28		.int 5
29	.L.str6:
30		.asciz "] , ["
31	# length of .L.str7
32		.int 3
33	.L.str7:
34		.asciz "] )"
35	# length of .L.str8
36		.int 2
37	.L.str8:
38		.asciz "[ "
39	# length of .L.str9
40		.int 4
41	.L.str9:
42		.asciz " = ("
43	# length of .L.str10
44		.int 3
45	.L.str10:
46		.asciz "), "
47	# length of .L.str11
48		.int 3
49	.L.str11:
50		.asciz ") ]"
51	.text
52	main:
53		push rbp
54		# push {rbx, r12, r13, r14, r15}
55		sub rsp, 40
56		mov qword ptr [rsp], rbx
57		mov qword ptr [rsp + 8], r12
58		mov qword ptr [rsp + 16], r13
59		mov qword ptr [rsp + 24], r14
60		mov qword ptr [rsp + 32], r15
61		mov rbp, rsp
62		# Stack pointer unchanged, no stack allocated variables
63		lea rax, [rip + .L.str0]
64		push rax
65		pop rax
66		mov rax, rax
67		mov r12, rax
68		mov rax, 5
69		mov r13, rax
70		# Stack pointer unchanged, no stack allocated variables
71		mov rax, 120
72		mov r14, rax
73		# Stack pointer unchanged, no stack allocated variables
74		mov rax, 1
75		mov r15, rax
76		# Stack pointer unchanged, no stack allocated variables
77		lea rax, [rip + .L.str1]
78		push rax
79		pop rax
80		mov rax, rax
81		mov rcx, rax
82		# Stack pointer unchanged, no stack allocated variables
83		# 3 element array
84		# push {rcx, rdx, rsi, rdi, r8, r9}
85		sub rsp, 48
86		mov qword ptr [rsp], rcx
87		mov qword ptr [rsp + 8], rdx
88		mov qword ptr [rsp + 16], rsi
89		mov qword ptr [rsp + 24], rdi
90		mov qword ptr [rsp + 32], r8
91		mov qword ptr [rsp + 40], r9
92		mov edi, 16
93		call _malloc
94		mov r11, rax
95		# pop {rcx, rdx, rsi, rdi, r8, r9}
96		mov rcx, qword ptr [rsp]
97		mov rdx, qword ptr [rsp + 8]
98		mov rsi, qword ptr [rsp + 16]
99		mov rdi, qword ptr [rsp + 24]
100		mov r8, qword ptr [rsp + 32]
101		mov r9, qword ptr [rsp + 40]
102		add rsp, 48
103		# array pointers are shifted forwards by 4 bytes (to account for size)
104		mov r11, r11
105		add r11, 4
106		mov rax, 3
107		mov dword ptr [r11 - 4], eax
108		mov rax, 1
109		mov dword ptr [r11], eax
110		mov rax, 2
111		mov dword ptr [r11 + 4], eax
112		mov rax, 3
113		mov dword ptr [r11 + 8], eax
114		mov rax, r11
115		mov rdx, rax
116		# Stack pointer unchanged, no stack allocated variables
117		# 3 element array
118		# push {rcx, rdx, rsi, rdi, r8, r9}
119		sub rsp, 48
120		mov qword ptr [rsp], rcx
121		mov qword ptr [rsp + 8], rdx
122		mov qword ptr [rsp + 16], rsi
123		mov qword ptr [rsp + 24], rdi
124		mov qword ptr [rsp + 32], r8
125		mov qword ptr [rsp + 40], r9
126		mov edi, 7
127		call _malloc
128		mov r11, rax
129		# pop {rcx, rdx, rsi, rdi, r8, r9}
130		mov rcx, qword ptr [rsp]
131		mov rdx, qword ptr [rsp + 8]
132		mov rsi, qword ptr [rsp + 16]
133		mov rdi, qword ptr [rsp + 24]
134		mov r8, qword ptr [rsp + 32]
135		mov r9, qword ptr [rsp + 40]
136		add rsp, 48
137		# array pointers are shifted forwards by 4 bytes (to account for size)
138		mov r11, r11
139		add r11, 4
140		mov rax, 3
141		mov dword ptr [r11 - 4], eax
142		mov rax, 120
143		mov byte ptr [r11], al
144		mov rax, 121
145		mov byte ptr [r11 + 1], al
146		mov rax, 122
147		mov byte ptr [r11 + 2], al
148		mov rax, r11
149		mov r9, rax
150		sub rsp, 8
151		# 3 element array
152		# push {rcx, rdx, rsi, rdi, r8, r9}
153		sub rsp, 48
154		mov qword ptr [rsp], rcx
155		mov qword ptr [rsp + 8], rdx
156		mov qword ptr [rsp + 16], rsi
157		mov qword ptr [rsp + 24], rdi
158		mov qword ptr [rsp + 32], r8
159		mov qword ptr [rsp + 40], r9
160		mov edi, 7
161		call _malloc
162		mov r11, rax
163		# pop {rcx, rdx, rsi, rdi, r8, r9}
164		mov rcx, qword ptr [rsp]
165		mov rdx, qword ptr [rsp + 8]
166		mov rsi, qword ptr [rsp + 16]
167		mov rdi, qword ptr [rsp + 24]
168		mov r8, qword ptr [rsp + 32]
169		mov r9, qword ptr [rsp + 40]
170		add rsp, 48
171		# array pointers are shifted forwards by 4 bytes (to account for size)
172		mov r11, r11
173		add r11, 4
174		mov rax, 3
175		mov dword ptr [r11 - 4], eax
176		mov rax, 1
177		mov byte ptr [r11], al
178		mov rax, 0
179		mov byte ptr [r11 + 1], al
180		mov rax, 1
181		mov byte ptr [r11 + 2], al
182		mov rax, r11
183		mov qword ptr [rbp - 8], rax
184		sub rsp, 32
185		# 3 element array
186		# push {rcx, rdx, rsi, rdi, r8, r9}
187		sub rsp, 48
188		mov qword ptr [rsp], rcx
189		mov qword ptr [rsp + 8], rdx
190		mov qword ptr [rsp + 16], rsi
191		mov qword ptr [rsp + 24], rdi
192		mov qword ptr [rsp + 32], r8
193		mov qword ptr [rsp + 40], r9
194		mov edi, 28
195		call _malloc
196		mov r11, rax
197		# pop {rcx, rdx, rsi, rdi, r8, r9}
198		mov rcx, qword ptr [rsp]
199		mov rdx, qword ptr [rsp + 8]
200		mov rsi, qword ptr [rsp + 16]
201		mov rdi, qword ptr [rsp + 24]
202		mov r8, qword ptr [rsp + 32]
203		mov r9, qword ptr [rsp + 40]
204		add rsp, 48
205		# array pointers are shifted forwards by 4 bytes (to account for size)
206		mov r11, r11
207		add r11, 4
208		mov rax, 3
209		mov dword ptr [r11 - 4], eax
210		lea rax, [rip + .L.str2]
211		push rax
212		pop rax
213		mov rax, rax
214		mov qword ptr [r11], rax
215		lea rax, [rip + .L.str3]
216		push rax
217		pop rax
218		mov rax, rax
219		mov qword ptr [r11 + 8], rax
220		lea rax, [rip + .L.str4]
221		push rax
222		pop rax
223		mov rax, rax
224		mov qword ptr [r11 + 16], rax
225		mov rax, r11
226		mov qword ptr [rbp - 40], rax
227		sub rsp, 16
228		# push {rcx, rdx, rsi, rdi, r8, r9}
229		sub rsp, 48
230		mov qword ptr [rsp], rcx
231		mov qword ptr [rsp + 8], rdx
232		mov qword ptr [rsp + 16], rsi
233		mov qword ptr [rsp + 24], rdi
234		mov qword ptr [rsp + 32], r8
235		mov qword ptr [rsp + 40], r9
236		mov edi, 16
237		call _malloc
238		mov r11, rax
239		# pop {rcx, rdx, rsi, rdi, r8, r9}
240		mov rcx, qword ptr [rsp]
241		mov rdx, qword ptr [rsp + 8]
242		mov rsi, qword ptr [rsp + 16]
243		mov rdi, qword ptr [rsp + 24]
244		mov r8, qword ptr [rsp + 32]
245		mov r9, qword ptr [rsp + 40]
246		add rsp, 48
247		mov rax, 1
248		mov qword ptr [r11], rax
249		mov rax, 2
250		mov qword ptr [r11 + 8], rax
251		mov rax, r11
252		mov qword ptr [rbp - 56], rax
253		sub rsp, 44
254		# push {rcx, rdx, rsi, rdi, r8, r9}
255		sub rsp, 48
256		mov qword ptr [rsp], rcx
257		mov qword ptr [rsp + 8], rdx
258		mov qword ptr [rsp + 16], rsi
259		mov qword ptr [rsp + 24], rdi
260		mov qword ptr [rsp + 32], r8
261		mov qword ptr [rsp + 40], r9
262		mov edi, 16
263		call _malloc
264		mov r11, rax
265		# pop {rcx, rdx, rsi, rdi, r8, r9}
266		mov rcx, qword ptr [rsp]
267		mov rdx, qword ptr [rsp + 8]
268		mov rsi, qword ptr [rsp + 16]
269		mov rdi, qword ptr [rsp + 24]
270		mov r8, qword ptr [rsp + 32]
271		mov r9, qword ptr [rsp + 40]
272		add rsp, 48
273		mov rax, 97
274		mov qword ptr [r11], rax
275		mov rax, 1
276		mov qword ptr [r11 + 8], rax
277		mov rax, r11
278		mov qword ptr [rbp - 100], rax
279		# push {rcx, rdx, rsi, rdi, r8, r9}
280		sub rsp, 48
281		mov qword ptr [rsp], rcx
282		mov qword ptr [rsp + 8], rdx
283		mov qword ptr [rsp + 16], rsi
284		mov qword ptr [rsp + 24], rdi
285		mov qword ptr [rsp + 32], r8
286		mov qword ptr [rsp + 40], r9
287		mov edi, 16
288		call _malloc
289		mov r11, rax
290		# pop {rcx, rdx, rsi, rdi, r8, r9}
291		mov rcx, qword ptr [rsp]
292		mov rdx, qword ptr [rsp + 8]
293		mov rsi, qword ptr [rsp + 16]
294		mov rdi, qword ptr [rsp + 24]
295		mov r8, qword ptr [rsp + 32]
296		mov r9, qword ptr [rsp + 40]
297		add rsp, 48
298		mov rax, 98
299		mov qword ptr [r11], rax
300		mov rax, 0
301		mov qword ptr [r11 + 8], rax
302		mov rax, r11
303		mov qword ptr [rbp - 92], rax
304		# 2 element array
305		# push {rcx, rdx, rsi, rdi, r8, r9}
306		sub rsp, 48
307		mov qword ptr [rsp], rcx
308		mov qword ptr [rsp + 8], rdx
309		mov qword ptr [rsp + 16], rsi
310		mov qword ptr [rsp + 24], rdi
311		mov qword ptr [rsp + 32], r8
312		mov qword ptr [rsp + 40], r9
313		mov edi, 20
314		call _malloc
315		mov r11, rax
316		# pop {rcx, rdx, rsi, rdi, r8, r9}
317		mov rcx, qword ptr [rsp]
318		mov rdx, qword ptr [rsp + 8]
319		mov rsi, qword ptr [rsp + 16]
320		mov rdi, qword ptr [rsp + 24]
321		mov r8, qword ptr [rsp + 32]
322		mov r9, qword ptr [rsp + 40]
323		add rsp, 48
324		# array pointers are shifted forwards by 4 bytes (to account for size)
325		mov r11, r11
326		add r11, 4
327		mov rax, 2
328		mov dword ptr [r11 - 4], eax
329		mov rax, qword ptr [rbp - 100]
330		mov rax, rax
331		mov qword ptr [r11], rax
332		mov rax, qword ptr [rbp - 92]
333		mov rax, rax
334		mov qword ptr [r11 + 8], rax
335		mov rax, r11
336		mov qword ptr [rbp - 84], rax
337		sub rsp, 40
338		# 3 element array
339		# push {rcx, rdx, rsi, rdi, r8, r9}
340		sub rsp, 48
341		mov qword ptr [rsp], rcx
342		mov qword ptr [rsp + 8], rdx
343		mov qword ptr [rsp + 16], rsi
344		mov qword ptr [rsp + 24], rdi
345		mov qword ptr [rsp + 32], r8
346		mov qword ptr [rsp + 40], r9
347		mov edi, 16
348		call _malloc
349		mov r11, rax
350		# pop {rcx, rdx, rsi, rdi, r8, r9}
351		mov rcx, qword ptr [rsp]
352		mov rdx, qword ptr [rsp + 8]
353		mov rsi, qword ptr [rsp + 16]
354		mov rdi, qword ptr [rsp + 24]
355		mov r8, qword ptr [rsp + 32]
356		mov r9, qword ptr [rsp + 40]
357		add rsp, 48
358		# array pointers are shifted forwards by 4 bytes (to account for size)
359		mov r11, r11
360		add r11, 4
361		mov rax, 3
362		mov dword ptr [r11 - 4], eax
363		mov rax, 1
364		mov dword ptr [r11], eax
365		mov rax, 2
366		mov dword ptr [r11 + 4], eax
367		mov rax, 3
368		mov dword ptr [r11 + 8], eax
369		mov rax, r11
370		mov qword ptr [rbp - 140], rax
371		# 3 element array
372		# push {rcx, rdx, rsi, rdi, r8, r9}
373		sub rsp, 48
374		mov qword ptr [rsp], rcx
375		mov qword ptr [rsp + 8], rdx
376		mov qword ptr [rsp + 16], rsi
377		mov qword ptr [rsp + 24], rdi
378		mov qword ptr [rsp + 32], r8
379		mov qword ptr [rsp + 40], r9
380		mov edi, 7
381		call _malloc
382		mov r11, rax
383		# pop {rcx, rdx, rsi, rdi, r8, r9}
384		mov rcx, qword ptr [rsp]
385		mov rdx, qword ptr [rsp + 8]
386		mov rsi, qword ptr [rsp + 16]
387		mov rdi, qword ptr [rsp + 24]
388		mov r8, qword ptr [rsp + 32]
389		mov r9, qword ptr [rsp + 40]
390		add rsp, 48
391		# array pointers are shifted forwards by 4 bytes (to account for size)
392		mov r11, r11
393		add r11, 4
394		mov rax, 3
395		mov dword ptr [r11 - 4], eax
396		mov rax, 97
397		mov byte ptr [r11], al
398		mov rax, 98
399		mov byte ptr [r11 + 1], al
400		mov rax, 99
401		mov byte ptr [r11 + 2], al
402		mov rax, r11
403		mov qword ptr [rbp - 132], rax
404		# push {rcx, rdx, rsi, rdi, r8, r9}
405		sub rsp, 48
406		mov qword ptr [rsp], rcx
407		mov qword ptr [rsp + 8], rdx
408		mov qword ptr [rsp + 16], rsi
409		mov qword ptr [rsp + 24], rdi
410		mov qword ptr [rsp + 32], r8
411		mov qword ptr [rsp + 40], r9
412		mov edi, 16
413		call _malloc
414		mov r11, rax
415		# pop {rcx, rdx, rsi, rdi, r8, r9}
416		mov rcx, qword ptr [rsp]
417		mov rdx, qword ptr [rsp + 8]
418		mov rsi, qword ptr [rsp + 16]
419		mov rdi, qword ptr [rsp + 24]
420		mov r8, qword ptr [rsp + 32]
421		mov r9, qword ptr [rsp + 40]
422		add rsp, 48
423		mov rax, qword ptr [rbp - 140]
424		mov rax, rax
425		mov qword ptr [r11], rax
426		mov rax, qword ptr [rbp - 132]
427		mov rax, rax
428		mov qword ptr [r11 + 8], rax
429		mov rax, r11
430		mov qword ptr [rbp - 124], rax
431		mov rax, qword ptr [rbp - 124]
432		cmp rax, 0
433		je _errNull
434		mov rax, qword ptr [rax]
435		mov rax, rax
436		mov qword ptr [rbp - 116], rax
437		mov rax, qword ptr [rbp - 124]
438		cmp rax, 0
439		je _errNull
440		mov rax, qword ptr [rax + 8]
441		mov rax, rax
442		mov qword ptr [rbp - 108], rax
443		# push {rcx, rdx, rsi, rdi, r8, r9}
444		sub rsp, 48
445		mov qword ptr [rsp], rcx
446		mov qword ptr [rsp + 8], rdx
447		mov qword ptr [rsp + 16], rsi
448		mov qword ptr [rsp + 24], rdi
449		mov qword ptr [rsp + 32], r8
450		mov qword ptr [rsp + 40], r9
451		# Set up R11 as a temporary second base pointer for the caller saved things
452		mov r11, rsp
453		# Stack pointer unchanged, no stack allocated arguments
454		lea rax, [rip + .L.str5]
455		push rax
456		pop rax
457		mov rax, rax
458		mov rdi, rax
459		# statement primitives do not return results (but will clobber r0/rax)
460		call _prints
461		# pop {rcx, rdx, rsi, rdi, r8, r9}
462		mov rcx, qword ptr [rsp]
463		mov rdx, qword ptr [rsp + 8]
464		mov rsi, qword ptr [rsp + 16]
465		mov rdi, qword ptr [rsp + 24]
466		mov r8, qword ptr [rsp + 32]
467		mov r9, qword ptr [rsp + 40]
468		add rsp, 48
469		# push {rcx, rdx, rsi, rdi, r8, r9}
470		sub rsp, 48
471		mov qword ptr [rsp], rcx
472		mov qword ptr [rsp + 8], rdx
473		mov qword ptr [rsp + 16], rsi
474		mov qword ptr [rsp + 24], rdi
475		mov qword ptr [rsp + 32], r8
476		mov qword ptr [rsp + 40], r9
477		# Set up R11 as a temporary second base pointer for the caller saved things
478		mov r11, rsp
479		# Stack pointer unchanged, no stack allocated arguments
480		mov r10d, 0
481		mov rax, qword ptr [rbp - 116]
482		push r9
483		mov r9, rax
484		call _arrLoad4
485		mov eax, r9d
486		pop r9
487		mov rax, rax
488		mov rdi, rax
489		# statement primitives do not return results (but will clobber r0/rax)
490		call _printi
491		# pop {rcx, rdx, rsi, rdi, r8, r9}
492		mov rcx, qword ptr [rsp]
493		mov rdx, qword ptr [rsp + 8]
494		mov rsi, qword ptr [rsp + 16]
495		mov rdi, qword ptr [rsp + 24]
496		mov r8, qword ptr [rsp + 32]
497		mov r9, qword ptr [rsp + 40]
498		add rsp, 48
499		# push {rcx, rdx, rsi, rdi, r8, r9}
500		sub rsp, 48
501		mov qword ptr [rsp], rcx
502		mov qword ptr [rsp + 8], rdx
503		mov qword ptr [rsp + 16], rsi
504		mov qword ptr [rsp + 24], rdi
505		mov qword ptr [rsp + 32], r8
506		mov qword ptr [rsp + 40], r9
507		# Set up R11 as a temporary second base pointer for the caller saved things
508		mov r11, rsp
509		# Stack pointer unchanged, no stack allocated arguments
510		mov rax, r12
511		mov rdi, rax
512		# statement primitives do not return results (but will clobber r0/rax)
513		call _prints
514		# pop {rcx, rdx, rsi, rdi, r8, r9}
515		mov rcx, qword ptr [rsp]
516		mov rdx, qword ptr [rsp + 8]
517		mov rsi, qword ptr [rsp + 16]
518		mov rdi, qword ptr [rsp + 24]
519		mov r8, qword ptr [rsp + 32]
520		mov r9, qword ptr [rsp + 40]
521		add rsp, 48
522		# push {rcx, rdx, rsi, rdi, r8, r9}
523		sub rsp, 48
524		mov qword ptr [rsp], rcx
525		mov qword ptr [rsp + 8], rdx
526		mov qword ptr [rsp + 16], rsi
527		mov qword ptr [rsp + 24], rdi
528		mov qword ptr [rsp + 32], r8
529		mov qword ptr [rsp + 40], r9
530		# Set up R11 as a temporary second base pointer for the caller saved things
531		mov r11, rsp
532		# Stack pointer unchanged, no stack allocated arguments
533		mov r10d, 1
534		mov rax, qword ptr [rbp - 116]
535		push r9
536		mov r9, rax
537		call _arrLoad4
538		mov eax, r9d
539		pop r9
540		mov rax, rax
541		mov rdi, rax
542		# statement primitives do not return results (but will clobber r0/rax)
543		call _printi
544		# pop {rcx, rdx, rsi, rdi, r8, r9}
545		mov rcx, qword ptr [rsp]
546		mov rdx, qword ptr [rsp + 8]
547		mov rsi, qword ptr [rsp + 16]
548		mov rdi, qword ptr [rsp + 24]
549		mov r8, qword ptr [rsp + 32]
550		mov r9, qword ptr [rsp + 40]
551		add rsp, 48
552		# push {rcx, rdx, rsi, rdi, r8, r9}
553		sub rsp, 48
554		mov qword ptr [rsp], rcx
555		mov qword ptr [rsp + 8], rdx
556		mov qword ptr [rsp + 16], rsi
557		mov qword ptr [rsp + 24], rdi
558		mov qword ptr [rsp + 32], r8
559		mov qword ptr [rsp + 40], r9
560		# Set up R11 as a temporary second base pointer for the caller saved things
561		mov r11, rsp
562		# Stack pointer unchanged, no stack allocated arguments
563		mov rax, r12
564		mov rdi, rax
565		# statement primitives do not return results (but will clobber r0/rax)
566		call _prints
567		# pop {rcx, rdx, rsi, rdi, r8, r9}
568		mov rcx, qword ptr [rsp]
569		mov rdx, qword ptr [rsp + 8]
570		mov rsi, qword ptr [rsp + 16]
571		mov rdi, qword ptr [rsp + 24]
572		mov r8, qword ptr [rsp + 32]
573		mov r9, qword ptr [rsp + 40]
574		add rsp, 48
575		# push {rcx, rdx, rsi, rdi, r8, r9}
576		sub rsp, 48
577		mov qword ptr [rsp], rcx
578		mov qword ptr [rsp + 8], rdx
579		mov qword ptr [rsp + 16], rsi
580		mov qword ptr [rsp + 24], rdi
581		mov qword ptr [rsp + 32], r8
582		mov qword ptr [rsp + 40], r9
583		# Set up R11 as a temporary second base pointer for the caller saved things
584		mov r11, rsp
585		# Stack pointer unchanged, no stack allocated arguments
586		mov r10d, 2
587		mov rax, qword ptr [rbp - 116]
588		push r9
589		mov r9, rax
590		call _arrLoad4
591		mov eax, r9d
592		pop r9
593		mov rax, rax
594		mov rdi, rax
595		# statement primitives do not return results (but will clobber r0/rax)
596		call _printi
597		# pop {rcx, rdx, rsi, rdi, r8, r9}
598		mov rcx, qword ptr [rsp]
599		mov rdx, qword ptr [rsp + 8]
600		mov rsi, qword ptr [rsp + 16]
601		mov rdi, qword ptr [rsp + 24]
602		mov r8, qword ptr [rsp + 32]
603		mov r9, qword ptr [rsp + 40]
604		add rsp, 48
605		# push {rcx, rdx, rsi, rdi, r8, r9}
606		sub rsp, 48
607		mov qword ptr [rsp], rcx
608		mov qword ptr [rsp + 8], rdx
609		mov qword ptr [rsp + 16], rsi
610		mov qword ptr [rsp + 24], rdi
611		mov qword ptr [rsp + 32], r8
612		mov qword ptr [rsp + 40], r9
613		# Set up R11 as a temporary second base pointer for the caller saved things
614		mov r11, rsp
615		# Stack pointer unchanged, no stack allocated arguments
616		lea rax, [rip + .L.str6]
617		push rax
618		pop rax
619		mov rax, rax
620		mov rdi, rax
621		# statement primitives do not return results (but will clobber r0/rax)
622		call _prints
623		# pop {rcx, rdx, rsi, rdi, r8, r9}
624		mov rcx, qword ptr [rsp]
625		mov rdx, qword ptr [rsp + 8]
626		mov rsi, qword ptr [rsp + 16]
627		mov rdi, qword ptr [rsp + 24]
628		mov r8, qword ptr [rsp + 32]
629		mov r9, qword ptr [rsp + 40]
630		add rsp, 48
631		# push {rcx, rdx, rsi, rdi, r8, r9}
632		sub rsp, 48
633		mov qword ptr [rsp], rcx
634		mov qword ptr [rsp + 8], rdx
635		mov qword ptr [rsp + 16], rsi
636		mov qword ptr [rsp + 24], rdi
637		mov qword ptr [rsp + 32], r8
638		mov qword ptr [rsp + 40], r9
639		# Set up R11 as a temporary second base pointer for the caller saved things
640		mov r11, rsp
641		# Stack pointer unchanged, no stack allocated arguments
642		mov r10d, 0
643		mov rax, qword ptr [rbp - 108]
644		push r9
645		mov r9, rax
646		call _arrLoad1
647		mov al, r9b
648		pop r9
649		mov rax, rax
650		mov rdi, rax
651		# statement primitives do not return results (but will clobber r0/rax)
652		call _printc
653		# pop {rcx, rdx, rsi, rdi, r8, r9}
654		mov rcx, qword ptr [rsp]
655		mov rdx, qword ptr [rsp + 8]
656		mov rsi, qword ptr [rsp + 16]
657		mov rdi, qword ptr [rsp + 24]
658		mov r8, qword ptr [rsp + 32]
659		mov r9, qword ptr [rsp + 40]
660		add rsp, 48
661		# push {rcx, rdx, rsi, rdi, r8, r9}
662		sub rsp, 48
663		mov qword ptr [rsp], rcx
664		mov qword ptr [rsp + 8], rdx
665		mov qword ptr [rsp + 16], rsi
666		mov qword ptr [rsp + 24], rdi
667		mov qword ptr [rsp + 32], r8
668		mov qword ptr [rsp + 40], r9
669		# Set up R11 as a temporary second base pointer for the caller saved things
670		mov r11, rsp
671		# Stack pointer unchanged, no stack allocated arguments
672		mov rax, r12
673		mov rdi, rax
674		# statement primitives do not return results (but will clobber r0/rax)
675		call _prints
676		# pop {rcx, rdx, rsi, rdi, r8, r9}
677		mov rcx, qword ptr [rsp]
678		mov rdx, qword ptr [rsp + 8]
679		mov rsi, qword ptr [rsp + 16]
680		mov rdi, qword ptr [rsp + 24]
681		mov r8, qword ptr [rsp + 32]
682		mov r9, qword ptr [rsp + 40]
683		add rsp, 48
684		# push {rcx, rdx, rsi, rdi, r8, r9}
685		sub rsp, 48
686		mov qword ptr [rsp], rcx
687		mov qword ptr [rsp + 8], rdx
688		mov qword ptr [rsp + 16], rsi
689		mov qword ptr [rsp + 24], rdi
690		mov qword ptr [rsp + 32], r8
691		mov qword ptr [rsp + 40], r9
692		# Set up R11 as a temporary second base pointer for the caller saved things
693		mov r11, rsp
694		# Stack pointer unchanged, no stack allocated arguments
695		mov r10d, 1
696		mov rax, qword ptr [rbp - 108]
697		push r9
698		mov r9, rax
699		call _arrLoad1
700		mov al, r9b
701		pop r9
702		mov rax, rax
703		mov rdi, rax
704		# statement primitives do not return results (but will clobber r0/rax)
705		call _printc
706		# pop {rcx, rdx, rsi, rdi, r8, r9}
707		mov rcx, qword ptr [rsp]
708		mov rdx, qword ptr [rsp + 8]
709		mov rsi, qword ptr [rsp + 16]
710		mov rdi, qword ptr [rsp + 24]
711		mov r8, qword ptr [rsp + 32]
712		mov r9, qword ptr [rsp + 40]
713		add rsp, 48
714		# push {rcx, rdx, rsi, rdi, r8, r9}
715		sub rsp, 48
716		mov qword ptr [rsp], rcx
717		mov qword ptr [rsp + 8], rdx
718		mov qword ptr [rsp + 16], rsi
719		mov qword ptr [rsp + 24], rdi
720		mov qword ptr [rsp + 32], r8
721		mov qword ptr [rsp + 40], r9
722		# Set up R11 as a temporary second base pointer for the caller saved things
723		mov r11, rsp
724		# Stack pointer unchanged, no stack allocated arguments
725		mov rax, r12
726		mov rdi, rax
727		# statement primitives do not return results (but will clobber r0/rax)
728		call _prints
729		# pop {rcx, rdx, rsi, rdi, r8, r9}
730		mov rcx, qword ptr [rsp]
731		mov rdx, qword ptr [rsp + 8]
732		mov rsi, qword ptr [rsp + 16]
733		mov rdi, qword ptr [rsp + 24]
734		mov r8, qword ptr [rsp + 32]
735		mov r9, qword ptr [rsp + 40]
736		add rsp, 48
737		# push {rcx, rdx, rsi, rdi, r8, r9}
738		sub rsp, 48
739		mov qword ptr [rsp], rcx
740		mov qword ptr [rsp + 8], rdx
741		mov qword ptr [rsp + 16], rsi
742		mov qword ptr [rsp + 24], rdi
743		mov qword ptr [rsp + 32], r8
744		mov qword ptr [rsp + 40], r9
745		# Set up R11 as a temporary second base pointer for the caller saved things
746		mov r11, rsp
747		# Stack pointer unchanged, no stack allocated arguments
748		mov r10d, 2
749		mov rax, qword ptr [rbp - 108]
750		push r9
751		mov r9, rax
752		call _arrLoad1
753		mov al, r9b
754		pop r9
755		mov rax, rax
756		mov rdi, rax
757		# statement primitives do not return results (but will clobber r0/rax)
758		call _printc
759		# pop {rcx, rdx, rsi, rdi, r8, r9}
760		mov rcx, qword ptr [rsp]
761		mov rdx, qword ptr [rsp + 8]
762		mov rsi, qword ptr [rsp + 16]
763		mov rdi, qword ptr [rsp + 24]
764		mov r8, qword ptr [rsp + 32]
765		mov r9, qword ptr [rsp + 40]
766		add rsp, 48
767		# push {rcx, rdx, rsi, rdi, r8, r9}
768		sub rsp, 48
769		mov qword ptr [rsp], rcx
770		mov qword ptr [rsp + 8], rdx
771		mov qword ptr [rsp + 16], rsi
772		mov qword ptr [rsp + 24], rdi
773		mov qword ptr [rsp + 32], r8
774		mov qword ptr [rsp + 40], r9
775		# Set up R11 as a temporary second base pointer for the caller saved things
776		mov r11, rsp
777		# Stack pointer unchanged, no stack allocated arguments
778		lea rax, [rip + .L.str7]
779		push rax
780		pop rax
781		mov rax, rax
782		mov rdi, rax
783		# statement primitives do not return results (but will clobber r0/rax)
784		call _prints
785		call _println
786		# pop {rcx, rdx, rsi, rdi, r8, r9}
787		mov rcx, qword ptr [rsp]
788		mov rdx, qword ptr [rsp + 8]
789		mov rsi, qword ptr [rsp + 16]
790		mov rdi, qword ptr [rsp + 24]
791		mov r8, qword ptr [rsp + 32]
792		mov r9, qword ptr [rsp + 40]
793		add rsp, 48
794		add rsp, 40
795		mov r10d, 0
796		mov rax, qword ptr [rbp - 84]
797		push r9
798		mov r9, rax
799		call _arrLoad8
800		mov rax, r9
801		pop r9
802		mov rax, rax
803		mov qword ptr [rbp - 76], rax
804		mov rax, qword ptr [rbp - 76]
805		cmp rax, 0
806		je _errNull
807		mov rax, qword ptr [rax]
808		mov rax, rax
809		mov byte ptr [rbp - 68], al
810		mov rax, qword ptr [rbp - 76]
811		cmp rax, 0
812		je _errNull
813		mov rax, qword ptr [rax + 8]
814		mov rax, rax
815		mov byte ptr [rbp - 67], al
816		mov r10d, 1
817		mov rax, qword ptr [rbp - 84]
818		push r9
819		mov r9, rax
820		call _arrLoad8
821		mov rax, r9
822		pop r9
823		mov rax, rax
824		mov qword ptr [rbp - 66], rax
825		mov rax, qword ptr [rbp - 66]
826		cmp rax, 0
827		je _errNull
828		mov rax, qword ptr [rax]
829		mov rax, rax
830		mov byte ptr [rbp - 58], al
831		mov rax, qword ptr [rbp - 66]
832		cmp rax, 0
833		je _errNull
834		mov rax, qword ptr [rax + 8]
835		mov rax, rax
836		mov byte ptr [rbp - 57], al
837		# push {rcx, rdx, rsi, rdi, r8, r9}
838		sub rsp, 48
839		mov qword ptr [rsp], rcx
840		mov qword ptr [rsp + 8], rdx
841		mov qword ptr [rsp + 16], rsi
842		mov qword ptr [rsp + 24], rdi
843		mov qword ptr [rsp + 32], r8
844		mov qword ptr [rsp + 40], r9
845		# Set up R11 as a temporary second base pointer for the caller saved things
846		mov r11, rsp
847		# Stack pointer unchanged, no stack allocated arguments
848		lea rax, [rip + .L.str8]
849		push rax
850		pop rax
851		mov rax, rax
852		mov rdi, rax
853		# statement primitives do not return results (but will clobber r0/rax)
854		call _prints
855		# pop {rcx, rdx, rsi, rdi, r8, r9}
856		mov rcx, qword ptr [rsp]
857		mov rdx, qword ptr [rsp + 8]
858		mov rsi, qword ptr [rsp + 16]
859		mov rdi, qword ptr [rsp + 24]
860		mov r8, qword ptr [rsp + 32]
861		mov r9, qword ptr [rsp + 40]
862		add rsp, 48
863		# push {rcx, rdx, rsi, rdi, r8, r9}
864		sub rsp, 48
865		mov qword ptr [rsp], rcx
866		mov qword ptr [rsp + 8], rdx
867		mov qword ptr [rsp + 16], rsi
868		mov qword ptr [rsp + 24], rdi
869		mov qword ptr [rsp + 32], r8
870		mov qword ptr [rsp + 40], r9
871		# Set up R11 as a temporary second base pointer for the caller saved things
872		mov r11, rsp
873		# Stack pointer unchanged, no stack allocated arguments
874		mov rax, qword ptr [rbp - 76]
875		mov rax, rax
876		mov rdi, rax
877		# statement primitives do not return results (but will clobber r0/rax)
878		call _printp
879		# pop {rcx, rdx, rsi, rdi, r8, r9}
880		mov rcx, qword ptr [rsp]
881		mov rdx, qword ptr [rsp + 8]
882		mov rsi, qword ptr [rsp + 16]
883		mov rdi, qword ptr [rsp + 24]
884		mov r8, qword ptr [rsp + 32]
885		mov r9, qword ptr [rsp + 40]
886		add rsp, 48
887		# push {rcx, rdx, rsi, rdi, r8, r9}
888		sub rsp, 48
889		mov qword ptr [rsp], rcx
890		mov qword ptr [rsp + 8], rdx
891		mov qword ptr [rsp + 16], rsi
892		mov qword ptr [rsp + 24], rdi
893		mov qword ptr [rsp + 32], r8
894		mov qword ptr [rsp + 40], r9
895		# Set up R11 as a temporary second base pointer for the caller saved things
896		mov r11, rsp
897		# Stack pointer unchanged, no stack allocated arguments
898		lea rax, [rip + .L.str9]
899		push rax
900		pop rax
901		mov rax, rax
902		mov rdi, rax
903		# statement primitives do not return results (but will clobber r0/rax)
904		call _prints
905		# pop {rcx, rdx, rsi, rdi, r8, r9}
906		mov rcx, qword ptr [rsp]
907		mov rdx, qword ptr [rsp + 8]
908		mov rsi, qword ptr [rsp + 16]
909		mov rdi, qword ptr [rsp + 24]
910		mov r8, qword ptr [rsp + 32]
911		mov r9, qword ptr [rsp + 40]
912		add rsp, 48
913		# push {rcx, rdx, rsi, rdi, r8, r9}
914		sub rsp, 48
915		mov qword ptr [rsp], rcx
916		mov qword ptr [rsp + 8], rdx
917		mov qword ptr [rsp + 16], rsi
918		mov qword ptr [rsp + 24], rdi
919		mov qword ptr [rsp + 32], r8
920		mov qword ptr [rsp + 40], r9
921		# Set up R11 as a temporary second base pointer for the caller saved things
922		mov r11, rsp
923		# Stack pointer unchanged, no stack allocated arguments
924		movsx rax, byte ptr [rbp - 68]
925		mov rax, rax
926		mov rdi, rax
927		# statement primitives do not return results (but will clobber r0/rax)
928		call _printc
929		# pop {rcx, rdx, rsi, rdi, r8, r9}
930		mov rcx, qword ptr [rsp]
931		mov rdx, qword ptr [rsp + 8]
932		mov rsi, qword ptr [rsp + 16]
933		mov rdi, qword ptr [rsp + 24]
934		mov r8, qword ptr [rsp + 32]
935		mov r9, qword ptr [rsp + 40]
936		add rsp, 48
937		# push {rcx, rdx, rsi, rdi, r8, r9}
938		sub rsp, 48
939		mov qword ptr [rsp], rcx
940		mov qword ptr [rsp + 8], rdx
941		mov qword ptr [rsp + 16], rsi
942		mov qword ptr [rsp + 24], rdi
943		mov qword ptr [rsp + 32], r8
944		mov qword ptr [rsp + 40], r9
945		# Set up R11 as a temporary second base pointer for the caller saved things
946		mov r11, rsp
947		# Stack pointer unchanged, no stack allocated arguments
948		mov rax, r12
949		mov rdi, rax
950		# statement primitives do not return results (but will clobber r0/rax)
951		call _prints
952		# pop {rcx, rdx, rsi, rdi, r8, r9}
953		mov rcx, qword ptr [rsp]
954		mov rdx, qword ptr [rsp + 8]
955		mov rsi, qword ptr [rsp + 16]
956		mov rdi, qword ptr [rsp + 24]
957		mov r8, qword ptr [rsp + 32]
958		mov r9, qword ptr [rsp + 40]
959		add rsp, 48
960		# push {rcx, rdx, rsi, rdi, r8, r9}
961		sub rsp, 48
962		mov qword ptr [rsp], rcx
963		mov qword ptr [rsp + 8], rdx
964		mov qword ptr [rsp + 16], rsi
965		mov qword ptr [rsp + 24], rdi
966		mov qword ptr [rsp + 32], r8
967		mov qword ptr [rsp + 40], r9
968		# Set up R11 as a temporary second base pointer for the caller saved things
969		mov r11, rsp
970		# Stack pointer unchanged, no stack allocated arguments
971		movsx rax, byte ptr [rbp - 67]
972		mov rax, rax
973		mov rdi, rax
974		# statement primitives do not return results (but will clobber r0/rax)
975		call _printb
976		# pop {rcx, rdx, rsi, rdi, r8, r9}
977		mov rcx, qword ptr [rsp]
978		mov rdx, qword ptr [rsp + 8]
979		mov rsi, qword ptr [rsp + 16]
980		mov rdi, qword ptr [rsp + 24]
981		mov r8, qword ptr [rsp + 32]
982		mov r9, qword ptr [rsp + 40]
983		add rsp, 48
984		# push {rcx, rdx, rsi, rdi, r8, r9}
985		sub rsp, 48
986		mov qword ptr [rsp], rcx
987		mov qword ptr [rsp + 8], rdx
988		mov qword ptr [rsp + 16], rsi
989		mov qword ptr [rsp + 24], rdi
990		mov qword ptr [rsp + 32], r8
991		mov qword ptr [rsp + 40], r9
992		# Set up R11 as a temporary second base pointer for the caller saved things
993		mov r11, rsp
994		# Stack pointer unchanged, no stack allocated arguments
995		lea rax, [rip + .L.str10]
996		push rax
997		pop rax
998		mov rax, rax
999		mov rdi, rax
1000		# statement primitives do not return results (but will clobber r0/rax)
1001		call _prints
1002		# pop {rcx, rdx, rsi, rdi, r8, r9}
1003		mov rcx, qword ptr [rsp]
1004		mov rdx, qword ptr [rsp + 8]
1005		mov rsi, qword ptr [rsp + 16]
1006		mov rdi, qword ptr [rsp + 24]
1007		mov r8, qword ptr [rsp + 32]
1008		mov r9, qword ptr [rsp + 40]
1009		add rsp, 48
1010		# push {rcx, rdx, rsi, rdi, r8, r9}
1011		sub rsp, 48
1012		mov qword ptr [rsp], rcx
1013		mov qword ptr [rsp + 8], rdx
1014		mov qword ptr [rsp + 16], rsi
1015		mov qword ptr [rsp + 24], rdi
1016		mov qword ptr [rsp + 32], r8
1017		mov qword ptr [rsp + 40], r9
1018		# Set up R11 as a temporary second base pointer for the caller saved things
1019		mov r11, rsp
1020		# Stack pointer unchanged, no stack allocated arguments
1021		mov rax, qword ptr [rbp - 66]
1022		mov rax, rax
1023		mov rdi, rax
1024		# statement primitives do not return results (but will clobber r0/rax)
1025		call _printp
1026		# pop {rcx, rdx, rsi, rdi, r8, r9}
1027		mov rcx, qword ptr [rsp]
1028		mov rdx, qword ptr [rsp + 8]
1029		mov rsi, qword ptr [rsp + 16]
1030		mov rdi, qword ptr [rsp + 24]
1031		mov r8, qword ptr [rsp + 32]
1032		mov r9, qword ptr [rsp + 40]
1033		add rsp, 48
1034		# push {rcx, rdx, rsi, rdi, r8, r9}
1035		sub rsp, 48
1036		mov qword ptr [rsp], rcx
1037		mov qword ptr [rsp + 8], rdx
1038		mov qword ptr [rsp + 16], rsi
1039		mov qword ptr [rsp + 24], rdi
1040		mov qword ptr [rsp + 32], r8
1041		mov qword ptr [rsp + 40], r9
1042		# Set up R11 as a temporary second base pointer for the caller saved things
1043		mov r11, rsp
1044		# Stack pointer unchanged, no stack allocated arguments
1045		lea rax, [rip + .L.str9]
1046		push rax
1047		pop rax
1048		mov rax, rax
1049		mov rdi, rax
1050		# statement primitives do not return results (but will clobber r0/rax)
1051		call _prints
1052		# pop {rcx, rdx, rsi, rdi, r8, r9}
1053		mov rcx, qword ptr [rsp]
1054		mov rdx, qword ptr [rsp + 8]
1055		mov rsi, qword ptr [rsp + 16]
1056		mov rdi, qword ptr [rsp + 24]
1057		mov r8, qword ptr [rsp + 32]
1058		mov r9, qword ptr [rsp + 40]
1059		add rsp, 48
1060		# push {rcx, rdx, rsi, rdi, r8, r9}
1061		sub rsp, 48
1062		mov qword ptr [rsp], rcx
1063		mov qword ptr [rsp + 8], rdx
1064		mov qword ptr [rsp + 16], rsi
1065		mov qword ptr [rsp + 24], rdi
1066		mov qword ptr [rsp + 32], r8
1067		mov qword ptr [rsp + 40], r9
1068		# Set up R11 as a temporary second base pointer for the caller saved things
1069		mov r11, rsp
1070		# Stack pointer unchanged, no stack allocated arguments
1071		movsx rax, byte ptr [rbp - 58]
1072		mov rax, rax
1073		mov rdi, rax
1074		# statement primitives do not return results (but will clobber r0/rax)
1075		call _printc
1076		# pop {rcx, rdx, rsi, rdi, r8, r9}
1077		mov rcx, qword ptr [rsp]
1078		mov rdx, qword ptr [rsp + 8]
1079		mov rsi, qword ptr [rsp + 16]
1080		mov rdi, qword ptr [rsp + 24]
1081		mov r8, qword ptr [rsp + 32]
1082		mov r9, qword ptr [rsp + 40]
1083		add rsp, 48
1084		# push {rcx, rdx, rsi, rdi, r8, r9}
1085		sub rsp, 48
1086		mov qword ptr [rsp], rcx
1087		mov qword ptr [rsp + 8], rdx
1088		mov qword ptr [rsp + 16], rsi
1089		mov qword ptr [rsp + 24], rdi
1090		mov qword ptr [rsp + 32], r8
1091		mov qword ptr [rsp + 40], r9
1092		# Set up R11 as a temporary second base pointer for the caller saved things
1093		mov r11, rsp
1094		# Stack pointer unchanged, no stack allocated arguments
1095		mov rax, r12
1096		mov rdi, rax
1097		# statement primitives do not return results (but will clobber r0/rax)
1098		call _prints
1099		# pop {rcx, rdx, rsi, rdi, r8, r9}
1100		mov rcx, qword ptr [rsp]
1101		mov rdx, qword ptr [rsp + 8]
1102		mov rsi, qword ptr [rsp + 16]
1103		mov rdi, qword ptr [rsp + 24]
1104		mov r8, qword ptr [rsp + 32]
1105		mov r9, qword ptr [rsp + 40]
1106		add rsp, 48
1107		# push {rcx, rdx, rsi, rdi, r8, r9}
1108		sub rsp, 48
1109		mov qword ptr [rsp], rcx
1110		mov qword ptr [rsp + 8], rdx
1111		mov qword ptr [rsp + 16], rsi
1112		mov qword ptr [rsp + 24], rdi
1113		mov qword ptr [rsp + 32], r8
1114		mov qword ptr [rsp + 40], r9
1115		# Set up R11 as a temporary second base pointer for the caller saved things
1116		mov r11, rsp
1117		# Stack pointer unchanged, no stack allocated arguments
1118		movsx rax, byte ptr [rbp - 57]
1119		mov rax, rax
1120		mov rdi, rax
1121		# statement primitives do not return results (but will clobber r0/rax)
1122		call _printb
1123		# pop {rcx, rdx, rsi, rdi, r8, r9}
1124		mov rcx, qword ptr [rsp]
1125		mov rdx, qword ptr [rsp + 8]
1126		mov rsi, qword ptr [rsp + 16]
1127		mov rdi, qword ptr [rsp + 24]
1128		mov r8, qword ptr [rsp + 32]
1129		mov r9, qword ptr [rsp + 40]
1130		add rsp, 48
1131		# push {rcx, rdx, rsi, rdi, r8, r9}
1132		sub rsp, 48
1133		mov qword ptr [rsp], rcx
1134		mov qword ptr [rsp + 8], rdx
1135		mov qword ptr [rsp + 16], rsi
1136		mov qword ptr [rsp + 24], rdi
1137		mov qword ptr [rsp + 32], r8
1138		mov qword ptr [rsp + 40], r9
1139		# Set up R11 as a temporary second base pointer for the caller saved things
1140		mov r11, rsp
1141		# Stack pointer unchanged, no stack allocated arguments
1142		lea rax, [rip + .L.str11]
1143		push rax
1144		pop rax
1145		mov rax, rax
1146		mov rdi, rax
1147		# statement primitives do not return results (but will clobber r0/rax)
1148		call _prints
1149		call _println
1150		# pop {rcx, rdx, rsi, rdi, r8, r9}
1151		mov rcx, qword ptr [rsp]
1152		mov rdx, qword ptr [rsp + 8]
1153		mov rsi, qword ptr [rsp + 16]
1154		mov rdi, qword ptr [rsp + 24]
1155		mov r8, qword ptr [rsp + 32]
1156		mov r9, qword ptr [rsp + 40]
1157		add rsp, 48
1158		add rsp, 44
1159		mov rax, qword ptr [rbp - 56]
1160		cmp rax, 0
1161		je _errNull
1162		mov rax, qword ptr [rax]
1163		mov rax, rax
1164		mov dword ptr [rbp - 48], eax
1165		mov rax, qword ptr [rbp - 56]
1166		cmp rax, 0
1167		je _errNull
1168		mov rax, qword ptr [rax + 8]
1169		mov rax, rax
1170		mov dword ptr [rbp - 44], eax
1171		# push {rcx, rdx, rsi, rdi, r8, r9}
1172		sub rsp, 48
1173		mov qword ptr [rsp], rcx
1174		mov qword ptr [rsp + 8], rdx
1175		mov qword ptr [rsp + 16], rsi
1176		mov qword ptr [rsp + 24], rdi
1177		mov qword ptr [rsp + 32], r8
1178		mov qword ptr [rsp + 40], r9
1179		# Set up R11 as a temporary second base pointer for the caller saved things
1180		mov r11, rsp
1181		# Stack pointer unchanged, no stack allocated arguments
1182		movsx rax, dword ptr [rbp - 48]
1183		mov rax, rax
1184		mov rdi, rax
1185		# statement primitives do not return results (but will clobber r0/rax)
1186		call _printi
1187		# pop {rcx, rdx, rsi, rdi, r8, r9}
1188		mov rcx, qword ptr [rsp]
1189		mov rdx, qword ptr [rsp + 8]
1190		mov rsi, qword ptr [rsp + 16]
1191		mov rdi, qword ptr [rsp + 24]
1192		mov r8, qword ptr [rsp + 32]
1193		mov r9, qword ptr [rsp + 40]
1194		add rsp, 48
1195		# push {rcx, rdx, rsi, rdi, r8, r9}
1196		sub rsp, 48
1197		mov qword ptr [rsp], rcx
1198		mov qword ptr [rsp + 8], rdx
1199		mov qword ptr [rsp + 16], rsi
1200		mov qword ptr [rsp + 24], rdi
1201		mov qword ptr [rsp + 32], r8
1202		mov qword ptr [rsp + 40], r9
1203		# Set up R11 as a temporary second base pointer for the caller saved things
1204		mov r11, rsp
1205		# Stack pointer unchanged, no stack allocated arguments
1206		mov rax, r12
1207		mov rdi, rax
1208		# statement primitives do not return results (but will clobber r0/rax)
1209		call _prints
1210		# pop {rcx, rdx, rsi, rdi, r8, r9}
1211		mov rcx, qword ptr [rsp]
1212		mov rdx, qword ptr [rsp + 8]
1213		mov rsi, qword ptr [rsp + 16]
1214		mov rdi, qword ptr [rsp + 24]
1215		mov r8, qword ptr [rsp + 32]
1216		mov r9, qword ptr [rsp + 40]
1217		add rsp, 48
1218		# push {rcx, rdx, rsi, rdi, r8, r9}
1219		sub rsp, 48
1220		mov qword ptr [rsp], rcx
1221		mov qword ptr [rsp + 8], rdx
1222		mov qword ptr [rsp + 16], rsi
1223		mov qword ptr [rsp + 24], rdi
1224		mov qword ptr [rsp + 32], r8
1225		mov qword ptr [rsp + 40], r9
1226		# Set up R11 as a temporary second base pointer for the caller saved things
1227		mov r11, rsp
1228		# Stack pointer unchanged, no stack allocated arguments
1229		movsx rax, dword ptr [rbp - 44]
1230		mov rax, rax
1231		mov rdi, rax
1232		# statement primitives do not return results (but will clobber r0/rax)
1233		call _printi
1234		call _println
1235		# pop {rcx, rdx, rsi, rdi, r8, r9}
1236		mov rcx, qword ptr [rsp]
1237		mov rdx, qword ptr [rsp + 8]
1238		mov rsi, qword ptr [rsp + 16]
1239		mov rdi, qword ptr [rsp + 24]
1240		mov r8, qword ptr [rsp + 32]
1241		mov r9, qword ptr [rsp + 40]
1242		add rsp, 48
1243		add rsp, 16
1244		mov r10d, 0
1245		mov rax, qword ptr [rbp - 40]
1246		push r9
1247		mov r9, rax
1248		call _arrLoad8
1249		mov rax, r9
1250		pop r9
1251		mov rax, rax
1252		mov qword ptr [rbp - 32], rax
1253		mov r10d, 1
1254		mov rax, qword ptr [rbp - 40]
1255		push r9
1256		mov r9, rax
1257		call _arrLoad8
1258		mov rax, r9
1259		pop r9
1260		mov rax, rax
1261		mov qword ptr [rbp - 24], rax
1262		mov r10d, 2
1263		mov rax, qword ptr [rbp - 40]
1264		push r9
1265		mov r9, rax
1266		call _arrLoad8
1267		mov rax, r9
1268		pop r9
1269		mov rax, rax
1270		mov qword ptr [rbp - 16], rax
1271		# push {rcx, rdx, rsi, rdi, r8, r9}
1272		sub rsp, 48
1273		mov qword ptr [rsp], rcx
1274		mov qword ptr [rsp + 8], rdx
1275		mov qword ptr [rsp + 16], rsi
1276		mov qword ptr [rsp + 24], rdi
1277		mov qword ptr [rsp + 32], r8
1278		mov qword ptr [rsp + 40], r9
1279		# Set up R11 as a temporary second base pointer for the caller saved things
1280		mov r11, rsp
1281		# Stack pointer unchanged, no stack allocated arguments
1282		mov rax, qword ptr [rbp - 32]
1283		mov rax, rax
1284		mov rdi, rax
1285		# statement primitives do not return results (but will clobber r0/rax)
1286		call _prints
1287		# pop {rcx, rdx, rsi, rdi, r8, r9}
1288		mov rcx, qword ptr [rsp]
1289		mov rdx, qword ptr [rsp + 8]
1290		mov rsi, qword ptr [rsp + 16]
1291		mov rdi, qword ptr [rsp + 24]
1292		mov r8, qword ptr [rsp + 32]
1293		mov r9, qword ptr [rsp + 40]
1294		add rsp, 48
1295		# push {rcx, rdx, rsi, rdi, r8, r9}
1296		sub rsp, 48
1297		mov qword ptr [rsp], rcx
1298		mov qword ptr [rsp + 8], rdx
1299		mov qword ptr [rsp + 16], rsi
1300		mov qword ptr [rsp + 24], rdi
1301		mov qword ptr [rsp + 32], r8
1302		mov qword ptr [rsp + 40], r9
1303		# Set up R11 as a temporary second base pointer for the caller saved things
1304		mov r11, rsp
1305		# Stack pointer unchanged, no stack allocated arguments
1306		mov rax, r12
1307		mov rdi, rax
1308		# statement primitives do not return results (but will clobber r0/rax)
1309		call _prints
1310		# pop {rcx, rdx, rsi, rdi, r8, r9}
1311		mov rcx, qword ptr [rsp]
1312		mov rdx, qword ptr [rsp + 8]
1313		mov rsi, qword ptr [rsp + 16]
1314		mov rdi, qword ptr [rsp + 24]
1315		mov r8, qword ptr [rsp + 32]
1316		mov r9, qword ptr [rsp + 40]
1317		add rsp, 48
1318		# push {rcx, rdx, rsi, rdi, r8, r9}
1319		sub rsp, 48
1320		mov qword ptr [rsp], rcx
1321		mov qword ptr [rsp + 8], rdx
1322		mov qword ptr [rsp + 16], rsi
1323		mov qword ptr [rsp + 24], rdi
1324		mov qword ptr [rsp + 32], r8
1325		mov qword ptr [rsp + 40], r9
1326		# Set up R11 as a temporary second base pointer for the caller saved things
1327		mov r11, rsp
1328		# Stack pointer unchanged, no stack allocated arguments
1329		mov rax, qword ptr [rbp - 24]
1330		mov rax, rax
1331		mov rdi, rax
1332		# statement primitives do not return results (but will clobber r0/rax)
1333		call _prints
1334		# pop {rcx, rdx, rsi, rdi, r8, r9}
1335		mov rcx, qword ptr [rsp]
1336		mov rdx, qword ptr [rsp + 8]
1337		mov rsi, qword ptr [rsp + 16]
1338		mov rdi, qword ptr [rsp + 24]
1339		mov r8, qword ptr [rsp + 32]
1340		mov r9, qword ptr [rsp + 40]
1341		add rsp, 48
1342		# push {rcx, rdx, rsi, rdi, r8, r9}
1343		sub rsp, 48
1344		mov qword ptr [rsp], rcx
1345		mov qword ptr [rsp + 8], rdx
1346		mov qword ptr [rsp + 16], rsi
1347		mov qword ptr [rsp + 24], rdi
1348		mov qword ptr [rsp + 32], r8
1349		mov qword ptr [rsp + 40], r9
1350		# Set up R11 as a temporary second base pointer for the caller saved things
1351		mov r11, rsp
1352		# Stack pointer unchanged, no stack allocated arguments
1353		mov rax, r12
1354		mov rdi, rax
1355		# statement primitives do not return results (but will clobber r0/rax)
1356		call _prints
1357		# pop {rcx, rdx, rsi, rdi, r8, r9}
1358		mov rcx, qword ptr [rsp]
1359		mov rdx, qword ptr [rsp + 8]
1360		mov rsi, qword ptr [rsp + 16]
1361		mov rdi, qword ptr [rsp + 24]
1362		mov r8, qword ptr [rsp + 32]
1363		mov r9, qword ptr [rsp + 40]
1364		add rsp, 48
1365		# push {rcx, rdx, rsi, rdi, r8, r9}
1366		sub rsp, 48
1367		mov qword ptr [rsp], rcx
1368		mov qword ptr [rsp + 8], rdx
1369		mov qword ptr [rsp + 16], rsi
1370		mov qword ptr [rsp + 24], rdi
1371		mov qword ptr [rsp + 32], r8
1372		mov qword ptr [rsp + 40], r9
1373		# Set up R11 as a temporary second base pointer for the caller saved things
1374		mov r11, rsp
1375		# Stack pointer unchanged, no stack allocated arguments
1376		mov rax, qword ptr [rbp - 16]
1377		mov rax, rax
1378		mov rdi, rax
1379		# statement primitives do not return results (but will clobber r0/rax)
1380		call _prints
1381		call _println
1382		# pop {rcx, rdx, rsi, rdi, r8, r9}
1383		mov rcx, qword ptr [rsp]
1384		mov rdx, qword ptr [rsp + 8]
1385		mov rsi, qword ptr [rsp + 16]
1386		mov rdi, qword ptr [rsp + 24]
1387		mov r8, qword ptr [rsp + 32]
1388		mov r9, qword ptr [rsp + 40]
1389		add rsp, 48
1390		add rsp, 32
1391		# push {rcx, rdx, rsi, rdi, r8, r9}
1392		sub rsp, 48
1393		mov qword ptr [rsp], rcx
1394		mov qword ptr [rsp + 8], rdx
1395		mov qword ptr [rsp + 16], rsi
1396		mov qword ptr [rsp + 24], rdi
1397		mov qword ptr [rsp + 32], r8
1398		mov qword ptr [rsp + 40], r9
1399		# Set up R11 as a temporary second base pointer for the caller saved things
1400		mov r11, rsp
1401		# Stack pointer unchanged, no stack allocated arguments
1402		mov r10d, 0
1403		mov rax, qword ptr [rbp - 8]
1404		push r9
1405		mov r9, rax
1406		call _arrLoad1
1407		mov al, r9b
1408		pop r9
1409		mov rax, rax
1410		mov rdi, rax
1411		# statement primitives do not return results (but will clobber r0/rax)
1412		call _printb
1413		# pop {rcx, rdx, rsi, rdi, r8, r9}
1414		mov rcx, qword ptr [rsp]
1415		mov rdx, qword ptr [rsp + 8]
1416		mov rsi, qword ptr [rsp + 16]
1417		mov rdi, qword ptr [rsp + 24]
1418		mov r8, qword ptr [rsp + 32]
1419		mov r9, qword ptr [rsp + 40]
1420		add rsp, 48
1421		# push {rcx, rdx, rsi, rdi, r8, r9}
1422		sub rsp, 48
1423		mov qword ptr [rsp], rcx
1424		mov qword ptr [rsp + 8], rdx
1425		mov qword ptr [rsp + 16], rsi
1426		mov qword ptr [rsp + 24], rdi
1427		mov qword ptr [rsp + 32], r8
1428		mov qword ptr [rsp + 40], r9
1429		# Set up R11 as a temporary second base pointer for the caller saved things
1430		mov r11, rsp
1431		# Stack pointer unchanged, no stack allocated arguments
1432		mov rax, r12
1433		mov rdi, rax
1434		# statement primitives do not return results (but will clobber r0/rax)
1435		call _prints
1436		# pop {rcx, rdx, rsi, rdi, r8, r9}
1437		mov rcx, qword ptr [rsp]
1438		mov rdx, qword ptr [rsp + 8]
1439		mov rsi, qword ptr [rsp + 16]
1440		mov rdi, qword ptr [rsp + 24]
1441		mov r8, qword ptr [rsp + 32]
1442		mov r9, qword ptr [rsp + 40]
1443		add rsp, 48
1444		# push {rcx, rdx, rsi, rdi, r8, r9}
1445		sub rsp, 48
1446		mov qword ptr [rsp], rcx
1447		mov qword ptr [rsp + 8], rdx
1448		mov qword ptr [rsp + 16], rsi
1449		mov qword ptr [rsp + 24], rdi
1450		mov qword ptr [rsp + 32], r8
1451		mov qword ptr [rsp + 40], r9
1452		# Set up R11 as a temporary second base pointer for the caller saved things
1453		mov r11, rsp
1454		# Stack pointer unchanged, no stack allocated arguments
1455		mov r10d, 1
1456		mov rax, qword ptr [rbp - 8]
1457		push r9
1458		mov r9, rax
1459		call _arrLoad1
1460		mov al, r9b
1461		pop r9
1462		mov rax, rax
1463		mov rdi, rax
1464		# statement primitives do not return results (but will clobber r0/rax)
1465		call _printb
1466		# pop {rcx, rdx, rsi, rdi, r8, r9}
1467		mov rcx, qword ptr [rsp]
1468		mov rdx, qword ptr [rsp + 8]
1469		mov rsi, qword ptr [rsp + 16]
1470		mov rdi, qword ptr [rsp + 24]
1471		mov r8, qword ptr [rsp + 32]
1472		mov r9, qword ptr [rsp + 40]
1473		add rsp, 48
1474		# push {rcx, rdx, rsi, rdi, r8, r9}
1475		sub rsp, 48
1476		mov qword ptr [rsp], rcx
1477		mov qword ptr [rsp + 8], rdx
1478		mov qword ptr [rsp + 16], rsi
1479		mov qword ptr [rsp + 24], rdi
1480		mov qword ptr [rsp + 32], r8
1481		mov qword ptr [rsp + 40], r9
1482		# Set up R11 as a temporary second base pointer for the caller saved things
1483		mov r11, rsp
1484		# Stack pointer unchanged, no stack allocated arguments
1485		mov rax, r12
1486		mov rdi, rax
1487		# statement primitives do not return results (but will clobber r0/rax)
1488		call _prints
1489		# pop {rcx, rdx, rsi, rdi, r8, r9}
1490		mov rcx, qword ptr [rsp]
1491		mov rdx, qword ptr [rsp + 8]
1492		mov rsi, qword ptr [rsp + 16]
1493		mov rdi, qword ptr [rsp + 24]
1494		mov r8, qword ptr [rsp + 32]
1495		mov r9, qword ptr [rsp + 40]
1496		add rsp, 48
1497		# push {rcx, rdx, rsi, rdi, r8, r9}
1498		sub rsp, 48
1499		mov qword ptr [rsp], rcx
1500		mov qword ptr [rsp + 8], rdx
1501		mov qword ptr [rsp + 16], rsi
1502		mov qword ptr [rsp + 24], rdi
1503		mov qword ptr [rsp + 32], r8
1504		mov qword ptr [rsp + 40], r9
1505		# Set up R11 as a temporary second base pointer for the caller saved things
1506		mov r11, rsp
1507		# Stack pointer unchanged, no stack allocated arguments
1508		mov r10d, 2
1509		mov rax, qword ptr [rbp - 8]
1510		push r9
1511		mov r9, rax
1512		call _arrLoad1
1513		mov al, r9b
1514		pop r9
1515		mov rax, rax
1516		mov rdi, rax
1517		# statement primitives do not return results (but will clobber r0/rax)
1518		call _printb
1519		call _println
1520		# pop {rcx, rdx, rsi, rdi, r8, r9}
1521		mov rcx, qword ptr [rsp]
1522		mov rdx, qword ptr [rsp + 8]
1523		mov rsi, qword ptr [rsp + 16]
1524		mov rdi, qword ptr [rsp + 24]
1525		mov r8, qword ptr [rsp + 32]
1526		mov r9, qword ptr [rsp + 40]
1527		add rsp, 48
1528		add rsp, 8
1529		# push {rcx, rdx, rsi, rdi, r8, r9}
1530		sub rsp, 48
1531		mov qword ptr [rsp], rcx
1532		mov qword ptr [rsp + 8], rdx
1533		mov qword ptr [rsp + 16], rsi
1534		mov qword ptr [rsp + 24], rdi
1535		mov qword ptr [rsp + 32], r8
1536		mov qword ptr [rsp + 40], r9
1537		# Set up R11 as a temporary second base pointer for the caller saved things
1538		mov r11, rsp
1539		# Stack pointer unchanged, no stack allocated arguments
1540		mov rax, r9
1541		mov rdi, rax
1542		# statement primitives do not return results (but will clobber r0/rax)
1543		call _prints
1544		call _println
1545		# pop {rcx, rdx, rsi, rdi, r8, r9}
1546		mov rcx, qword ptr [rsp]
1547		mov rdx, qword ptr [rsp + 8]
1548		mov rsi, qword ptr [rsp + 16]
1549		mov rdi, qword ptr [rsp + 24]
1550		mov r8, qword ptr [rsp + 32]
1551		mov r9, qword ptr [rsp + 40]
1552		add rsp, 48
1553		# Stack pointer unchanged, no stack allocated variables
1554		mov r10d, 0
1555		push r9
1556		mov r9, rdx
1557		call _arrLoad4
1558		mov eax, r9d
1559		pop r9
1560		mov rax, rax
1561		mov rsi, rax
1562		mov r10d, 1
1563		push r9
1564		mov r9, rdx
1565		call _arrLoad4
1566		mov eax, r9d
1567		pop r9
1568		mov rax, rax
1569		mov rdi, rax
1570		mov r10d, 2
1571		push r9
1572		mov r9, rdx
1573		call _arrLoad4
1574		mov eax, r9d
1575		pop r9
1576		mov rax, rax
1577		mov r8, rax
1578		# push {rcx, rdx, rsi, rdi, r8, r9}
1579		sub rsp, 48
1580		mov qword ptr [rsp], rcx
1581		mov qword ptr [rsp + 8], rdx
1582		mov qword ptr [rsp + 16], rsi
1583		mov qword ptr [rsp + 24], rdi
1584		mov qword ptr [rsp + 32], r8
1585		mov qword ptr [rsp + 40], r9
1586		# Set up R11 as a temporary second base pointer for the caller saved things
1587		mov r11, rsp
1588		# Stack pointer unchanged, no stack allocated arguments
1589		mov rax, rsi
1590		mov rdi, rax
1591		# statement primitives do not return results (but will clobber r0/rax)
1592		call _printi
1593		# pop {rcx, rdx, rsi, rdi, r8, r9}
1594		mov rcx, qword ptr [rsp]
1595		mov rdx, qword ptr [rsp + 8]
1596		mov rsi, qword ptr [rsp + 16]
1597		mov rdi, qword ptr [rsp + 24]
1598		mov r8, qword ptr [rsp + 32]
1599		mov r9, qword ptr [rsp + 40]
1600		add rsp, 48
1601		# push {rcx, rdx, rsi, rdi, r8, r9}
1602		sub rsp, 48
1603		mov qword ptr [rsp], rcx
1604		mov qword ptr [rsp + 8], rdx
1605		mov qword ptr [rsp + 16], rsi
1606		mov qword ptr [rsp + 24], rdi
1607		mov qword ptr [rsp + 32], r8
1608		mov qword ptr [rsp + 40], r9
1609		# Set up R11 as a temporary second base pointer for the caller saved things
1610		mov r11, rsp
1611		# Stack pointer unchanged, no stack allocated arguments
1612		mov rax, r12
1613		mov rdi, rax
1614		# statement primitives do not return results (but will clobber r0/rax)
1615		call _prints
1616		# pop {rcx, rdx, rsi, rdi, r8, r9}
1617		mov rcx, qword ptr [rsp]
1618		mov rdx, qword ptr [rsp + 8]
1619		mov rsi, qword ptr [rsp + 16]
1620		mov rdi, qword ptr [rsp + 24]
1621		mov r8, qword ptr [rsp + 32]
1622		mov r9, qword ptr [rsp + 40]
1623		add rsp, 48
1624		# push {rcx, rdx, rsi, rdi, r8, r9}
1625		sub rsp, 48
1626		mov qword ptr [rsp], rcx
1627		mov qword ptr [rsp + 8], rdx
1628		mov qword ptr [rsp + 16], rsi
1629		mov qword ptr [rsp + 24], rdi
1630		mov qword ptr [rsp + 32], r8
1631		mov qword ptr [rsp + 40], r9
1632		# Set up R11 as a temporary second base pointer for the caller saved things
1633		mov r11, rsp
1634		# Stack pointer unchanged, no stack allocated arguments
1635		mov rax, rdi
1636		mov rdi, rax
1637		# statement primitives do not return results (but will clobber r0/rax)
1638		call _printi
1639		# pop {rcx, rdx, rsi, rdi, r8, r9}
1640		mov rcx, qword ptr [rsp]
1641		mov rdx, qword ptr [rsp + 8]
1642		mov rsi, qword ptr [rsp + 16]
1643		mov rdi, qword ptr [rsp + 24]
1644		mov r8, qword ptr [rsp + 32]
1645		mov r9, qword ptr [rsp + 40]
1646		add rsp, 48
1647		# push {rcx, rdx, rsi, rdi, r8, r9}
1648		sub rsp, 48
1649		mov qword ptr [rsp], rcx
1650		mov qword ptr [rsp + 8], rdx
1651		mov qword ptr [rsp + 16], rsi
1652		mov qword ptr [rsp + 24], rdi
1653		mov qword ptr [rsp + 32], r8
1654		mov qword ptr [rsp + 40], r9
1655		# Set up R11 as a temporary second base pointer for the caller saved things
1656		mov r11, rsp
1657		# Stack pointer unchanged, no stack allocated arguments
1658		mov rax, r12
1659		mov rdi, rax
1660		# statement primitives do not return results (but will clobber r0/rax)
1661		call _prints
1662		# pop {rcx, rdx, rsi, rdi, r8, r9}
1663		mov rcx, qword ptr [rsp]
1664		mov rdx, qword ptr [rsp + 8]
1665		mov rsi, qword ptr [rsp + 16]
1666		mov rdi, qword ptr [rsp + 24]
1667		mov r8, qword ptr [rsp + 32]
1668		mov r9, qword ptr [rsp + 40]
1669		add rsp, 48
1670		# push {rcx, rdx, rsi, rdi, r8, r9}
1671		sub rsp, 48
1672		mov qword ptr [rsp], rcx
1673		mov qword ptr [rsp + 8], rdx
1674		mov qword ptr [rsp + 16], rsi
1675		mov qword ptr [rsp + 24], rdi
1676		mov qword ptr [rsp + 32], r8
1677		mov qword ptr [rsp + 40], r9
1678		# Set up R11 as a temporary second base pointer for the caller saved things
1679		mov r11, rsp
1680		# Stack pointer unchanged, no stack allocated arguments
1681		mov rax, r8
1682		mov rdi, rax
1683		# statement primitives do not return results (but will clobber r0/rax)
1684		call _printi
1685		call _println
1686		# pop {rcx, rdx, rsi, rdi, r8, r9}
1687		mov rcx, qword ptr [rsp]
1688		mov rdx, qword ptr [rsp + 8]
1689		mov rsi, qword ptr [rsp + 16]
1690		mov rdi, qword ptr [rsp + 24]
1691		mov r8, qword ptr [rsp + 32]
1692		mov r9, qword ptr [rsp + 40]
1693		add rsp, 48
1694		# Stack pointer unchanged, no stack allocated variables
1695		# push {rcx, rdx, rsi, rdi, r8, r9}
1696		sub rsp, 48
1697		mov qword ptr [rsp], rcx
1698		mov qword ptr [rsp + 8], rdx
1699		mov qword ptr [rsp + 16], rsi
1700		mov qword ptr [rsp + 24], rdi
1701		mov qword ptr [rsp + 32], r8
1702		mov qword ptr [rsp + 40], r9
1703		# Set up R11 as a temporary second base pointer for the caller saved things
1704		mov r11, rsp
1705		# Stack pointer unchanged, no stack allocated arguments
1706		mov rax, rcx
1707		mov rdi, rax
1708		# statement primitives do not return results (but will clobber r0/rax)
1709		call _prints
1710		call _println
1711		# pop {rcx, rdx, rsi, rdi, r8, r9}
1712		mov rcx, qword ptr [rsp]
1713		mov rdx, qword ptr [rsp + 8]
1714		mov rsi, qword ptr [rsp + 16]
1715		mov rdi, qword ptr [rsp + 24]
1716		mov r8, qword ptr [rsp + 32]
1717		mov r9, qword ptr [rsp + 40]
1718		add rsp, 48
1719		# Stack pointer unchanged, no stack allocated variables
1720		# push {rcx, rdx, rsi, rdi, r8, r9}
1721		sub rsp, 48
1722		mov qword ptr [rsp], rcx
1723		mov qword ptr [rsp + 8], rdx
1724		mov qword ptr [rsp + 16], rsi
1725		mov qword ptr [rsp + 24], rdi
1726		mov qword ptr [rsp + 32], r8
1727		mov qword ptr [rsp + 40], r9
1728		# Set up R11 as a temporary second base pointer for the caller saved things
1729		mov r11, rsp
1730		# Stack pointer unchanged, no stack allocated arguments
1731		mov rax, r15
1732		mov rdi, rax
1733		# statement primitives do not return results (but will clobber r0/rax)
1734		call _printb
1735		call _println
1736		# pop {rcx, rdx, rsi, rdi, r8, r9}
1737		mov rcx, qword ptr [rsp]
1738		mov rdx, qword ptr [rsp + 8]
1739		mov rsi, qword ptr [rsp + 16]
1740		mov rdi, qword ptr [rsp + 24]
1741		mov r8, qword ptr [rsp + 32]
1742		mov r9, qword ptr [rsp + 40]
1743		add rsp, 48
1744		# Stack pointer unchanged, no stack allocated variables
1745		# push {rcx, rdx, rsi, rdi, r8, r9}
1746		sub rsp, 48
1747		mov qword ptr [rsp], rcx
1748		mov qword ptr [rsp + 8], rdx
1749		mov qword ptr [rsp + 16], rsi
1750		mov qword ptr [rsp + 24], rdi
1751		mov qword ptr [rsp + 32], r8
1752		mov qword ptr [rsp + 40], r9
1753		# Set up R11 as a temporary second base pointer for the caller saved things
1754		mov r11, rsp
1755		# Stack pointer unchanged, no stack allocated arguments
1756		mov rax, r14
1757		mov rdi, rax
1758		# statement primitives do not return results (but will clobber r0/rax)
1759		call _printc
1760		call _println
1761		# pop {rcx, rdx, rsi, rdi, r8, r9}
1762		mov rcx, qword ptr [rsp]
1763		mov rdx, qword ptr [rsp + 8]
1764		mov rsi, qword ptr [rsp + 16]
1765		mov rdi, qword ptr [rsp + 24]
1766		mov r8, qword ptr [rsp + 32]
1767		mov r9, qword ptr [rsp + 40]
1768		add rsp, 48
1769		# Stack pointer unchanged, no stack allocated variables
1770		# push {rcx, rdx, rsi, rdi, r8, r9}
1771		sub rsp, 48
1772		mov qword ptr [rsp], rcx
1773		mov qword ptr [rsp + 8], rdx
1774		mov qword ptr [rsp + 16], rsi
1775		mov qword ptr [rsp + 24], rdi
1776		mov qword ptr [rsp + 32], r8
1777		mov qword ptr [rsp + 40], r9
1778		# Set up R11 as a temporary second base pointer for the caller saved things
1779		mov r11, rsp
1780		# Stack pointer unchanged, no stack allocated arguments
1781		mov rax, r13
1782		mov rdi, rax
1783		# statement primitives do not return results (but will clobber r0/rax)
1784		call _printi
1785		call _println
1786		# pop {rcx, rdx, rsi, rdi, r8, r9}
1787		mov rcx, qword ptr [rsp]
1788		mov rdx, qword ptr [rsp + 8]
1789		mov rsi, qword ptr [rsp + 16]
1790		mov rdi, qword ptr [rsp + 24]
1791		mov r8, qword ptr [rsp + 32]
1792		mov r9, qword ptr [rsp + 40]
1793		add rsp, 48
1794		# Stack pointer unchanged, no stack allocated variables
1795		mov rax, 0
1796		# pop {rbx, r12, r13, r14, r15}
1797		mov rbx, qword ptr [rsp]
1798		mov r12, qword ptr [rsp + 8]
1799		mov r13, qword ptr [rsp + 16]
1800		mov r14, qword ptr [rsp + 24]
1801		mov r15, qword ptr [rsp + 32]
1802		add rsp, 40
1803		pop rbp
1804		ret
1805	
1806	.section .rodata
1807	# length of .L._prints_str0
1808		.int 4
1809	.L._prints_str0:
1810		.asciz "%.*s"
1811	.text
1812	_prints:
1813		push rbp
1814		mov rbp, rsp
1815		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1816		and rsp, -16
1817		mov rdx, rdi
1818		mov esi, dword ptr [rdi - 4]
1819		lea rdi, [rip + .L._prints_str0]
1820		# on x86, al represents the number of SIMD registers used as variadic arguments
1821		mov al, 0
1822		call printf@plt
1823		mov rdi, 0
1824		call fflush@plt
1825		mov rsp, rbp
1826		pop rbp
1827		ret
1828	
1829	.section .rodata
1830	# length of .L._printp_str0
1831		.int 2
1832	.L._printp_str0:
1833		.asciz "%p"
1834	.text
1835	_printp:
1836		push rbp
1837		mov rbp, rsp
1838		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1839		and rsp, -16
1840		mov rsi, rdi
1841		lea rdi, [rip + .L._printp_str0]
1842		# on x86, al represents the number of SIMD registers used as variadic arguments
1843		mov al, 0
1844		call printf@plt
1845		mov rdi, 0
1846		call fflush@plt
1847		mov rsp, rbp
1848		pop rbp
1849		ret
1850	
1851	_malloc:
1852		push rbp
1853		mov rbp, rsp
1854		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1855		and rsp, -16
1856		call malloc@plt
1857		cmp rax, 0
1858		je _errOutOfMemory
1859		mov rsp, rbp
1860		pop rbp
1861		ret
1862	
1863	.section .rodata
1864	# length of .L._printi_str0
1865		.int 2
1866	.L._printi_str0:
1867		.asciz "%d"
1868	.text
1869	_printi:
1870		push rbp
1871		mov rbp, rsp
1872		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1873		and rsp, -16
1874		mov esi, edi
1875		lea rdi, [rip + .L._printi_str0]
1876		# on x86, al represents the number of SIMD registers used as variadic arguments
1877		mov al, 0
1878		call printf@plt
1879		mov rdi, 0
1880		call fflush@plt
1881		mov rsp, rbp
1882		pop rbp
1883		ret
1884	
1885	.section .rodata
1886	# length of .L._printb_str0
1887		.int 5
1888	.L._printb_str0:
1889		.asciz "false"
1890	# length of .L._printb_str1
1891		.int 4
1892	.L._printb_str1:
1893		.asciz "true"
1894	# length of .L._printb_str2
1895		.int 4
1896	.L._printb_str2:
1897		.asciz "%.*s"
1898	.text
1899	_printb:
1900		push rbp
1901		mov rbp, rsp
1902		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1903		and rsp, -16
1904		cmp dil, 0
1905		jne .L_printb0
1906		lea rdx, [rip + .L._printb_str0]
1907		jmp .L_printb1
1908	.L_printb0:
1909		lea rdx, [rip + .L._printb_str1]
1910	.L_printb1:
1911		mov esi, dword ptr [rdx - 4]
1912		lea rdi, [rip + .L._printb_str2]
1913		# on x86, al represents the number of SIMD registers used as variadic arguments
1914		mov al, 0
1915		call printf@plt
1916		mov rdi, 0
1917		call fflush@plt
1918		mov rsp, rbp
1919		pop rbp
1920		ret
1921	
1922	.section .rodata
1923	# length of .L._printc_str0
1924		.int 2
1925	.L._printc_str0:
1926		.asciz "%c"
1927	.text
1928	_printc:
1929		push rbp
1930		mov rbp, rsp
1931		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1932		and rsp, -16
1933		mov sil, dil
1934		lea rdi, [rip + .L._printc_str0]
1935		# on x86, al represents the number of SIMD registers used as variadic arguments
1936		mov al, 0
1937		call printf@plt
1938		mov rdi, 0
1939		call fflush@plt
1940		mov rsp, rbp
1941		pop rbp
1942		ret
1943	
1944	.section .rodata
1945	# length of .L._println_str0
1946		.int 0
1947	.L._println_str0:
1948		.asciz ""
1949	.text
1950	_println:
1951		push rbp
1952		mov rbp, rsp
1953		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1954		and rsp, -16
1955		lea rdi, [rip + .L._println_str0]
1956		call puts@plt
1957		mov rdi, 0
1958		call fflush@plt
1959		mov rsp, rbp
1960		pop rbp
1961		ret
1962	
1963	_exit:
1964		push rbp
1965		mov rbp, rsp
1966		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
1967		and rsp, -16
1968		call exit@plt
1969		mov rsp, rbp
1970		pop rbp
1971		ret
1972	
1973	_arrLoad8:
1974		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
1975		push rbx
1976		cmp r10d, 0
1977		cmovl esi, r10d
1978		jl _errOutOfBounds
1979		mov ebx, dword ptr [r9 - 4]
1980		cmp r10d, ebx
1981		cmovge esi, r10d
1982		jge _errOutOfBounds
1983		mov r9, qword ptr [r9 + 8*r10]
1984		pop rbx
1985		ret
1986	
1987	_arrLoad4:
1988		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
1989		push rbx
1990		cmp r10d, 0
1991		cmovl esi, r10d
1992		jl _errOutOfBounds
1993		mov ebx, dword ptr [r9 - 4]
1994		cmp r10d, ebx
1995		cmovge esi, r10d
1996		jge _errOutOfBounds
1997		movsx r9, dword ptr [r9 + 4*r10]
1998		pop rbx
1999		ret
2000	
2001	_arrLoad1:
2002		# Special calling convention: array ptr passed in R9, index in R10, and return into R9
2003		push rbx
2004		cmp r10d, 0
2005		cmovl esi, r10d
2006		jl _errOutOfBounds
2007		mov ebx, dword ptr [r9 - 4]
2008		cmp r10d, ebx
2009		cmovge esi, r10d
2010		jge _errOutOfBounds
2011		movsx r9, byte ptr [r9 + r10]
2012		pop rbx
2013		ret
2014	
2015	.section .rodata
2016	# length of .L._errOutOfMemory_str0
2017		.int 27
2018	.L._errOutOfMemory_str0:
2019		.asciz "fatal error: out of memory\n"
2020	.text
2021	_errOutOfMemory:
2022		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2023		and rsp, -16
2024		lea rdi, [rip + .L._errOutOfMemory_str0]
2025		call _prints
2026		mov dil, -1
2027		call exit@plt
2028	
2029	.section .rodata
2030	# length of .L._errNull_str0
2031		.int 45
2032	.L._errNull_str0:
2033		.asciz "fatal error: null pair dereferenced or freed\n"
2034	.text
2035	_errNull:
2036		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2037		and rsp, -16
2038		lea rdi, [rip + .L._errNull_str0]
2039		call _prints
2040		mov dil, -1
2041		call exit@plt
2042	
2043	.section .rodata
2044	# length of .L._errOutOfBounds_str0
2045		.int 42
2046	.L._errOutOfBounds_str0:
2047		.asciz "fatal error: array index %d out of bounds\n"
2048	.text
2049	_errOutOfBounds:
2050		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
2051		and rsp, -16
2052		lea rdi, [rip + .L._errOutOfBounds_str0]
2053		# on x86, al represents the number of SIMD registers used as variadic arguments
2054		mov al, 0
2055		call printf@plt
2056		mov rdi, 0
2057		call fflush@plt
2058		mov dil, -1
2059		call exit@plt
===========================================================
-- Finished

