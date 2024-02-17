./valid/expressions/longSplitExpr2.wacc
calling the reference compiler on ./valid/expressions/longSplitExpr2.wacc
-- Test: longSplitExpr2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests whether the compiler can handle long expressions with several variables

# Output:
# 362880
# 128
#

# Exit:
# 128

begin

  int x = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 ;
  int y = -1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9 - 10 - 11 - 12 - 13 - 14 - 15 - 16 - 17 ;
  int z = 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 ;
  int div = 10 ;
  println x + y + ( z / div ) ;
  println (x + y + ( z / div )) % 256 ;
  exit x + y + ( z / div )

end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
longSplitExpr2.s contents are:
===========================================================
0	.globl main
1	.section .rodata
2	.text
3	main:
4		pushq %rbp
5		# pushq {%rbx, %r12, %r13, %r14, %r15}
6		subq $40, %rsp
7		movq %rbx, (%rsp)
8		movq %r12, 8(%rsp)
9		movq %r13, 16(%rsp)
10		movq %r14, 24(%rsp)
11		movq %r15, 32(%rsp)
12		movq %rsp, %rbp
13		# Stack pointer unchanged, no stack allocated variables
14		movl $1, %eax
15		addl $2, %eax
16		jo _errOverflow
17		movslq %eax, %rax
18		pushq %rax
19		popq %rax
20		movl %eax, %eax
21		addl $3, %eax
22		jo _errOverflow
23		movslq %eax, %rax
24		pushq %rax
25		popq %rax
26		movl %eax, %eax
27		addl $4, %eax
28		jo _errOverflow
29		movslq %eax, %rax
30		pushq %rax
31		popq %rax
32		movl %eax, %eax
33		addl $5, %eax
34		jo _errOverflow
35		movslq %eax, %rax
36		pushq %rax
37		popq %rax
38		movl %eax, %eax
39		addl $6, %eax
40		jo _errOverflow
41		movslq %eax, %rax
42		pushq %rax
43		popq %rax
44		movl %eax, %eax
45		addl $7, %eax
46		jo _errOverflow
47		movslq %eax, %rax
48		pushq %rax
49		popq %rax
50		movl %eax, %eax
51		addl $8, %eax
52		jo _errOverflow
53		movslq %eax, %rax
54		pushq %rax
55		popq %rax
56		movl %eax, %eax
57		addl $9, %eax
58		jo _errOverflow
59		movslq %eax, %rax
60		pushq %rax
61		popq %rax
62		movl %eax, %eax
63		addl $10, %eax
64		jo _errOverflow
65		movslq %eax, %rax
66		pushq %rax
67		popq %rax
68		movl %eax, %eax
69		addl $11, %eax
70		jo _errOverflow
71		movslq %eax, %rax
72		pushq %rax
73		popq %rax
74		movl %eax, %eax
75		addl $12, %eax
76		jo _errOverflow
77		movslq %eax, %rax
78		pushq %rax
79		popq %rax
80		movl %eax, %eax
81		addl $13, %eax
82		jo _errOverflow
83		movslq %eax, %rax
84		pushq %rax
85		popq %rax
86		movl %eax, %eax
87		addl $14, %eax
88		jo _errOverflow
89		movslq %eax, %rax
90		pushq %rax
91		popq %rax
92		movl %eax, %eax
93		addl $15, %eax
94		jo _errOverflow
95		movslq %eax, %rax
96		pushq %rax
97		popq %rax
98		movl %eax, %eax
99		addl $16, %eax
100		jo _errOverflow
101		movslq %eax, %rax
102		pushq %rax
103		popq %rax
104		movl %eax, %eax
105		addl $17, %eax
106		jo _errOverflow
107		movslq %eax, %rax
108		pushq %rax
109		popq %rax
110		movq %rax, %rax
111		movq %rax, %r12
112		movl $-1, %eax
113		subl $2, %eax
114		jo _errOverflow
115		movslq %eax, %rax
116		pushq %rax
117		popq %rax
118		movl %eax, %eax
119		subl $3, %eax
120		jo _errOverflow
121		movslq %eax, %rax
122		pushq %rax
123		popq %rax
124		movl %eax, %eax
125		subl $4, %eax
126		jo _errOverflow
127		movslq %eax, %rax
128		pushq %rax
129		popq %rax
130		movl %eax, %eax
131		subl $5, %eax
132		jo _errOverflow
133		movslq %eax, %rax
134		pushq %rax
135		popq %rax
136		movl %eax, %eax
137		subl $6, %eax
138		jo _errOverflow
139		movslq %eax, %rax
140		pushq %rax
141		popq %rax
142		movl %eax, %eax
143		subl $7, %eax
144		jo _errOverflow
145		movslq %eax, %rax
146		pushq %rax
147		popq %rax
148		movl %eax, %eax
149		subl $8, %eax
150		jo _errOverflow
151		movslq %eax, %rax
152		pushq %rax
153		popq %rax
154		movl %eax, %eax
155		subl $9, %eax
156		jo _errOverflow
157		movslq %eax, %rax
158		pushq %rax
159		popq %rax
160		movl %eax, %eax
161		subl $10, %eax
162		jo _errOverflow
163		movslq %eax, %rax
164		pushq %rax
165		popq %rax
166		movl %eax, %eax
167		subl $11, %eax
168		jo _errOverflow
169		movslq %eax, %rax
170		pushq %rax
171		popq %rax
172		movl %eax, %eax
173		subl $12, %eax
174		jo _errOverflow
175		movslq %eax, %rax
176		pushq %rax
177		popq %rax
178		movl %eax, %eax
179		subl $13, %eax
180		jo _errOverflow
181		movslq %eax, %rax
182		pushq %rax
183		popq %rax
184		movl %eax, %eax
185		subl $14, %eax
186		jo _errOverflow
187		movslq %eax, %rax
188		pushq %rax
189		popq %rax
190		movl %eax, %eax
191		subl $15, %eax
192		jo _errOverflow
193		movslq %eax, %rax
194		pushq %rax
195		popq %rax
196		movl %eax, %eax
197		subl $16, %eax
198		jo _errOverflow
199		movslq %eax, %rax
200		pushq %rax
201		popq %rax
202		movl %eax, %eax
203		subl $17, %eax
204		jo _errOverflow
205		movslq %eax, %rax
206		pushq %rax
207		popq %rax
208		movq %rax, %rax
209		movq %rax, %r13
210		movq $1, %rax
211		imull $2, %eax, %eax
212		jo _errOverflow
213		movslq %eax, %rax
214		pushq %rax
215		popq %rax
216		imull $3, %eax, %eax
217		jo _errOverflow
218		movslq %eax, %rax
219		pushq %rax
220		popq %rax
221		imull $4, %eax, %eax
222		jo _errOverflow
223		movslq %eax, %rax
224		pushq %rax
225		popq %rax
226		imull $5, %eax, %eax
227		jo _errOverflow
228		movslq %eax, %rax
229		pushq %rax
230		popq %rax
231		imull $6, %eax, %eax
232		jo _errOverflow
233		movslq %eax, %rax
234		pushq %rax
235		popq %rax
236		imull $7, %eax, %eax
237		jo _errOverflow
238		movslq %eax, %rax
239		pushq %rax
240		popq %rax
241		imull $8, %eax, %eax
242		jo _errOverflow
243		movslq %eax, %rax
244		pushq %rax
245		popq %rax
246		imull $9, %eax, %eax
247		jo _errOverflow
248		movslq %eax, %rax
249		pushq %rax
250		popq %rax
251		imull $10, %eax, %eax
252		jo _errOverflow
253		movslq %eax, %rax
254		pushq %rax
255		popq %rax
256		movq %rax, %rax
257		movq %rax, %r14
258		movq $10, %rax
259		movq %rax, %r15
260		# Stack pointer unchanged, no stack allocated arguments
261		movl %r12d, %eax
262		addl %r13d, %eax
263		jo _errOverflow
264		movslq %eax, %rax
265		pushq %rax
266		movl %r14d, %eax
267		cmpl $0, %r15d
268		je _errDivZero
269		# sign extend EAX into EDX
270		cltd
271		idivl %r15d
272		movl %eax, %eax
273		movl %eax, %eax
274		movslq %eax, %rax
275		pushq %rax
276		popq %rbx
277		popq %rax
278		movl %eax, %eax
279		addl %ebx, %eax
280		jo _errOverflow
281		movslq %eax, %rax
282		pushq %rax
283		popq %rax
284		movq %rax, %rax
285		movq %rax, %rdi
286		# statement primitives do not return results (but will clobber r0/rax)
287		call _printi
288		call _println
289		# Stack pointer unchanged, no stack allocated arguments
290		movl %r12d, %eax
291		addl %r13d, %eax
292		jo _errOverflow
293		movslq %eax, %rax
294		pushq %rax
295		movl %r14d, %eax
296		cmpl $0, %r15d
297		je _errDivZero
298		# sign extend EAX into EDX
299		cltd
300		idivl %r15d
301		movl %eax, %eax
302		movl %eax, %eax
303		movslq %eax, %rax
304		pushq %rax
305		popq %rbx
306		popq %rax
307		movl %eax, %eax
308		addl %ebx, %eax
309		jo _errOverflow
310		movslq %eax, %rax
311		pushq %rax
312		movq $256, %rbx
313		popq %rax
314		movl %eax, %eax
315		cmpl $0, %ebx
316		je _errDivZero
317		# sign extend EAX into EDX
318		cltd
319		idivl %ebx
320		movl %edx, %eax
321		movl %eax, %eax
322		movslq %eax, %rax
323		pushq %rax
324		popq %rax
325		movq %rax, %rax
326		movq %rax, %rdi
327		# statement primitives do not return results (but will clobber r0/rax)
328		call _printi
329		call _println
330		# Stack pointer unchanged, no stack allocated arguments
331		movl %r12d, %eax
332		addl %r13d, %eax
333		jo _errOverflow
334		movslq %eax, %rax
335		pushq %rax
336		movl %r14d, %eax
337		cmpl $0, %r15d
338		je _errDivZero
339		# sign extend EAX into EDX
340		cltd
341		idivl %r15d
342		movl %eax, %eax
343		movl %eax, %eax
344		movslq %eax, %rax
345		pushq %rax
346		popq %rbx
347		popq %rax
348		movl %eax, %eax
349		addl %ebx, %eax
350		jo _errOverflow
351		movslq %eax, %rax
352		pushq %rax
353		popq %rax
354		movq %rax, %rax
355		movq %rax, %rdi
356		# statement primitives do not return results (but will clobber r0/rax)
357		call _exit
358		# Stack pointer unchanged, no stack allocated variables
359		movq $0, %rax
360		# popq {%rbx, %r12, %r13, %r14, %r15}
361		movq (%rsp), %rbx
362		movq 8(%rsp), %r12
363		movq 16(%rsp), %r13
364		movq 24(%rsp), %r14
365		movq 32(%rsp), %r15
366		addq $40, %rsp
367		popq %rbp
368		ret
369	
370	.section .rodata
371	# length of .L._prints_str0
372		.int 4
373	.L._prints_str0:
374		.asciz "%.*s"
375	.text
376	_prints:
377		pushq %rbp
378		movq %rsp, %rbp
379		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
380		andq $-16, %rsp
381		movq %rdi, %rdx
382		movl -4(%rdi), %esi
383		leaq .L._prints_str0(%rip), %rdi
384		# on x86, al represents the number of SIMD registers used as variadic arguments
385		movb $0, %al
386		call printf@plt
387		movq $0, %rdi
388		call fflush@plt
389		movq %rbp, %rsp
390		popq %rbp
391		ret
392	
393	.section .rodata
394	# length of .L._printi_str0
395		.int 2
396	.L._printi_str0:
397		.asciz "%d"
398	.text
399	_printi:
400		pushq %rbp
401		movq %rsp, %rbp
402		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
403		andq $-16, %rsp
404		movl %edi, %esi
405		leaq .L._printi_str0(%rip), %rdi
406		# on x86, al represents the number of SIMD registers used as variadic arguments
407		movb $0, %al
408		call printf@plt
409		movq $0, %rdi
410		call fflush@plt
411		movq %rbp, %rsp
412		popq %rbp
413		ret
414	
415	.section .rodata
416	# length of .L._println_str0
417		.int 0
418	.L._println_str0:
419		.asciz ""
420	.text
421	_println:
422		pushq %rbp
423		movq %rsp, %rbp
424		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
425		andq $-16, %rsp
426		leaq .L._println_str0(%rip), %rdi
427		call puts@plt
428		movq $0, %rdi
429		call fflush@plt
430		movq %rbp, %rsp
431		popq %rbp
432		ret
433	
434	_exit:
435		pushq %rbp
436		movq %rsp, %rbp
437		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
438		andq $-16, %rsp
439		call exit@plt
440		movq %rbp, %rsp
441		popq %rbp
442		ret
443	
444	.section .rodata
445	# length of .L._errDivZero_str0
446		.int 40
447	.L._errDivZero_str0:
448		.asciz "fatal error: division or modulo by zero\n"
449	.text
450	_errDivZero:
451		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
452		andq $-16, %rsp
453		leaq .L._errDivZero_str0(%rip), %rdi
454		call _prints
455		movb $-1, %dil
456		call exit@plt
457	
458	.section .rodata
459	# length of .L._errOverflow_str0
460		.int 52
461	.L._errOverflow_str0:
462		.asciz "fatal error: integer overflow or underflow occurred\n"
463	.text
464	_errOverflow:
465		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
466		andq $-16, %rsp
467		leaq .L._errOverflow_str0(%rip), %rdi
468		call _prints
469		movb $-1, %dil
470		call exit@plt
===========================================================
-- Finished

