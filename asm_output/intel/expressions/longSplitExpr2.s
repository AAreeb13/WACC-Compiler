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
0	.intel_syntax noprefix
1	.globl main
2	.section .rodata
3	.text
4	main:
5		push rbp
6		# push {rbx, r12, r13, r14, r15}
7		sub rsp, 40
8		mov qword ptr [rsp], rbx
9		mov qword ptr [rsp + 8], r12
10		mov qword ptr [rsp + 16], r13
11		mov qword ptr [rsp + 24], r14
12		mov qword ptr [rsp + 32], r15
13		mov rbp, rsp
14		# Stack pointer unchanged, no stack allocated variables
15		mov eax, 1
16		add eax, 2
17		jo _errOverflow
18		movsx rax, eax
19		push rax
20		pop rax
21		mov eax, eax
22		add eax, 3
23		jo _errOverflow
24		movsx rax, eax
25		push rax
26		pop rax
27		mov eax, eax
28		add eax, 4
29		jo _errOverflow
30		movsx rax, eax
31		push rax
32		pop rax
33		mov eax, eax
34		add eax, 5
35		jo _errOverflow
36		movsx rax, eax
37		push rax
38		pop rax
39		mov eax, eax
40		add eax, 6
41		jo _errOverflow
42		movsx rax, eax
43		push rax
44		pop rax
45		mov eax, eax
46		add eax, 7
47		jo _errOverflow
48		movsx rax, eax
49		push rax
50		pop rax
51		mov eax, eax
52		add eax, 8
53		jo _errOverflow
54		movsx rax, eax
55		push rax
56		pop rax
57		mov eax, eax
58		add eax, 9
59		jo _errOverflow
60		movsx rax, eax
61		push rax
62		pop rax
63		mov eax, eax
64		add eax, 10
65		jo _errOverflow
66		movsx rax, eax
67		push rax
68		pop rax
69		mov eax, eax
70		add eax, 11
71		jo _errOverflow
72		movsx rax, eax
73		push rax
74		pop rax
75		mov eax, eax
76		add eax, 12
77		jo _errOverflow
78		movsx rax, eax
79		push rax
80		pop rax
81		mov eax, eax
82		add eax, 13
83		jo _errOverflow
84		movsx rax, eax
85		push rax
86		pop rax
87		mov eax, eax
88		add eax, 14
89		jo _errOverflow
90		movsx rax, eax
91		push rax
92		pop rax
93		mov eax, eax
94		add eax, 15
95		jo _errOverflow
96		movsx rax, eax
97		push rax
98		pop rax
99		mov eax, eax
100		add eax, 16
101		jo _errOverflow
102		movsx rax, eax
103		push rax
104		pop rax
105		mov eax, eax
106		add eax, 17
107		jo _errOverflow
108		movsx rax, eax
109		push rax
110		pop rax
111		mov rax, rax
112		mov r12, rax
113		mov eax, -1
114		sub eax, 2
115		jo _errOverflow
116		movsx rax, eax
117		push rax
118		pop rax
119		mov eax, eax
120		sub eax, 3
121		jo _errOverflow
122		movsx rax, eax
123		push rax
124		pop rax
125		mov eax, eax
126		sub eax, 4
127		jo _errOverflow
128		movsx rax, eax
129		push rax
130		pop rax
131		mov eax, eax
132		sub eax, 5
133		jo _errOverflow
134		movsx rax, eax
135		push rax
136		pop rax
137		mov eax, eax
138		sub eax, 6
139		jo _errOverflow
140		movsx rax, eax
141		push rax
142		pop rax
143		mov eax, eax
144		sub eax, 7
145		jo _errOverflow
146		movsx rax, eax
147		push rax
148		pop rax
149		mov eax, eax
150		sub eax, 8
151		jo _errOverflow
152		movsx rax, eax
153		push rax
154		pop rax
155		mov eax, eax
156		sub eax, 9
157		jo _errOverflow
158		movsx rax, eax
159		push rax
160		pop rax
161		mov eax, eax
162		sub eax, 10
163		jo _errOverflow
164		movsx rax, eax
165		push rax
166		pop rax
167		mov eax, eax
168		sub eax, 11
169		jo _errOverflow
170		movsx rax, eax
171		push rax
172		pop rax
173		mov eax, eax
174		sub eax, 12
175		jo _errOverflow
176		movsx rax, eax
177		push rax
178		pop rax
179		mov eax, eax
180		sub eax, 13
181		jo _errOverflow
182		movsx rax, eax
183		push rax
184		pop rax
185		mov eax, eax
186		sub eax, 14
187		jo _errOverflow
188		movsx rax, eax
189		push rax
190		pop rax
191		mov eax, eax
192		sub eax, 15
193		jo _errOverflow
194		movsx rax, eax
195		push rax
196		pop rax
197		mov eax, eax
198		sub eax, 16
199		jo _errOverflow
200		movsx rax, eax
201		push rax
202		pop rax
203		mov eax, eax
204		sub eax, 17
205		jo _errOverflow
206		movsx rax, eax
207		push rax
208		pop rax
209		mov rax, rax
210		mov r13, rax
211		mov rax, 1
212		imul eax, eax, 2
213		jo _errOverflow
214		movsx rax, eax
215		push rax
216		pop rax
217		imul eax, eax, 3
218		jo _errOverflow
219		movsx rax, eax
220		push rax
221		pop rax
222		imul eax, eax, 4
223		jo _errOverflow
224		movsx rax, eax
225		push rax
226		pop rax
227		imul eax, eax, 5
228		jo _errOverflow
229		movsx rax, eax
230		push rax
231		pop rax
232		imul eax, eax, 6
233		jo _errOverflow
234		movsx rax, eax
235		push rax
236		pop rax
237		imul eax, eax, 7
238		jo _errOverflow
239		movsx rax, eax
240		push rax
241		pop rax
242		imul eax, eax, 8
243		jo _errOverflow
244		movsx rax, eax
245		push rax
246		pop rax
247		imul eax, eax, 9
248		jo _errOverflow
249		movsx rax, eax
250		push rax
251		pop rax
252		imul eax, eax, 10
253		jo _errOverflow
254		movsx rax, eax
255		push rax
256		pop rax
257		mov rax, rax
258		mov r14, rax
259		mov rax, 10
260		mov r15, rax
261		# Stack pointer unchanged, no stack allocated arguments
262		mov eax, r12d
263		add eax, r13d
264		jo _errOverflow
265		movsx rax, eax
266		push rax
267		mov eax, r14d
268		cmp r15d, 0
269		je _errDivZero
270		# sign extend EAX into EDX
271		cdq
272		idiv r15d
273		mov eax, eax
274		mov eax, eax
275		movsx rax, eax
276		push rax
277		pop rbx
278		pop rax
279		mov eax, eax
280		add eax, ebx
281		jo _errOverflow
282		movsx rax, eax
283		push rax
284		pop rax
285		mov rax, rax
286		mov rdi, rax
287		# statement primitives do not return results (but will clobber r0/rax)
288		call _printi
289		call _println
290		# Stack pointer unchanged, no stack allocated arguments
291		mov eax, r12d
292		add eax, r13d
293		jo _errOverflow
294		movsx rax, eax
295		push rax
296		mov eax, r14d
297		cmp r15d, 0
298		je _errDivZero
299		# sign extend EAX into EDX
300		cdq
301		idiv r15d
302		mov eax, eax
303		mov eax, eax
304		movsx rax, eax
305		push rax
306		pop rbx
307		pop rax
308		mov eax, eax
309		add eax, ebx
310		jo _errOverflow
311		movsx rax, eax
312		push rax
313		mov rbx, 256
314		pop rax
315		mov eax, eax
316		cmp ebx, 0
317		je _errDivZero
318		# sign extend EAX into EDX
319		cdq
320		idiv ebx
321		mov eax, edx
322		mov eax, eax
323		movsx rax, eax
324		push rax
325		pop rax
326		mov rax, rax
327		mov rdi, rax
328		# statement primitives do not return results (but will clobber r0/rax)
329		call _printi
330		call _println
331		# Stack pointer unchanged, no stack allocated arguments
332		mov eax, r12d
333		add eax, r13d
334		jo _errOverflow
335		movsx rax, eax
336		push rax
337		mov eax, r14d
338		cmp r15d, 0
339		je _errDivZero
340		# sign extend EAX into EDX
341		cdq
342		idiv r15d
343		mov eax, eax
344		mov eax, eax
345		movsx rax, eax
346		push rax
347		pop rbx
348		pop rax
349		mov eax, eax
350		add eax, ebx
351		jo _errOverflow
352		movsx rax, eax
353		push rax
354		pop rax
355		mov rax, rax
356		mov rdi, rax
357		# statement primitives do not return results (but will clobber r0/rax)
358		call _exit
359		# Stack pointer unchanged, no stack allocated variables
360		mov rax, 0
361		# pop {rbx, r12, r13, r14, r15}
362		mov rbx, qword ptr [rsp]
363		mov r12, qword ptr [rsp + 8]
364		mov r13, qword ptr [rsp + 16]
365		mov r14, qword ptr [rsp + 24]
366		mov r15, qword ptr [rsp + 32]
367		add rsp, 40
368		pop rbp
369		ret
370	
371	.section .rodata
372	# length of .L._prints_str0
373		.int 4
374	.L._prints_str0:
375		.asciz "%.*s"
376	.text
377	_prints:
378		push rbp
379		mov rbp, rsp
380		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
381		and rsp, -16
382		mov rdx, rdi
383		mov esi, dword ptr [rdi - 4]
384		lea rdi, [rip + .L._prints_str0]
385		# on x86, al represents the number of SIMD registers used as variadic arguments
386		mov al, 0
387		call printf@plt
388		mov rdi, 0
389		call fflush@plt
390		mov rsp, rbp
391		pop rbp
392		ret
393	
394	.section .rodata
395	# length of .L._printi_str0
396		.int 2
397	.L._printi_str0:
398		.asciz "%d"
399	.text
400	_printi:
401		push rbp
402		mov rbp, rsp
403		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
404		and rsp, -16
405		mov esi, edi
406		lea rdi, [rip + .L._printi_str0]
407		# on x86, al represents the number of SIMD registers used as variadic arguments
408		mov al, 0
409		call printf@plt
410		mov rdi, 0
411		call fflush@plt
412		mov rsp, rbp
413		pop rbp
414		ret
415	
416	.section .rodata
417	# length of .L._println_str0
418		.int 0
419	.L._println_str0:
420		.asciz ""
421	.text
422	_println:
423		push rbp
424		mov rbp, rsp
425		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
426		and rsp, -16
427		lea rdi, [rip + .L._println_str0]
428		call puts@plt
429		mov rdi, 0
430		call fflush@plt
431		mov rsp, rbp
432		pop rbp
433		ret
434	
435	_exit:
436		push rbp
437		mov rbp, rsp
438		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
439		and rsp, -16
440		call exit@plt
441		mov rsp, rbp
442		pop rbp
443		ret
444	
445	.section .rodata
446	# length of .L._errDivZero_str0
447		.int 40
448	.L._errDivZero_str0:
449		.asciz "fatal error: division or modulo by zero\n"
450	.text
451	_errDivZero:
452		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
453		and rsp, -16
454		lea rdi, [rip + .L._errDivZero_str0]
455		call _prints
456		mov dil, -1
457		call exit@plt
458	
459	.section .rodata
460	# length of .L._errOverflow_str0
461		.int 52
462	.L._errOverflow_str0:
463		.asciz "fatal error: integer overflow or underflow occurred\n"
464	.text
465	_errOverflow:
466		# external calls must be stack-aligned to 16 bytes, accomplished by masking with fffffffffffffff0
467		and rsp, -16
468		lea rdi, [rip + .L._errOverflow_str0]
469		call _prints
470		mov dil, -1
471		call exit@plt
===========================================================
-- Finished

