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
0	.data
1	@ length of .L.str0
2		.word 5
3	.L.str0:
4		.asciz "a is "
5	@ length of .L.str1
6		.word 5
7	.L.str1:
8		.asciz "b is "
9	@ length of .L.str2
10		.word 5
11	.L.str2:
12		.asciz "c is "
13	@ length of .L.str3
14		.word 5
15	.L.str3:
16		.asciz "d is "
17	@ length of .L.str4
18		.word 5
19	.L.str4:
20		.asciz "e is "
21	@ length of .L.str5
22		.word 5
23	.L.str5:
24		.asciz "f is "
25	@ length of .L.str6
26		.word 5
27	.L.str6:
28		.asciz "hello"
29	@ length of .L.str7
30		.word 10
31	.L.str7:
32		.asciz "answer is "
33	.align 4
34	.text
35	.global main
36	main:
37		push {fp, lr}
38		push {r4, r5, r6, r8, r10, r12}
39		mov fp, sp
40		@ Stack pointer unchanged, no stack allocated variables
41		@ 2 element array
42		mov r0, #6
43		bl _malloc
44		mov r12, r0
45		@ array pointers are shifted forwards by 4 bytes (to account for size)
46		add r12, r12, #4
47		mov r8, #2
48		str r8, [r12, #-4]
49		mov r8, #0
50		strb r8, [r12, #0]
51		mov r8, #1
52		strb r8, [r12, #1]
53		mov r8, r12
54		mov r4, r8
55		@ 2 element array
56		mov r0, #12
57		bl _malloc
58		mov r12, r0
59		@ array pointers are shifted forwards by 4 bytes (to account for size)
60		add r12, r12, #4
61		mov r8, #2
62		str r8, [r12, #-4]
63		mov r8, #1
64		str r8, [r12, #0]
65		mov r8, #2
66		str r8, [r12, #4]
67		mov r8, r12
68		mov r5, r8
69		@ Stack padded to a multiple of the required alignment
70		sub sp, sp, #12
71		mov r8, #42
72		mov r0, r8
73		mov r8, #1
74		mov r1, r8
75		mov r8, #117
76		mov r2, r8
77		ldr r8, =.L.str6
78		push {r8}
79		pop {r8}
80		mov r8, r8
81		mov r3, r8
82		mov r8, r4
83		str r8, [sp, #4]
84		mov r8, r5
85		str r8, [sp, #0]
86		bl wacc_doSomething
87		mov r12, r0
88		@ Stack padded to a multiple of the required alignment
89		add sp, sp, #12
90		mov r8, r12
91		mov r6, r8
92		@ Stack pointer unchanged, no stack allocated arguments
93		ldr r8, =.L.str7
94		push {r8}
95		pop {r8}
96		mov r8, r8
97		mov r0, r8
98		@ statement primitives do not return results (but will clobber r0/rax)
99		bl _prints
100		@ Stack pointer unchanged, no stack allocated arguments
101		mov r8, r6
102		mov r0, r8
103		@ statement primitives do not return results (but will clobber r0/rax)
104		bl _printc
105		bl _println
106		@ Stack pointer unchanged, no stack allocated variables
107		mov r0, #0
108		pop {r4, r5, r6, r8, r10, r12}
109		pop {fp, pc}
110	
111	wacc_doSomething:
112		push {fp, lr}
113		mov fp, sp
114		push {r0, r1, r2, r3}
115		@ Set up R12 as a temporary second base pointer for the caller saved things
116		mov r12, sp
117		@ Stack pointer unchanged, no stack allocated arguments
118		ldr r8, =.L.str0
119		push {r8}
120		pop {r8}
121		mov r8, r8
122		mov r0, r8
123		@ statement primitives do not return results (but will clobber r0/rax)
124		bl _prints
125		pop {r0, r1, r2, r3}
126		push {r0, r1, r2, r3}
127		@ Set up R12 as a temporary second base pointer for the caller saved things
128		mov r12, sp
129		@ Stack pointer unchanged, no stack allocated arguments
130		mov r8, r0
131		mov r0, r8
132		@ statement primitives do not return results (but will clobber r0/rax)
133		bl _printi
134		bl _println
135		pop {r0, r1, r2, r3}
136		push {r0, r1, r2, r3}
137		@ Set up R12 as a temporary second base pointer for the caller saved things
138		mov r12, sp
139		@ Stack pointer unchanged, no stack allocated arguments
140		ldr r8, =.L.str1
141		push {r8}
142		pop {r8}
143		mov r8, r8
144		mov r0, r8
145		@ statement primitives do not return results (but will clobber r0/rax)
146		bl _prints
147		pop {r0, r1, r2, r3}
148		push {r0, r1, r2, r3}
149		@ Set up R12 as a temporary second base pointer for the caller saved things
150		mov r12, sp
151		@ Stack pointer unchanged, no stack allocated arguments
152		mov r8, r1
153		mov r0, r8
154		@ statement primitives do not return results (but will clobber r0/rax)
155		bl _printb
156		bl _println
157		pop {r0, r1, r2, r3}
158		push {r0, r1, r2, r3}
159		@ Set up R12 as a temporary second base pointer for the caller saved things
160		mov r12, sp
161		@ Stack pointer unchanged, no stack allocated arguments
162		ldr r8, =.L.str2
163		push {r8}
164		pop {r8}
165		mov r8, r8
166		mov r0, r8
167		@ statement primitives do not return results (but will clobber r0/rax)
168		bl _prints
169		pop {r0, r1, r2, r3}
170		push {r0, r1, r2, r3}
171		@ Set up R12 as a temporary second base pointer for the caller saved things
172		mov r12, sp
173		@ Stack pointer unchanged, no stack allocated arguments
174		mov r8, r2
175		mov r0, r8
176		@ statement primitives do not return results (but will clobber r0/rax)
177		bl _printc
178		bl _println
179		pop {r0, r1, r2, r3}
180		push {r0, r1, r2, r3}
181		@ Set up R12 as a temporary second base pointer for the caller saved things
182		mov r12, sp
183		@ Stack pointer unchanged, no stack allocated arguments
184		ldr r8, =.L.str3
185		push {r8}
186		pop {r8}
187		mov r8, r8
188		mov r0, r8
189		@ statement primitives do not return results (but will clobber r0/rax)
190		bl _prints
191		pop {r0, r1, r2, r3}
192		push {r0, r1, r2, r3}
193		@ Set up R12 as a temporary second base pointer for the caller saved things
194		mov r12, sp
195		@ Stack pointer unchanged, no stack allocated arguments
196		mov r8, r3
197		mov r0, r8
198		@ statement primitives do not return results (but will clobber r0/rax)
199		bl _prints
200		bl _println
201		pop {r0, r1, r2, r3}
202		push {r0, r1, r2, r3}
203		@ Set up R12 as a temporary second base pointer for the caller saved things
204		mov r12, sp
205		@ Stack pointer unchanged, no stack allocated arguments
206		ldr r8, =.L.str4
207		push {r8}
208		pop {r8}
209		mov r8, r8
210		mov r0, r8
211		@ statement primitives do not return results (but will clobber r0/rax)
212		bl _prints
213		pop {r0, r1, r2, r3}
214		push {r0, r1, r2, r3}
215		@ Set up R12 as a temporary second base pointer for the caller saved things
216		mov r12, sp
217		@ Stack pointer unchanged, no stack allocated arguments
218		ldr r8, [fp, #12]
219		mov r8, r8
220		mov r0, r8
221		@ statement primitives do not return results (but will clobber r0/rax)
222		bl _printp
223		bl _println
224		pop {r0, r1, r2, r3}
225		push {r0, r1, r2, r3}
226		@ Set up R12 as a temporary second base pointer for the caller saved things
227		mov r12, sp
228		@ Stack pointer unchanged, no stack allocated arguments
229		ldr r8, =.L.str5
230		push {r8}
231		pop {r8}
232		mov r8, r8
233		mov r0, r8
234		@ statement primitives do not return results (but will clobber r0/rax)
235		bl _prints
236		pop {r0, r1, r2, r3}
237		push {r0, r1, r2, r3}
238		@ Set up R12 as a temporary second base pointer for the caller saved things
239		mov r12, sp
240		@ Stack pointer unchanged, no stack allocated arguments
241		ldr r8, [fp, #8]
242		mov r8, r8
243		mov r0, r8
244		@ statement primitives do not return results (but will clobber r0/rax)
245		bl _printp
246		bl _println
247		pop {r0, r1, r2, r3}
248		mov r0, #103
249		@ reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
250		mov sp, fp
251		pop {fp, pc}
252		@ 'ere be dragons: this is 100% dead code, functions always end in returns!
253		@ the .ltorg here is used to generate the constant pool after every function
254		@ this is important for ensuring string literals and large integers are accessible!
255		.ltorg
256	
257	@ length of .L._prints_str0
258		.word 4
259	.L._prints_str0:
260		.asciz "%.*s"
261	.align 4
262	_prints:
263		push {fp, lr}
264		mov fp, sp
265		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
266		bic sp, sp, #0x7
267		mov r2, r0
268		ldr r1, [r0, #-4]
269		adr r0, .L._prints_str0
270		bl printf
271		mov r0, #0
272		bl fflush
273		mov sp, fp
274		pop {fp, pc}
275	
276	@ length of .L._printp_str0
277		.word 2
278	.L._printp_str0:
279		.asciz "%p"
280	.align 4
281	_printp:
282		push {fp, lr}
283		mov fp, sp
284		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
285		bic sp, sp, #0x7
286		mov r1, r0
287		adr r0, .L._printp_str0
288		bl printf
289		mov r0, #0
290		bl fflush
291		mov sp, fp
292		pop {fp, pc}
293	
294	_malloc:
295		push {fp, lr}
296		mov fp, sp
297		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
298		bic sp, sp, #0x7
299		bl malloc
300		cmp r0, #0
301		bleq _errOutOfMemory
302		mov sp, fp
303		pop {fp, pc}
304	
305	@ length of .L._printi_str0
306		.word 2
307	.L._printi_str0:
308		.asciz "%d"
309	.align 4
310	_printi:
311		push {fp, lr}
312		mov fp, sp
313		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
314		bic sp, sp, #0x7
315		mov r1, r0
316		adr r0, .L._printi_str0
317		bl printf
318		mov r0, #0
319		bl fflush
320		mov sp, fp
321		pop {fp, pc}
322	
323	@ length of .L._printb_str0
324		.word 5
325	.L._printb_str0:
326		.asciz "false"
327	@ length of .L._printb_str1
328		.word 4
329	.L._printb_str1:
330		.asciz "true"
331	@ length of .L._printb_str2
332		.word 4
333	.L._printb_str2:
334		.asciz "%.*s"
335	.align 4
336	_printb:
337		push {fp, lr}
338		mov fp, sp
339		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
340		bic sp, sp, #0x7
341		cmp r0, #0
342		bne .L_printb0
343		adr r2, .L._printb_str0
344		b .L_printb1
345	.L_printb0:
346		adr r2, .L._printb_str1
347	.L_printb1:
348		ldr r1, [r2, #-4]
349		adr r0, .L._printb_str2
350		bl printf
351		mov r0, #0
352		bl fflush
353		mov sp, fp
354		pop {fp, pc}
355	
356	@ length of .L._printc_str0
357		.word 2
358	.L._printc_str0:
359		.asciz "%c"
360	.align 4
361	_printc:
362		push {fp, lr}
363		mov fp, sp
364		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
365		bic sp, sp, #0x7
366		mov r1, r0
367		adr r0, .L._printc_str0
368		bl printf
369		mov r0, #0
370		bl fflush
371		mov sp, fp
372		pop {fp, pc}
373	
374	@ length of .L._println_str0
375		.word 0
376	.L._println_str0:
377		.asciz ""
378	.align 4
379	_println:
380		push {fp, lr}
381		mov fp, sp
382		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
383		bic sp, sp, #0x7
384		adr r0, .L._println_str0
385		bl puts
386		mov r0, #0
387		bl fflush
388		mov sp, fp
389		pop {fp, pc}
390	
391	_exit:
392		push {fp, lr}
393		mov fp, sp
394		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
395		bic sp, sp, #0x7
396		bl exit
397		mov sp, fp
398		pop {fp, pc}
399	
400	@ length of .L._errOutOfMemory_str0
401		.word 27
402	.L._errOutOfMemory_str0:
403		.asciz "fatal error: out of memory\n"
404	.align 4
405	_errOutOfMemory:
406		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
407		bic sp, sp, #0x7
408		adr r0, .L._errOutOfMemory_str0
409		bl _prints
410		mov r0, #255
411		bl _exit
===========================================================
-- Finished

