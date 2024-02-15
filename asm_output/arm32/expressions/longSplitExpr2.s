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
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		push {fp, lr}
6		push {r4, r5, r6, r7, r8, r10, r12}
7		mov fp, sp
8		@ Stack pointer unchanged, no stack allocated variables
9		mov r8, #1
10		adds r8, r8, #2
11		blvs _errOverflow
12		push {r8}
13		pop {r8}
14		adds r8, r8, #3
15		blvs _errOverflow
16		push {r8}
17		pop {r8}
18		adds r8, r8, #4
19		blvs _errOverflow
20		push {r8}
21		pop {r8}
22		adds r8, r8, #5
23		blvs _errOverflow
24		push {r8}
25		pop {r8}
26		adds r8, r8, #6
27		blvs _errOverflow
28		push {r8}
29		pop {r8}
30		adds r8, r8, #7
31		blvs _errOverflow
32		push {r8}
33		pop {r8}
34		adds r8, r8, #8
35		blvs _errOverflow
36		push {r8}
37		pop {r8}
38		adds r8, r8, #9
39		blvs _errOverflow
40		push {r8}
41		pop {r8}
42		adds r8, r8, #10
43		blvs _errOverflow
44		push {r8}
45		pop {r8}
46		adds r8, r8, #11
47		blvs _errOverflow
48		push {r8}
49		pop {r8}
50		adds r8, r8, #12
51		blvs _errOverflow
52		push {r8}
53		pop {r8}
54		adds r8, r8, #13
55		blvs _errOverflow
56		push {r8}
57		pop {r8}
58		adds r8, r8, #14
59		blvs _errOverflow
60		push {r8}
61		pop {r8}
62		adds r8, r8, #15
63		blvs _errOverflow
64		push {r8}
65		pop {r8}
66		adds r8, r8, #16
67		blvs _errOverflow
68		push {r8}
69		pop {r8}
70		adds r8, r8, #17
71		blvs _errOverflow
72		push {r8}
73		pop {r8}
74		mov r8, r8
75		mov r4, r8
76		ldr r8, =-1
77		subs r8, r8, #2
78		blvs _errOverflow
79		push {r8}
80		pop {r8}
81		subs r8, r8, #3
82		blvs _errOverflow
83		push {r8}
84		pop {r8}
85		subs r8, r8, #4
86		blvs _errOverflow
87		push {r8}
88		pop {r8}
89		subs r8, r8, #5
90		blvs _errOverflow
91		push {r8}
92		pop {r8}
93		subs r8, r8, #6
94		blvs _errOverflow
95		push {r8}
96		pop {r8}
97		subs r8, r8, #7
98		blvs _errOverflow
99		push {r8}
100		pop {r8}
101		subs r8, r8, #8
102		blvs _errOverflow
103		push {r8}
104		pop {r8}
105		subs r8, r8, #9
106		blvs _errOverflow
107		push {r8}
108		pop {r8}
109		subs r8, r8, #10
110		blvs _errOverflow
111		push {r8}
112		pop {r8}
113		subs r8, r8, #11
114		blvs _errOverflow
115		push {r8}
116		pop {r8}
117		subs r8, r8, #12
118		blvs _errOverflow
119		push {r8}
120		pop {r8}
121		subs r8, r8, #13
122		blvs _errOverflow
123		push {r8}
124		pop {r8}
125		subs r8, r8, #14
126		blvs _errOverflow
127		push {r8}
128		pop {r8}
129		subs r8, r8, #15
130		blvs _errOverflow
131		push {r8}
132		pop {r8}
133		subs r8, r8, #16
134		blvs _errOverflow
135		push {r8}
136		pop {r8}
137		subs r8, r8, #17
138		blvs _errOverflow
139		push {r8}
140		pop {r8}
141		mov r8, r8
142		mov r5, r8
143		mov r9, #2
144		mov r8, #1
145		smull r8, r9, r8, r9
146		@ Compare the sign of the lowest bits of the multiplication against the highest
147		cmp r9, r8, asr #31
148		blne _errOverflow
149		push {r8}
150		mov r9, #3
151		pop {r8}
152		smull r8, r9, r8, r9
153		@ Compare the sign of the lowest bits of the multiplication against the highest
154		cmp r9, r8, asr #31
155		blne _errOverflow
156		push {r8}
157		mov r9, #4
158		pop {r8}
159		smull r8, r9, r8, r9
160		@ Compare the sign of the lowest bits of the multiplication against the highest
161		cmp r9, r8, asr #31
162		blne _errOverflow
163		push {r8}
164		mov r9, #5
165		pop {r8}
166		smull r8, r9, r8, r9
167		@ Compare the sign of the lowest bits of the multiplication against the highest
168		cmp r9, r8, asr #31
169		blne _errOverflow
170		push {r8}
171		mov r9, #6
172		pop {r8}
173		smull r8, r9, r8, r9
174		@ Compare the sign of the lowest bits of the multiplication against the highest
175		cmp r9, r8, asr #31
176		blne _errOverflow
177		push {r8}
178		mov r9, #7
179		pop {r8}
180		smull r8, r9, r8, r9
181		@ Compare the sign of the lowest bits of the multiplication against the highest
182		cmp r9, r8, asr #31
183		blne _errOverflow
184		push {r8}
185		mov r9, #8
186		pop {r8}
187		smull r8, r9, r8, r9
188		@ Compare the sign of the lowest bits of the multiplication against the highest
189		cmp r9, r8, asr #31
190		blne _errOverflow
191		push {r8}
192		mov r9, #9
193		pop {r8}
194		smull r8, r9, r8, r9
195		@ Compare the sign of the lowest bits of the multiplication against the highest
196		cmp r9, r8, asr #31
197		blne _errOverflow
198		push {r8}
199		mov r9, #10
200		pop {r8}
201		smull r8, r9, r8, r9
202		@ Compare the sign of the lowest bits of the multiplication against the highest
203		cmp r9, r8, asr #31
204		blne _errOverflow
205		push {r8}
206		pop {r8}
207		mov r8, r8
208		mov r6, r8
209		mov r8, #10
210		mov r7, r8
211		@ Stack pointer unchanged, no stack allocated arguments
212		adds r8, r4, r5
213		blvs _errOverflow
214		push {r8}
215		mov r0, r6
216		mov r1, r7
217		cmp r1, #0
218		bleq _errDivZero
219		bl __aeabi_idivmod
220		mov r12, r0
221		mov r8, r12
222		push {r8}
223		pop {r9}
224		pop {r8}
225		adds r8, r8, r9
226		blvs _errOverflow
227		push {r8}
228		pop {r8}
229		mov r8, r8
230		mov r0, r8
231		@ statement primitives do not return results (but will clobber r0/rax)
232		bl _printi
233		bl _println
234		@ Stack pointer unchanged, no stack allocated arguments
235		adds r8, r4, r5
236		blvs _errOverflow
237		push {r8}
238		mov r0, r6
239		mov r1, r7
240		cmp r1, #0
241		bleq _errDivZero
242		bl __aeabi_idivmod
243		mov r12, r0
244		mov r8, r12
245		push {r8}
246		pop {r9}
247		pop {r8}
248		adds r8, r8, r9
249		blvs _errOverflow
250		push {r8}
251		pop {r8}
252		mov r0, r8
253		mov r1, #256
254		cmp r1, #0
255		bleq _errDivZero
256		bl __aeabi_idivmod
257		mov r12, r1
258		mov r8, r12
259		push {r8}
260		pop {r8}
261		mov r8, r8
262		mov r0, r8
263		@ statement primitives do not return results (but will clobber r0/rax)
264		bl _printi
265		bl _println
266		@ Stack pointer unchanged, no stack allocated arguments
267		adds r8, r4, r5
268		blvs _errOverflow
269		push {r8}
270		mov r0, r6
271		mov r1, r7
272		cmp r1, #0
273		bleq _errDivZero
274		bl __aeabi_idivmod
275		mov r12, r0
276		mov r8, r12
277		push {r8}
278		pop {r9}
279		pop {r8}
280		adds r8, r8, r9
281		blvs _errOverflow
282		push {r8}
283		pop {r8}
284		mov r8, r8
285		mov r0, r8
286		@ statement primitives do not return results (but will clobber r0/rax)
287		bl _exit
288		@ Stack pointer unchanged, no stack allocated variables
289		mov r0, #0
290		pop {r4, r5, r6, r7, r8, r10, r12}
291		pop {fp, pc}
292	
293	@ length of .L._prints_str0
294		.word 4
295	.L._prints_str0:
296		.asciz "%.*s"
297	.align 4
298	_prints:
299		push {fp, lr}
300		mov fp, sp
301		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
302		bic sp, sp, #0x7
303		mov r2, r0
304		ldr r1, [r0, #-4]
305		adr r0, .L._prints_str0
306		bl printf
307		mov r0, #0
308		bl fflush
309		mov sp, fp
310		pop {fp, pc}
311	
312	@ length of .L._printi_str0
313		.word 2
314	.L._printi_str0:
315		.asciz "%d"
316	.align 4
317	_printi:
318		push {fp, lr}
319		mov fp, sp
320		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
321		bic sp, sp, #0x7
322		mov r1, r0
323		adr r0, .L._printi_str0
324		bl printf
325		mov r0, #0
326		bl fflush
327		mov sp, fp
328		pop {fp, pc}
329	
330	@ length of .L._println_str0
331		.word 0
332	.L._println_str0:
333		.asciz ""
334	.align 4
335	_println:
336		push {fp, lr}
337		mov fp, sp
338		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
339		bic sp, sp, #0x7
340		adr r0, .L._println_str0
341		bl puts
342		mov r0, #0
343		bl fflush
344		mov sp, fp
345		pop {fp, pc}
346	
347	_exit:
348		push {fp, lr}
349		mov fp, sp
350		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
351		bic sp, sp, #0x7
352		bl exit
353		mov sp, fp
354		pop {fp, pc}
355	
356	@ length of .L._errDivZero_str0
357		.word 40
358	.L._errDivZero_str0:
359		.asciz "fatal error: division or modulo by zero\n"
360	.align 4
361	_errDivZero:
362		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
363		bic sp, sp, #0x7
364		adr r0, .L._errDivZero_str0
365		bl _prints
366		mov r0, #255
367		bl _exit
368	
369	@ length of .L._errOverflow_str0
370		.word 52
371	.L._errOverflow_str0:
372		.asciz "fatal error: integer overflow or underflow occurred\n"
373	.align 4
374	_errOverflow:
375		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
376		bic sp, sp, #0x7
377		adr r0, .L._errOverflow_str0
378		bl _prints
379		mov r0, #255
380		bl _exit
===========================================================
-- Finished

