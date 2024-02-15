./valid/pairs/checkRefPair.wacc
calling the reference compiler on ./valid/pairs/checkRefPair.wacc
-- Test: checkRefPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair(int, char) with a second reference to it and check it works

# Output:
# #addrs#
# #addrs#
# true
# 10
# 10
# true
# a
# a
# true
#

# Program:

begin
  pair(int, char) p = newpair(10, 'a') ;
  pair(int, char) q = p;
  println p ;
  println q ;
  println p == q ;
  int x = fst p ;
  int y = fst q ;
  println x ;
  println y ;
  println x == y ;
  char c1 = snd p ;
  char c2 = snd q ;
  println c1 ;
  println c2 ;
  println c1 == c2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
checkRefPair.s contents are:
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
9		push {r0, r1}
10		mov r0, #8
11		bl _malloc
12		mov r12, r0
13		pop {r0, r1}
14		mov r8, #10
15		str r8, [r12, #0]
16		mov r8, #97
17		str r8, [r12, #4]
18		mov r8, r12
19		mov r4, r8
20		mov r8, r4
21		mov r5, r8
22		push {r0, r1}
23		@ Set up R12 as a temporary second base pointer for the caller saved things
24		mov r12, sp
25		@ Stack pointer unchanged, no stack allocated arguments
26		mov r8, r4
27		mov r0, r8
28		@ statement primitives do not return results (but will clobber r0/rax)
29		bl _printp
30		bl _println
31		pop {r0, r1}
32		push {r0, r1}
33		@ Set up R12 as a temporary second base pointer for the caller saved things
34		mov r12, sp
35		@ Stack pointer unchanged, no stack allocated arguments
36		mov r8, r5
37		mov r0, r8
38		@ statement primitives do not return results (but will clobber r0/rax)
39		bl _printp
40		bl _println
41		pop {r0, r1}
42		push {r0, r1}
43		@ Set up R12 as a temporary second base pointer for the caller saved things
44		mov r12, sp
45		@ Stack pointer unchanged, no stack allocated arguments
46		cmp r4, r5
47		moveq r8, #1
48		movne r8, #0
49		push {r8}
50		pop {r8}
51		mov r8, r8
52		mov r0, r8
53		@ statement primitives do not return results (but will clobber r0/rax)
54		bl _printb
55		bl _println
56		pop {r0, r1}
57		cmp r4, #0
58		bleq _errNull
59		ldr r8, [r4, #0]
60		mov r8, r8
61		mov r6, r8
62		cmp r5, #0
63		bleq _errNull
64		ldr r8, [r5, #0]
65		mov r8, r8
66		mov r7, r8
67		push {r0, r1}
68		@ Set up R12 as a temporary second base pointer for the caller saved things
69		mov r12, sp
70		@ Stack pointer unchanged, no stack allocated arguments
71		mov r8, r6
72		mov r0, r8
73		@ statement primitives do not return results (but will clobber r0/rax)
74		bl _printi
75		bl _println
76		pop {r0, r1}
77		push {r0, r1}
78		@ Set up R12 as a temporary second base pointer for the caller saved things
79		mov r12, sp
80		@ Stack pointer unchanged, no stack allocated arguments
81		mov r8, r7
82		mov r0, r8
83		@ statement primitives do not return results (but will clobber r0/rax)
84		bl _printi
85		bl _println
86		pop {r0, r1}
87		push {r0, r1}
88		@ Set up R12 as a temporary second base pointer for the caller saved things
89		mov r12, sp
90		@ Stack pointer unchanged, no stack allocated arguments
91		cmp r6, r7
92		moveq r8, #1
93		movne r8, #0
94		push {r8}
95		pop {r8}
96		mov r8, r8
97		mov r0, r8
98		@ statement primitives do not return results (but will clobber r0/rax)
99		bl _printb
100		bl _println
101		pop {r0, r1}
102		cmp r4, #0
103		bleq _errNull
104		ldr r8, [r4, #4]
105		mov r8, r8
106		mov r0, r8
107		cmp r5, #0
108		bleq _errNull
109		ldr r8, [r5, #4]
110		mov r8, r8
111		mov r1, r8
112		push {r0, r1}
113		@ Set up R12 as a temporary second base pointer for the caller saved things
114		mov r12, sp
115		@ Stack pointer unchanged, no stack allocated arguments
116		mov r8, r0
117		mov r0, r8
118		@ statement primitives do not return results (but will clobber r0/rax)
119		bl _printc
120		bl _println
121		pop {r0, r1}
122		push {r0, r1}
123		@ Set up R12 as a temporary second base pointer for the caller saved things
124		mov r12, sp
125		@ Stack pointer unchanged, no stack allocated arguments
126		mov r8, r1
127		mov r0, r8
128		@ statement primitives do not return results (but will clobber r0/rax)
129		bl _printc
130		bl _println
131		pop {r0, r1}
132		push {r0, r1}
133		@ Set up R12 as a temporary second base pointer for the caller saved things
134		mov r12, sp
135		@ Stack pointer unchanged, no stack allocated arguments
136		cmp r0, r1
137		moveq r8, #1
138		movne r8, #0
139		push {r8}
140		pop {r8}
141		mov r8, r8
142		mov r0, r8
143		@ statement primitives do not return results (but will clobber r0/rax)
144		bl _printb
145		bl _println
146		pop {r0, r1}
147		@ Stack pointer unchanged, no stack allocated variables
148		mov r0, #0
149		pop {r4, r5, r6, r7, r8, r10, r12}
150		pop {fp, pc}
151	
152	@ length of .L._prints_str0
153		.word 4
154	.L._prints_str0:
155		.asciz "%.*s"
156	.align 4
157	_prints:
158		push {fp, lr}
159		mov fp, sp
160		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
161		bic sp, sp, #0x7
162		mov r2, r0
163		ldr r1, [r0, #-4]
164		adr r0, .L._prints_str0
165		bl printf
166		mov r0, #0
167		bl fflush
168		mov sp, fp
169		pop {fp, pc}
170	
171	@ length of .L._printp_str0
172		.word 2
173	.L._printp_str0:
174		.asciz "%p"
175	.align 4
176	_printp:
177		push {fp, lr}
178		mov fp, sp
179		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
180		bic sp, sp, #0x7
181		mov r1, r0
182		adr r0, .L._printp_str0
183		bl printf
184		mov r0, #0
185		bl fflush
186		mov sp, fp
187		pop {fp, pc}
188	
189	_malloc:
190		push {fp, lr}
191		mov fp, sp
192		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
193		bic sp, sp, #0x7
194		bl malloc
195		cmp r0, #0
196		bleq _errOutOfMemory
197		mov sp, fp
198		pop {fp, pc}
199	
200	@ length of .L._printi_str0
201		.word 2
202	.L._printi_str0:
203		.asciz "%d"
204	.align 4
205	_printi:
206		push {fp, lr}
207		mov fp, sp
208		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
209		bic sp, sp, #0x7
210		mov r1, r0
211		adr r0, .L._printi_str0
212		bl printf
213		mov r0, #0
214		bl fflush
215		mov sp, fp
216		pop {fp, pc}
217	
218	@ length of .L._printb_str0
219		.word 5
220	.L._printb_str0:
221		.asciz "false"
222	@ length of .L._printb_str1
223		.word 4
224	.L._printb_str1:
225		.asciz "true"
226	@ length of .L._printb_str2
227		.word 4
228	.L._printb_str2:
229		.asciz "%.*s"
230	.align 4
231	_printb:
232		push {fp, lr}
233		mov fp, sp
234		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
235		bic sp, sp, #0x7
236		cmp r0, #0
237		bne .L_printb0
238		adr r2, .L._printb_str0
239		b .L_printb1
240	.L_printb0:
241		adr r2, .L._printb_str1
242	.L_printb1:
243		ldr r1, [r2, #-4]
244		adr r0, .L._printb_str2
245		bl printf
246		mov r0, #0
247		bl fflush
248		mov sp, fp
249		pop {fp, pc}
250	
251	@ length of .L._printc_str0
252		.word 2
253	.L._printc_str0:
254		.asciz "%c"
255	.align 4
256	_printc:
257		push {fp, lr}
258		mov fp, sp
259		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
260		bic sp, sp, #0x7
261		mov r1, r0
262		adr r0, .L._printc_str0
263		bl printf
264		mov r0, #0
265		bl fflush
266		mov sp, fp
267		pop {fp, pc}
268	
269	@ length of .L._println_str0
270		.word 0
271	.L._println_str0:
272		.asciz ""
273	.align 4
274	_println:
275		push {fp, lr}
276		mov fp, sp
277		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
278		bic sp, sp, #0x7
279		adr r0, .L._println_str0
280		bl puts
281		mov r0, #0
282		bl fflush
283		mov sp, fp
284		pop {fp, pc}
285	
286	_exit:
287		push {fp, lr}
288		mov fp, sp
289		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
290		bic sp, sp, #0x7
291		bl exit
292		mov sp, fp
293		pop {fp, pc}
294	
295	@ length of .L._errOutOfMemory_str0
296		.word 27
297	.L._errOutOfMemory_str0:
298		.asciz "fatal error: out of memory\n"
299	.align 4
300	_errOutOfMemory:
301		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
302		bic sp, sp, #0x7
303		adr r0, .L._errOutOfMemory_str0
304		bl _prints
305		mov r0, #255
306		bl _exit
307	
308	@ length of .L._errNull_str0
309		.word 45
310	.L._errNull_str0:
311		.asciz "fatal error: null pair dereferenced or freed\n"
312	.align 4
313	_errNull:
314		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
315		bic sp, sp, #0x7
316		adr r0, .L._errNull_str0
317		bl _prints
318		mov r0, #255
319		bl _exit
===========================================================
-- Finished

