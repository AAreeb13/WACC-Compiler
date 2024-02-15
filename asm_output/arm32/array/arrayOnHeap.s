./valid/array/arrayOnHeap.wacc
calling the reference compiler on ./valid/array/arrayOnHeap.wacc
-- Test: arrayOnHeap.wacc

-- Uploaded file: 
---------------------------------------------------------------
# ensures that arrays are heap allocated

# Output:
# false
# 0
# 1
#

# Program:

begin
  int[] dummy_arr = [] ;
  int[][] arrs = [dummy_arr, dummy_arr] ;
  int i = 0;
  while i < 2 do
    int[] arr = [i] ;
    arrs[i] = arr ;
    i = i + 1
  done;

  # should have different addresses
  println arrs[0] == arrs[1];
  # should have different values
  println arrs[0][0];
  println arrs[1][0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayOnHeap.s contents are:
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
9		@ 0 element array
10		mov r0, #4
11		bl _malloc
12		mov r12, r0
13		@ array pointers are shifted forwards by 4 bytes (to account for size)
14		add r12, r12, #4
15		mov r8, #0
16		str r8, [r12, #-4]
17		mov r8, r12
18		mov r4, r8
19		@ 2 element array
20		mov r0, #12
21		bl _malloc
22		mov r12, r0
23		@ array pointers are shifted forwards by 4 bytes (to account for size)
24		add r12, r12, #4
25		mov r8, #2
26		str r8, [r12, #-4]
27		mov r8, r4
28		str r8, [r12, #0]
29		mov r8, r4
30		str r8, [r12, #4]
31		mov r8, r12
32		mov r5, r8
33		mov r8, #0
34		mov r6, r8
35		b .L0
36	.L1:
37		@ Stack pointer unchanged, no stack allocated variables
38		@ 1 element array
39		mov r0, #8
40		bl _malloc
41		mov r12, r0
42		@ array pointers are shifted forwards by 4 bytes (to account for size)
43		add r12, r12, #4
44		mov r8, #1
45		str r8, [r12, #-4]
46		mov r8, r6
47		str r8, [r12, #0]
48		mov r8, r12
49		mov r7, r8
50		mov r10, r6
51		mov r8, r7
52		mov r3, r5
53		bl _arrStore4
54		adds r8, r6, #1
55		blvs _errOverflow
56		push {r8}
57		pop {r8}
58		mov r8, r8
59		mov r6, r8
60		@ Stack pointer unchanged, no stack allocated variables
61	.L0:
62		cmp r6, #2
63		blt .L1
64		@ Stack pointer unchanged, no stack allocated arguments
65		mov r10, #1
66		mov r3, r5
67		bl _arrLoad4
68		mov r9, r3
69		mov r10, #0
70		mov r3, r5
71		bl _arrLoad4
72		mov r8, r3
73		cmp r8, r9
74		moveq r8, #1
75		movne r8, #0
76		push {r8}
77		pop {r8}
78		mov r8, r8
79		mov r0, r8
80		@ statement primitives do not return results (but will clobber r0/rax)
81		bl _printb
82		bl _println
83		@ Stack pointer unchanged, no stack allocated arguments
84		mov r10, #0
85		mov r3, r5
86		bl _arrLoad4
87		mov r8, r3
88		mov r8, r8
89		push {r8}
90		mov r10, #0
91		pop {r8}
92		mov r3, r8
93		bl _arrLoad4
94		mov r8, r3
95		mov r8, r8
96		mov r0, r8
97		@ statement primitives do not return results (but will clobber r0/rax)
98		bl _printi
99		bl _println
100		@ Stack pointer unchanged, no stack allocated arguments
101		mov r10, #1
102		mov r3, r5
103		bl _arrLoad4
104		mov r8, r3
105		mov r8, r8
106		push {r8}
107		mov r10, #0
108		pop {r8}
109		mov r3, r8
110		bl _arrLoad4
111		mov r8, r3
112		mov r8, r8
113		mov r0, r8
114		@ statement primitives do not return results (but will clobber r0/rax)
115		bl _printi
116		bl _println
117		@ Stack pointer unchanged, no stack allocated variables
118		mov r0, #0
119		pop {r4, r5, r6, r7, r8, r10, r12}
120		pop {fp, pc}
121	
122	@ length of .L._prints_str0
123		.word 4
124	.L._prints_str0:
125		.asciz "%.*s"
126	.align 4
127	_prints:
128		push {fp, lr}
129		mov fp, sp
130		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
131		bic sp, sp, #0x7
132		mov r2, r0
133		ldr r1, [r0, #-4]
134		adr r0, .L._prints_str0
135		bl printf
136		mov r0, #0
137		bl fflush
138		mov sp, fp
139		pop {fp, pc}
140	
141	_malloc:
142		push {fp, lr}
143		mov fp, sp
144		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
145		bic sp, sp, #0x7
146		bl malloc
147		cmp r0, #0
148		bleq _errOutOfMemory
149		mov sp, fp
150		pop {fp, pc}
151	
152	@ length of .L._printi_str0
153		.word 2
154	.L._printi_str0:
155		.asciz "%d"
156	.align 4
157	_printi:
158		push {fp, lr}
159		mov fp, sp
160		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
161		bic sp, sp, #0x7
162		mov r1, r0
163		adr r0, .L._printi_str0
164		bl printf
165		mov r0, #0
166		bl fflush
167		mov sp, fp
168		pop {fp, pc}
169	
170	@ length of .L._printb_str0
171		.word 5
172	.L._printb_str0:
173		.asciz "false"
174	@ length of .L._printb_str1
175		.word 4
176	.L._printb_str1:
177		.asciz "true"
178	@ length of .L._printb_str2
179		.word 4
180	.L._printb_str2:
181		.asciz "%.*s"
182	.align 4
183	_printb:
184		push {fp, lr}
185		mov fp, sp
186		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
187		bic sp, sp, #0x7
188		cmp r0, #0
189		bne .L_printb0
190		adr r2, .L._printb_str0
191		b .L_printb1
192	.L_printb0:
193		adr r2, .L._printb_str1
194	.L_printb1:
195		ldr r1, [r2, #-4]
196		adr r0, .L._printb_str2
197		bl printf
198		mov r0, #0
199		bl fflush
200		mov sp, fp
201		pop {fp, pc}
202	
203	@ length of .L._println_str0
204		.word 0
205	.L._println_str0:
206		.asciz ""
207	.align 4
208	_println:
209		push {fp, lr}
210		mov fp, sp
211		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
212		bic sp, sp, #0x7
213		adr r0, .L._println_str0
214		bl puts
215		mov r0, #0
216		bl fflush
217		mov sp, fp
218		pop {fp, pc}
219	
220	_exit:
221		push {fp, lr}
222		mov fp, sp
223		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
224		bic sp, sp, #0x7
225		bl exit
226		mov sp, fp
227		pop {fp, pc}
228	
229	_arrStore4:
230		@ Special calling convention: array ptr passed in R3, index in R10, value to store in R8, LR (R14) is used as general register
231		push {lr}
232		cmp r10, #0
233		movlt r1, r10
234		bllt _errOutOfBounds
235		ldr lr, [r3, #-4]
236		cmp r10, lr
237		movge r1, r10
238		blge _errOutOfBounds
239		str r8, [r3, r10, lsl #2]
240		pop {pc}
241	
242	_arrLoad4:
243		@ Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
244		push {lr}
245		cmp r10, #0
246		movlt r1, r10
247		bllt _errOutOfBounds
248		ldr lr, [r3, #-4]
249		cmp r10, lr
250		movge r1, r10
251		blge _errOutOfBounds
252		ldr r3, [r3, r10, lsl #2]
253		pop {pc}
254	
255	@ length of .L._errOutOfMemory_str0
256		.word 27
257	.L._errOutOfMemory_str0:
258		.asciz "fatal error: out of memory\n"
259	.align 4
260	_errOutOfMemory:
261		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
262		bic sp, sp, #0x7
263		adr r0, .L._errOutOfMemory_str0
264		bl _prints
265		mov r0, #255
266		bl _exit
267	
268	@ length of .L._errOutOfBounds_str0
269		.word 42
270	.L._errOutOfBounds_str0:
271		.asciz "fatal error: array index %d out of bounds\n"
272	.align 4
273	_errOutOfBounds:
274		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
275		bic sp, sp, #0x7
276		adr r0, .L._errOutOfBounds_str0
277		bl printf
278		mov r0, #0
279		bl fflush
280		mov r0, #255
281		bl _exit
282	
283	@ length of .L._errOverflow_str0
284		.word 52
285	.L._errOverflow_str0:
286		.asciz "fatal error: integer overflow or underflow occurred\n"
287	.align 4
288	_errOverflow:
289		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
290		bic sp, sp, #0x7
291		adr r0, .L._errOverflow_str0
292		bl _prints
293		mov r0, #255
294		bl _exit
===========================================================
-- Finished

