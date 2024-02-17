./valid/array/array.wacc
calling the reference compiler on ./valid/array/array.wacc
-- Test: array.wacc

-- Uploaded file: 
---------------------------------------------------------------
# moderate complexity array manipulations

# Output:
# #addrs# = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
#

# Program:

begin
  int[] a = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0] ;
  int i = 0 ;
  while i < 10
  do
    a[i] = i ;
    i = i + 1
  done ;
  print a ;
  print " = {" ;
  i = 0 ;
  while i < 10
  do
    print a[i] ;
    if i < 9
    then
      print ", "
    else
      skip
    fi ;
    i = i + 1
  done ;
  println "}"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
array.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 4
3	.L.str0:
4		.asciz " = {"
5	@ length of .L.str1
6		.word 2
7	.L.str1:
8		.asciz ", "
9	@ length of .L.str2
10		.word 1
11	.L.str2:
12		.asciz "}"
13	.align 4
14	.text
15	.global main
16	main:
17		push {fp, lr}
18		push {r4, r5, r8, r10, r12}
19		mov fp, sp
20		@ Stack pointer unchanged, no stack allocated variables
21		@ 10 element array
22		mov r0, #44
23		bl _malloc
24		mov r12, r0
25		@ array pointers are shifted forwards by 4 bytes (to account for size)
26		add r12, r12, #4
27		mov r8, #10
28		str r8, [r12, #-4]
29		mov r8, #0
30		str r8, [r12, #0]
31		mov r8, #0
32		str r8, [r12, #4]
33		mov r8, #0
34		str r8, [r12, #8]
35		mov r8, #0
36		str r8, [r12, #12]
37		mov r8, #0
38		str r8, [r12, #16]
39		mov r8, #0
40		str r8, [r12, #20]
41		mov r8, #0
42		str r8, [r12, #24]
43		mov r8, #0
44		str r8, [r12, #28]
45		mov r8, #0
46		str r8, [r12, #32]
47		mov r8, #0
48		str r8, [r12, #36]
49		mov r8, r12
50		mov r4, r8
51		mov r8, #0
52		mov r5, r8
53		b .L0
54	.L1:
55		mov r10, r5
56		mov r8, r5
57		mov r3, r4
58		bl _arrStore4
59		adds r8, r5, #1
60		blvs _errOverflow
61		push {r8}
62		pop {r8}
63		mov r8, r8
64		mov r5, r8
65	.L0:
66		cmp r5, #10
67		blt .L1
68		@ Stack pointer unchanged, no stack allocated arguments
69		mov r8, r4
70		mov r0, r8
71		@ statement primitives do not return results (but will clobber r0/rax)
72		bl _printp
73		@ Stack pointer unchanged, no stack allocated arguments
74		ldr r8, =.L.str0
75		push {r8}
76		pop {r8}
77		mov r8, r8
78		mov r0, r8
79		@ statement primitives do not return results (but will clobber r0/rax)
80		bl _prints
81		mov r8, #0
82		mov r5, r8
83		b .L2
84	.L3:
85		@ Stack pointer unchanged, no stack allocated arguments
86		mov r10, r5
87		mov r3, r4
88		bl _arrLoad4
89		mov r8, r3
90		mov r8, r8
91		mov r0, r8
92		@ statement primitives do not return results (but will clobber r0/rax)
93		bl _printi
94		cmp r5, #9
95		blt .L4
96		b .L5
97	.L4:
98		@ Stack pointer unchanged, no stack allocated arguments
99		ldr r8, =.L.str1
100		push {r8}
101		pop {r8}
102		mov r8, r8
103		mov r0, r8
104		@ statement primitives do not return results (but will clobber r0/rax)
105		bl _prints
106	.L5:
107		adds r8, r5, #1
108		blvs _errOverflow
109		push {r8}
110		pop {r8}
111		mov r8, r8
112		mov r5, r8
113	.L2:
114		cmp r5, #10
115		blt .L3
116		@ Stack pointer unchanged, no stack allocated arguments
117		ldr r8, =.L.str2
118		push {r8}
119		pop {r8}
120		mov r8, r8
121		mov r0, r8
122		@ statement primitives do not return results (but will clobber r0/rax)
123		bl _prints
124		bl _println
125		@ Stack pointer unchanged, no stack allocated variables
126		mov r0, #0
127		pop {r4, r5, r8, r10, r12}
128		pop {fp, pc}
129	
130	@ length of .L._prints_str0
131		.word 4
132	.L._prints_str0:
133		.asciz "%.*s"
134	.align 4
135	_prints:
136		push {fp, lr}
137		mov fp, sp
138		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
139		bic sp, sp, #0x7
140		mov r2, r0
141		ldr r1, [r0, #-4]
142		adr r0, .L._prints_str0
143		bl printf
144		mov r0, #0
145		bl fflush
146		mov sp, fp
147		pop {fp, pc}
148	
149	@ length of .L._printp_str0
150		.word 2
151	.L._printp_str0:
152		.asciz "%p"
153	.align 4
154	_printp:
155		push {fp, lr}
156		mov fp, sp
157		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
158		bic sp, sp, #0x7
159		mov r1, r0
160		adr r0, .L._printp_str0
161		bl printf
162		mov r0, #0
163		bl fflush
164		mov sp, fp
165		pop {fp, pc}
166	
167	_malloc:
168		push {fp, lr}
169		mov fp, sp
170		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
171		bic sp, sp, #0x7
172		bl malloc
173		cmp r0, #0
174		bleq _errOutOfMemory
175		mov sp, fp
176		pop {fp, pc}
177	
178	@ length of .L._printi_str0
179		.word 2
180	.L._printi_str0:
181		.asciz "%d"
182	.align 4
183	_printi:
184		push {fp, lr}
185		mov fp, sp
186		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
187		bic sp, sp, #0x7
188		mov r1, r0
189		adr r0, .L._printi_str0
190		bl printf
191		mov r0, #0
192		bl fflush
193		mov sp, fp
194		pop {fp, pc}
195	
196	@ length of .L._println_str0
197		.word 0
198	.L._println_str0:
199		.asciz ""
200	.align 4
201	_println:
202		push {fp, lr}
203		mov fp, sp
204		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
205		bic sp, sp, #0x7
206		adr r0, .L._println_str0
207		bl puts
208		mov r0, #0
209		bl fflush
210		mov sp, fp
211		pop {fp, pc}
212	
213	_exit:
214		push {fp, lr}
215		mov fp, sp
216		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
217		bic sp, sp, #0x7
218		bl exit
219		mov sp, fp
220		pop {fp, pc}
221	
222	_arrStore4:
223		@ Special calling convention: array ptr passed in R3, index in R10, value to store in R8, LR (R14) is used as general register
224		push {lr}
225		cmp r10, #0
226		movlt r1, r10
227		bllt _errOutOfBounds
228		ldr lr, [r3, #-4]
229		cmp r10, lr
230		movge r1, r10
231		blge _errOutOfBounds
232		str r8, [r3, r10, lsl #2]
233		pop {pc}
234	
235	_arrLoad4:
236		@ Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
237		push {lr}
238		cmp r10, #0
239		movlt r1, r10
240		bllt _errOutOfBounds
241		ldr lr, [r3, #-4]
242		cmp r10, lr
243		movge r1, r10
244		blge _errOutOfBounds
245		ldr r3, [r3, r10, lsl #2]
246		pop {pc}
247	
248	@ length of .L._errOutOfMemory_str0
249		.word 27
250	.L._errOutOfMemory_str0:
251		.asciz "fatal error: out of memory\n"
252	.align 4
253	_errOutOfMemory:
254		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
255		bic sp, sp, #0x7
256		adr r0, .L._errOutOfMemory_str0
257		bl _prints
258		mov r0, #255
259		bl _exit
260	
261	@ length of .L._errOutOfBounds_str0
262		.word 42
263	.L._errOutOfBounds_str0:
264		.asciz "fatal error: array index %d out of bounds\n"
265	.align 4
266	_errOutOfBounds:
267		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
268		bic sp, sp, #0x7
269		adr r0, .L._errOutOfBounds_str0
270		bl printf
271		mov r0, #0
272		bl fflush
273		mov r0, #255
274		bl _exit
275	
276	@ length of .L._errOverflow_str0
277		.word 52
278	.L._errOverflow_str0:
279		.asciz "fatal error: integer overflow or underflow occurred\n"
280	.align 4
281	_errOverflow:
282		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
283		bic sp, sp, #0x7
284		adr r0, .L._errOverflow_str0
285		bl _prints
286		mov r0, #255
287		bl _exit
===========================================================
-- Finished

