./valid/array/arrayIndexMayBeArrayIndex.wacc
calling the reference compiler on ./valid/array/arrayIndexMayBeArrayIndex.wacc
-- Test: arrayIndexMayBeArrayIndex.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Testing recursive array indexing

# Output:
# 6
# 7
# 8
#

# Program:

begin
    int[] idxs1 = [2, 0, 1] ;
    int[] idxs2 = [1, 2, 0] ;
    # idxs1[idxs2[0]] = 0
    # idxs1[idxs2[1]] = 1
    # idxs1[idxs2[2]] = 2
    int[] xs = [5, 6, 7] ;
    int i = 0 ;
    while i != 3 do
        xs[idxs1[idxs2[i]]] = xs[idxs1[idxs2[i]]] + 1 ;
        println (xs[idxs1[idxs2[i]]]) ;
        i = i + 1
    done
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayIndexMayBeArrayIndex.s contents are:
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
9		@ 3 element array
10		mov r0, #16
11		bl _malloc
12		mov r12, r0
13		@ array pointers are shifted forwards by 4 bytes (to account for size)
14		add r12, r12, #4
15		mov r8, #3
16		str r8, [r12, #-4]
17		mov r8, #2
18		str r8, [r12, #0]
19		mov r8, #0
20		str r8, [r12, #4]
21		mov r8, #1
22		str r8, [r12, #8]
23		mov r8, r12
24		mov r4, r8
25		@ 3 element array
26		mov r0, #16
27		bl _malloc
28		mov r12, r0
29		@ array pointers are shifted forwards by 4 bytes (to account for size)
30		add r12, r12, #4
31		mov r8, #3
32		str r8, [r12, #-4]
33		mov r8, #1
34		str r8, [r12, #0]
35		mov r8, #2
36		str r8, [r12, #4]
37		mov r8, #0
38		str r8, [r12, #8]
39		mov r8, r12
40		mov r5, r8
41		@ 3 element array
42		mov r0, #16
43		bl _malloc
44		mov r12, r0
45		@ array pointers are shifted forwards by 4 bytes (to account for size)
46		add r12, r12, #4
47		mov r8, #3
48		str r8, [r12, #-4]
49		mov r8, #5
50		str r8, [r12, #0]
51		mov r8, #6
52		str r8, [r12, #4]
53		mov r8, #7
54		str r8, [r12, #8]
55		mov r8, r12
56		mov r6, r8
57		mov r8, #0
58		mov r7, r8
59		b .L0
60	.L1:
61		mov r10, r7
62		mov r3, r5
63		bl _arrLoad4
64		mov r8, r3
65		mov r8, r8
66		push {r8}
67		pop {r10}
68		mov r10, r10
69		mov r3, r4
70		bl _arrLoad4
71		mov r8, r3
72		mov r8, r8
73		push {r8}
74		pop {r10}
75		mov r10, r10
76		mov r3, r6
77		bl _arrLoad4
78		mov r8, r3
79		adds r8, r8, #1
80		blvs _errOverflow
81		push {r8}
82		mov r10, r7
83		mov r3, r5
84		bl _arrLoad4
85		mov r8, r3
86		mov r8, r8
87		push {r8}
88		pop {r10}
89		mov r10, r10
90		mov r3, r4
91		bl _arrLoad4
92		mov r8, r3
93		mov r8, r8
94		push {r8}
95		pop {r10}
96		mov r10, r10
97		pop {r8}
98		mov r8, r8
99		mov r3, r6
100		bl _arrStore4
101		@ Stack pointer unchanged, no stack allocated arguments
102		mov r10, r7
103		mov r3, r5
104		bl _arrLoad4
105		mov r8, r3
106		mov r8, r8
107		push {r8}
108		pop {r10}
109		mov r10, r10
110		mov r3, r4
111		bl _arrLoad4
112		mov r8, r3
113		mov r8, r8
114		push {r8}
115		pop {r10}
116		mov r10, r10
117		mov r3, r6
118		bl _arrLoad4
119		mov r8, r3
120		mov r8, r8
121		mov r0, r8
122		@ statement primitives do not return results (but will clobber r0/rax)
123		bl _printi
124		bl _println
125		adds r8, r7, #1
126		blvs _errOverflow
127		push {r8}
128		pop {r8}
129		mov r8, r8
130		mov r7, r8
131	.L0:
132		cmp r7, #3
133		bne .L1
134		@ Stack pointer unchanged, no stack allocated variables
135		mov r0, #0
136		pop {r4, r5, r6, r7, r8, r10, r12}
137		pop {fp, pc}
138	
139	@ length of .L._prints_str0
140		.word 4
141	.L._prints_str0:
142		.asciz "%.*s"
143	.align 4
144	_prints:
145		push {fp, lr}
146		mov fp, sp
147		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
148		bic sp, sp, #0x7
149		mov r2, r0
150		ldr r1, [r0, #-4]
151		adr r0, .L._prints_str0
152		bl printf
153		mov r0, #0
154		bl fflush
155		mov sp, fp
156		pop {fp, pc}
157	
158	_malloc:
159		push {fp, lr}
160		mov fp, sp
161		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
162		bic sp, sp, #0x7
163		bl malloc
164		cmp r0, #0
165		bleq _errOutOfMemory
166		mov sp, fp
167		pop {fp, pc}
168	
169	@ length of .L._printi_str0
170		.word 2
171	.L._printi_str0:
172		.asciz "%d"
173	.align 4
174	_printi:
175		push {fp, lr}
176		mov fp, sp
177		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
178		bic sp, sp, #0x7
179		mov r1, r0
180		adr r0, .L._printi_str0
181		bl printf
182		mov r0, #0
183		bl fflush
184		mov sp, fp
185		pop {fp, pc}
186	
187	@ length of .L._println_str0
188		.word 0
189	.L._println_str0:
190		.asciz ""
191	.align 4
192	_println:
193		push {fp, lr}
194		mov fp, sp
195		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
196		bic sp, sp, #0x7
197		adr r0, .L._println_str0
198		bl puts
199		mov r0, #0
200		bl fflush
201		mov sp, fp
202		pop {fp, pc}
203	
204	_exit:
205		push {fp, lr}
206		mov fp, sp
207		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
208		bic sp, sp, #0x7
209		bl exit
210		mov sp, fp
211		pop {fp, pc}
212	
213	_arrStore4:
214		@ Special calling convention: array ptr passed in R3, index in R10, value to store in R8, LR (R14) is used as general register
215		push {lr}
216		cmp r10, #0
217		movlt r1, r10
218		bllt _errOutOfBounds
219		ldr lr, [r3, #-4]
220		cmp r10, lr
221		movge r1, r10
222		blge _errOutOfBounds
223		str r8, [r3, r10, lsl #2]
224		pop {pc}
225	
226	_arrLoad4:
227		@ Special calling convention: array ptr passed in R3, index in R10, LR (R14) is used as general register, and return into R3
228		push {lr}
229		cmp r10, #0
230		movlt r1, r10
231		bllt _errOutOfBounds
232		ldr lr, [r3, #-4]
233		cmp r10, lr
234		movge r1, r10
235		blge _errOutOfBounds
236		ldr r3, [r3, r10, lsl #2]
237		pop {pc}
238	
239	@ length of .L._errOutOfMemory_str0
240		.word 27
241	.L._errOutOfMemory_str0:
242		.asciz "fatal error: out of memory\n"
243	.align 4
244	_errOutOfMemory:
245		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
246		bic sp, sp, #0x7
247		adr r0, .L._errOutOfMemory_str0
248		bl _prints
249		mov r0, #255
250		bl _exit
251	
252	@ length of .L._errOutOfBounds_str0
253		.word 42
254	.L._errOutOfBounds_str0:
255		.asciz "fatal error: array index %d out of bounds\n"
256	.align 4
257	_errOutOfBounds:
258		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
259		bic sp, sp, #0x7
260		adr r0, .L._errOutOfBounds_str0
261		bl printf
262		mov r0, #0
263		bl fflush
264		mov r0, #255
265		bl _exit
266	
267	@ length of .L._errOverflow_str0
268		.word 52
269	.L._errOverflow_str0:
270		.asciz "fatal error: integer overflow or underflow occurred\n"
271	.align 4
272	_errOverflow:
273		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
274		bic sp, sp, #0x7
275		adr r0, .L._errOverflow_str0
276		bl _prints
277		mov r0, #255
278		bl _exit
===========================================================
-- Finished

