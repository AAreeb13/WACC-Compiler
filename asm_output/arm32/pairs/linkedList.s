./valid/pairs/linkedList.wacc
calling the reference compiler on ./valid/pairs/linkedList.wacc
-- Test: linkedList.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create and print a linked list using pairs

# Output:
# list = {1, 2, 4, 11}
#

# Program:

begin
  pair(int, pair) p = newpair(11, null) ;
    pair(int, pair) q = newpair(4, p) ;
    pair(int, pair) r = newpair(2, q) ;
    pair(int, pair) s = newpair(1, r) ;
    print "list = {" ;
    pair(int, pair) x = s ;
    pair(int, pair) y = snd x ;
    int f = 0;
    while y != null do
      f = fst x ;
      print f ;
      print ", " ;
      x = y ;
      y = snd x
    done ;
    f = fst x ;
    print f ;
    println "}"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
linkedList.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 8
3	.L.str0:
4		.asciz "list = {"
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
18		push {r4, r5, r6, r7, r8, r10, r12}
19		mov fp, sp
20		@ Stack pointer unchanged, no stack allocated variables
21		push {r0, r1, r2}
22		mov r0, #8
23		bl _malloc
24		mov r12, r0
25		pop {r0, r1, r2}
26		mov r8, #11
27		str r8, [r12, #0]
28		mov r8, #0
29		str r8, [r12, #4]
30		mov r8, r12
31		mov r4, r8
32		push {r0, r1, r2}
33		mov r0, #8
34		bl _malloc
35		mov r12, r0
36		pop {r0, r1, r2}
37		mov r8, #4
38		str r8, [r12, #0]
39		mov r8, r4
40		str r8, [r12, #4]
41		mov r8, r12
42		mov r5, r8
43		push {r0, r1, r2}
44		mov r0, #8
45		bl _malloc
46		mov r12, r0
47		pop {r0, r1, r2}
48		mov r8, #2
49		str r8, [r12, #0]
50		mov r8, r5
51		str r8, [r12, #4]
52		mov r8, r12
53		mov r6, r8
54		push {r0, r1, r2}
55		mov r0, #8
56		bl _malloc
57		mov r12, r0
58		pop {r0, r1, r2}
59		mov r8, #1
60		str r8, [r12, #0]
61		mov r8, r6
62		str r8, [r12, #4]
63		mov r8, r12
64		mov r7, r8
65		push {r0, r1, r2}
66		@ Set up R12 as a temporary second base pointer for the caller saved things
67		mov r12, sp
68		@ Stack pointer unchanged, no stack allocated arguments
69		ldr r8, =.L.str0
70		push {r8}
71		pop {r8}
72		mov r8, r8
73		mov r0, r8
74		@ statement primitives do not return results (but will clobber r0/rax)
75		bl _prints
76		pop {r0, r1, r2}
77		mov r8, r7
78		mov r0, r8
79		cmp r0, #0
80		bleq _errNull
81		ldr r8, [r0, #4]
82		mov r8, r8
83		mov r1, r8
84		mov r8, #0
85		mov r2, r8
86		b .L0
87	.L1:
88		cmp r0, #0
89		bleq _errNull
90		ldr r8, [r0, #0]
91		mov r8, r8
92		push {r8}
93		pop {r8}
94		mov r8, r8
95		mov r2, r8
96		push {r0, r1, r2}
97		@ Set up R12 as a temporary second base pointer for the caller saved things
98		mov r12, sp
99		@ Stack pointer unchanged, no stack allocated arguments
100		mov r8, r2
101		mov r0, r8
102		@ statement primitives do not return results (but will clobber r0/rax)
103		bl _printi
104		pop {r0, r1, r2}
105		push {r0, r1, r2}
106		@ Set up R12 as a temporary second base pointer for the caller saved things
107		mov r12, sp
108		@ Stack pointer unchanged, no stack allocated arguments
109		ldr r8, =.L.str1
110		push {r8}
111		pop {r8}
112		mov r8, r8
113		mov r0, r8
114		@ statement primitives do not return results (but will clobber r0/rax)
115		bl _prints
116		pop {r0, r1, r2}
117		mov r8, r1
118		mov r0, r8
119		cmp r0, #0
120		bleq _errNull
121		ldr r8, [r0, #4]
122		mov r8, r8
123		push {r8}
124		pop {r8}
125		mov r8, r8
126		mov r1, r8
127	.L0:
128		cmp r1, #0
129		bne .L1
130		cmp r0, #0
131		bleq _errNull
132		ldr r8, [r0, #0]
133		mov r8, r8
134		push {r8}
135		pop {r8}
136		mov r8, r8
137		mov r2, r8
138		push {r0, r1, r2}
139		@ Set up R12 as a temporary second base pointer for the caller saved things
140		mov r12, sp
141		@ Stack pointer unchanged, no stack allocated arguments
142		mov r8, r2
143		mov r0, r8
144		@ statement primitives do not return results (but will clobber r0/rax)
145		bl _printi
146		pop {r0, r1, r2}
147		push {r0, r1, r2}
148		@ Set up R12 as a temporary second base pointer for the caller saved things
149		mov r12, sp
150		@ Stack pointer unchanged, no stack allocated arguments
151		ldr r8, =.L.str2
152		push {r8}
153		pop {r8}
154		mov r8, r8
155		mov r0, r8
156		@ statement primitives do not return results (but will clobber r0/rax)
157		bl _prints
158		bl _println
159		pop {r0, r1, r2}
160		@ Stack pointer unchanged, no stack allocated variables
161		mov r0, #0
162		pop {r4, r5, r6, r7, r8, r10, r12}
163		pop {fp, pc}
164	
165	@ length of .L._prints_str0
166		.word 4
167	.L._prints_str0:
168		.asciz "%.*s"
169	.align 4
170	_prints:
171		push {fp, lr}
172		mov fp, sp
173		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
174		bic sp, sp, #0x7
175		mov r2, r0
176		ldr r1, [r0, #-4]
177		adr r0, .L._prints_str0
178		bl printf
179		mov r0, #0
180		bl fflush
181		mov sp, fp
182		pop {fp, pc}
183	
184	_malloc:
185		push {fp, lr}
186		mov fp, sp
187		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
188		bic sp, sp, #0x7
189		bl malloc
190		cmp r0, #0
191		bleq _errOutOfMemory
192		mov sp, fp
193		pop {fp, pc}
194	
195	@ length of .L._printi_str0
196		.word 2
197	.L._printi_str0:
198		.asciz "%d"
199	.align 4
200	_printi:
201		push {fp, lr}
202		mov fp, sp
203		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
204		bic sp, sp, #0x7
205		mov r1, r0
206		adr r0, .L._printi_str0
207		bl printf
208		mov r0, #0
209		bl fflush
210		mov sp, fp
211		pop {fp, pc}
212	
213	@ length of .L._println_str0
214		.word 0
215	.L._println_str0:
216		.asciz ""
217	.align 4
218	_println:
219		push {fp, lr}
220		mov fp, sp
221		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
222		bic sp, sp, #0x7
223		adr r0, .L._println_str0
224		bl puts
225		mov r0, #0
226		bl fflush
227		mov sp, fp
228		pop {fp, pc}
229	
230	_exit:
231		push {fp, lr}
232		mov fp, sp
233		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
234		bic sp, sp, #0x7
235		bl exit
236		mov sp, fp
237		pop {fp, pc}
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
252	@ length of .L._errNull_str0
253		.word 45
254	.L._errNull_str0:
255		.asciz "fatal error: null pair dereferenced or freed\n"
256	.align 4
257	_errNull:
258		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
259		bic sp, sp, #0x7
260		adr r0, .L._errNull_str0
261		bl _prints
262		mov r0, #255
263		bl _exit
===========================================================
-- Finished

