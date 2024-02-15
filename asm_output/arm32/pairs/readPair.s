./valid/pairs/readPair.wacc
calling the reference compiler on ./valid/pairs/readPair.wacc
-- Test: readPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# construct a pair from supplied user input

# Input: f 16

# Output:
# Please enter the first element (char): Please enter the second element (int): The first element was f
# The second element was 16
#

# Program:

begin
	pair(char, int) p = newpair('\0', 0) ;
	print "Please enter the first element (char): " ;
  	char c = '0';
	read c ;
  	fst p = c ;
	print "Please enter the second element (int): " ;
	int i = 0 ;
	read i ;
	snd p = i ;
	# Clear the value for c and i
	c = '\0' ;
	i = -1 ;
	print "The first element was " ;
	c = fst p ;
	println c ;
	print "The second element was " ;
	i = snd p ;
	println i
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
readPair.s contents are:
===========================================================
0	.data
1	@ length of .L.str0
2		.word 39
3	.L.str0:
4		.asciz "Please enter the first element (char): "
5	@ length of .L.str1
6		.word 39
7	.L.str1:
8		.asciz "Please enter the second element (int): "
9	@ length of .L.str2
10		.word 22
11	.L.str2:
12		.asciz "The first element was "
13	@ length of .L.str3
14		.word 23
15	.L.str3:
16		.asciz "The second element was "
17	.align 4
18	.text
19	.global main
20	main:
21		push {fp, lr}
22		push {r4, r5, r6, r8, r10, r12}
23		mov fp, sp
24		@ Stack pointer unchanged, no stack allocated variables
25		mov r0, #8
26		bl _malloc
27		mov r12, r0
28		mov r8, #0
29		str r8, [r12, #0]
30		mov r8, #0
31		str r8, [r12, #4]
32		mov r8, r12
33		mov r4, r8
34		@ Stack pointer unchanged, no stack allocated arguments
35		ldr r8, =.L.str0
36		push {r8}
37		pop {r8}
38		mov r8, r8
39		mov r0, r8
40		@ statement primitives do not return results (but will clobber r0/rax)
41		bl _prints
42		mov r8, #48
43		mov r5, r8
44		@ Stack pointer unchanged, no stack allocated arguments
45		@ load the current value in the destination of the read so it supports defaults
46		mov r8, r5
47		mov r0, r8
48		bl _readc
49		mov r12, r0
50		mov r8, r12
51		mov r5, r8
52		cmp r4, #0
53		bleq _errNull
54		mov r8, r5
55		str r8, [r4, #0]
56		@ Stack pointer unchanged, no stack allocated arguments
57		ldr r8, =.L.str1
58		push {r8}
59		pop {r8}
60		mov r8, r8
61		mov r0, r8
62		@ statement primitives do not return results (but will clobber r0/rax)
63		bl _prints
64		mov r8, #0
65		mov r6, r8
66		@ Stack pointer unchanged, no stack allocated arguments
67		@ load the current value in the destination of the read so it supports defaults
68		mov r8, r6
69		mov r0, r8
70		bl _readi
71		mov r12, r0
72		mov r8, r12
73		mov r6, r8
74		cmp r4, #0
75		bleq _errNull
76		mov r8, r6
77		str r8, [r4, #4]
78		mov r8, #0
79		mov r5, r8
80		ldr r8, =-1
81		mov r8, r8
82		mov r6, r8
83		@ Stack pointer unchanged, no stack allocated arguments
84		ldr r8, =.L.str2
85		push {r8}
86		pop {r8}
87		mov r8, r8
88		mov r0, r8
89		@ statement primitives do not return results (but will clobber r0/rax)
90		bl _prints
91		cmp r4, #0
92		bleq _errNull
93		ldr r8, [r4, #0]
94		mov r8, r8
95		push {r8}
96		pop {r8}
97		mov r8, r8
98		mov r5, r8
99		@ Stack pointer unchanged, no stack allocated arguments
100		mov r8, r5
101		mov r0, r8
102		@ statement primitives do not return results (but will clobber r0/rax)
103		bl _printc
104		bl _println
105		@ Stack pointer unchanged, no stack allocated arguments
106		ldr r8, =.L.str3
107		push {r8}
108		pop {r8}
109		mov r8, r8
110		mov r0, r8
111		@ statement primitives do not return results (but will clobber r0/rax)
112		bl _prints
113		cmp r4, #0
114		bleq _errNull
115		ldr r8, [r4, #4]
116		mov r8, r8
117		push {r8}
118		pop {r8}
119		mov r8, r8
120		mov r6, r8
121		@ Stack pointer unchanged, no stack allocated arguments
122		mov r8, r6
123		mov r0, r8
124		@ statement primitives do not return results (but will clobber r0/rax)
125		bl _printi
126		bl _println
127		@ Stack pointer unchanged, no stack allocated variables
128		mov r0, #0
129		pop {r4, r5, r6, r8, r10, r12}
130		pop {fp, pc}
131	
132	@ length of .L._readc_str0
133		.word 3
134	.L._readc_str0:
135		.asciz " %c"
136	.align 4
137	_readc:
138		push {fp, lr}
139		mov fp, sp
140		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
141		bic sp, sp, #0x7
142		@ R0 contains the "original" value of the destination of the read
143		@ allocate space on the stack to store the read: preserve alignment!
144		@ the passed default argument should be stored in case of EOF
145		sub sp, sp, #8
146		strb r0, [sp, #0]
147		mov r1, sp
148		adr r0, .L._readc_str0
149		bl scanf
150		ldrsb r0, [sp, #0]
151		add sp, sp, #8
152		mov sp, fp
153		pop {fp, pc}
154	
155	@ length of .L._prints_str0
156		.word 4
157	.L._prints_str0:
158		.asciz "%.*s"
159	.align 4
160	_prints:
161		push {fp, lr}
162		mov fp, sp
163		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
164		bic sp, sp, #0x7
165		mov r2, r0
166		ldr r1, [r0, #-4]
167		adr r0, .L._prints_str0
168		bl printf
169		mov r0, #0
170		bl fflush
171		mov sp, fp
172		pop {fp, pc}
173	
174	@ length of .L._readi_str0
175		.word 2
176	.L._readi_str0:
177		.asciz "%d"
178	.align 4
179	_readi:
180		push {fp, lr}
181		mov fp, sp
182		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
183		bic sp, sp, #0x7
184		@ R0 contains the "original" value of the destination of the read
185		@ allocate space on the stack to store the read: preserve alignment!
186		@ the passed default argument should be stored in case of EOF
187		sub sp, sp, #8
188		str r0, [sp, #0]
189		mov r1, sp
190		adr r0, .L._readi_str0
191		bl scanf
192		ldr r0, [sp, #0]
193		add sp, sp, #8
194		mov sp, fp
195		pop {fp, pc}
196	
197	_malloc:
198		push {fp, lr}
199		mov fp, sp
200		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
201		bic sp, sp, #0x7
202		bl malloc
203		cmp r0, #0
204		bleq _errOutOfMemory
205		mov sp, fp
206		pop {fp, pc}
207	
208	@ length of .L._printi_str0
209		.word 2
210	.L._printi_str0:
211		.asciz "%d"
212	.align 4
213	_printi:
214		push {fp, lr}
215		mov fp, sp
216		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
217		bic sp, sp, #0x7
218		mov r1, r0
219		adr r0, .L._printi_str0
220		bl printf
221		mov r0, #0
222		bl fflush
223		mov sp, fp
224		pop {fp, pc}
225	
226	@ length of .L._printc_str0
227		.word 2
228	.L._printc_str0:
229		.asciz "%c"
230	.align 4
231	_printc:
232		push {fp, lr}
233		mov fp, sp
234		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
235		bic sp, sp, #0x7
236		mov r1, r0
237		adr r0, .L._printc_str0
238		bl printf
239		mov r0, #0
240		bl fflush
241		mov sp, fp
242		pop {fp, pc}
243	
244	@ length of .L._println_str0
245		.word 0
246	.L._println_str0:
247		.asciz ""
248	.align 4
249	_println:
250		push {fp, lr}
251		mov fp, sp
252		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
253		bic sp, sp, #0x7
254		adr r0, .L._println_str0
255		bl puts
256		mov r0, #0
257		bl fflush
258		mov sp, fp
259		pop {fp, pc}
260	
261	_exit:
262		push {fp, lr}
263		mov fp, sp
264		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
265		bic sp, sp, #0x7
266		bl exit
267		mov sp, fp
268		pop {fp, pc}
269	
270	@ length of .L._errOutOfMemory_str0
271		.word 27
272	.L._errOutOfMemory_str0:
273		.asciz "fatal error: out of memory\n"
274	.align 4
275	_errOutOfMemory:
276		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
277		bic sp, sp, #0x7
278		adr r0, .L._errOutOfMemory_str0
279		bl _prints
280		mov r0, #255
281		bl _exit
282	
283	@ length of .L._errNull_str0
284		.word 45
285	.L._errNull_str0:
286		.asciz "fatal error: null pair dereferenced or freed\n"
287	.align 4
288	_errNull:
289		@ external calls must be stack-aligned to 8 bytes, accomplished by clearing with mask 7
290		bic sp, sp, #0x7
291		adr r0, .L._errNull_str0
292		bl _prints
293		mov r0, #255
294		bl _exit
===========================================================
-- Finished

