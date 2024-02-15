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
1	// length of .L.str0
2		.word 39
3	.L.str0:
4		.asciz "Please enter the first element (char): "
5	// length of .L.str1
6		.word 39
7	.L.str1:
8		.asciz "Please enter the second element (int): "
9	// length of .L.str2
10		.word 22
11	.L.str2:
12		.asciz "The first element was "
13	// length of .L.str3
14		.word 23
15	.L.str3:
16		.asciz "The second element was "
17	.align 4
18	.text
19	.global main
20	main:
21		// push {fp, lr}
22		stp fp, lr, [sp, #-16]!
23		// push {x19, x20, x21}
24		stp x19, x20, [sp, #-32]!
25		stp x21, xzr, [sp, #16]
26		mov fp, sp
27		// Stack pointer unchanged, no stack allocated variables
28		mov w0, #16
29		bl _malloc
30		mov x16, x0
31		mov x8, #0
32		str x8, [x16, #0]
33		mov x8, #0
34		str x8, [x16, #8]
35		mov x8, x16
36		mov x19, x8
37		// Stack pointer unchanged, no stack allocated arguments
38		adrp x8, .L.str0
39		add x8, x8, :lo12:.L.str0
40		// push {x8}
41		stp x8, xzr, [sp, #-16]!
42		// pop {x8}
43		ldp x8, xzr, [sp], #16
44		mov x8, x8
45		mov x0, x8
46		// statement primitives do not return results (but will clobber r0/rax)
47		bl _prints
48		mov x8, #48
49		mov x20, x8
50		// Stack pointer unchanged, no stack allocated arguments
51		// load the current value in the destination of the read so it supports defaults
52		mov x8, x20
53		mov x0, x8
54		bl _readc
55		mov x16, x0
56		mov x8, x16
57		mov x20, x8
58		cbz x19, _errNull
59		mov x8, x20
60		str x8, [x19, #0]
61		// Stack pointer unchanged, no stack allocated arguments
62		adrp x8, .L.str1
63		add x8, x8, :lo12:.L.str1
64		// push {x8}
65		stp x8, xzr, [sp, #-16]!
66		// pop {x8}
67		ldp x8, xzr, [sp], #16
68		mov x8, x8
69		mov x0, x8
70		// statement primitives do not return results (but will clobber r0/rax)
71		bl _prints
72		mov x8, #0
73		mov x21, x8
74		// Stack pointer unchanged, no stack allocated arguments
75		// load the current value in the destination of the read so it supports defaults
76		mov x8, x21
77		mov x0, x8
78		bl _readi
79		mov x16, x0
80		mov x8, x16
81		mov x21, x8
82		cbz x19, _errNull
83		mov x8, x21
84		str x8, [x19, #8]
85		mov x8, #0
86		mov x20, x8
87		mov x8, #-1
88		mov x21, x8
89		// Stack pointer unchanged, no stack allocated arguments
90		adrp x8, .L.str2
91		add x8, x8, :lo12:.L.str2
92		// push {x8}
93		stp x8, xzr, [sp, #-16]!
94		// pop {x8}
95		ldp x8, xzr, [sp], #16
96		mov x8, x8
97		mov x0, x8
98		// statement primitives do not return results (but will clobber r0/rax)
99		bl _prints
100		cbz x19, _errNull
101		ldr x8, [x19, #0]
102		mov x8, x8
103		// push {x8}
104		stp x8, xzr, [sp, #-16]!
105		// pop {x8}
106		ldp x8, xzr, [sp], #16
107		mov x8, x8
108		mov x20, x8
109		// Stack pointer unchanged, no stack allocated arguments
110		mov x8, x20
111		mov x0, x8
112		// statement primitives do not return results (but will clobber r0/rax)
113		bl _printc
114		bl _println
115		// Stack pointer unchanged, no stack allocated arguments
116		adrp x8, .L.str3
117		add x8, x8, :lo12:.L.str3
118		// push {x8}
119		stp x8, xzr, [sp, #-16]!
120		// pop {x8}
121		ldp x8, xzr, [sp], #16
122		mov x8, x8
123		mov x0, x8
124		// statement primitives do not return results (but will clobber r0/rax)
125		bl _prints
126		cbz x19, _errNull
127		ldr x8, [x19, #8]
128		mov x8, x8
129		// push {x8}
130		stp x8, xzr, [sp, #-16]!
131		// pop {x8}
132		ldp x8, xzr, [sp], #16
133		mov x8, x8
134		mov x21, x8
135		// Stack pointer unchanged, no stack allocated arguments
136		mov x8, x21
137		mov x0, x8
138		// statement primitives do not return results (but will clobber r0/rax)
139		bl _printi
140		bl _println
141		// Stack pointer unchanged, no stack allocated variables
142		mov x0, #0
143		// pop {x19, x20, x21}
144		ldp x21, xzr, [sp, #16]
145		ldp x19, x20, [sp], #32
146		// pop {fp, lr}
147		ldp fp, lr, [sp], #16
148		ret
149	
150	// length of .L._readc_str0
151		.word 3
152	.L._readc_str0:
153		.asciz " %c"
154	.align 4
155	_readc:
156		// X0 contains the "original" value of the destination of the read
157		// allocate space on the stack to store the read: preserve alignment!
158		// the passed default argument should be stored in case of EOF
159		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
160		// push {x0, lr}
161		stp x0, lr, [sp, #-16]!
162		mov x1, sp
163		adr x0, .L._readc_str0
164		bl scanf
165		// pop {x0, lr}
166		ldp x0, lr, [sp], #16
167		ret
168	
169	// length of .L._prints_str0
170		.word 4
171	.L._prints_str0:
172		.asciz "%.*s"
173	.align 4
174	_prints:
175		// push {lr}
176		stp lr, xzr, [sp, #-16]!
177		mov x2, x0
178		ldrsw x1, [x0, #-4]
179		adr x0, .L._prints_str0
180		bl printf
181		mov x0, #0
182		bl fflush
183		// pop {lr}
184		ldp lr, xzr, [sp], #16
185		ret
186	
187	// length of .L._readi_str0
188		.word 2
189	.L._readi_str0:
190		.asciz "%d"
191	.align 4
192	_readi:
193		// X0 contains the "original" value of the destination of the read
194		// allocate space on the stack to store the read: preserve alignment!
195		// the passed default argument should be stored in case of EOF
196		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
197		// push {x0, lr}
198		stp x0, lr, [sp, #-16]!
199		mov x1, sp
200		adr x0, .L._readi_str0
201		bl scanf
202		// pop {x0, lr}
203		ldp x0, lr, [sp], #16
204		ret
205	
206	_malloc:
207		// push {lr}
208		stp lr, xzr, [sp, #-16]!
209		bl malloc
210		cbz x0, _errOutOfMemory
211		// pop {lr}
212		ldp lr, xzr, [sp], #16
213		ret
214	
215	// length of .L._printi_str0
216		.word 2
217	.L._printi_str0:
218		.asciz "%d"
219	.align 4
220	_printi:
221		// push {lr}
222		stp lr, xzr, [sp, #-16]!
223		mov x1, x0
224		adr x0, .L._printi_str0
225		bl printf
226		mov x0, #0
227		bl fflush
228		// pop {lr}
229		ldp lr, xzr, [sp], #16
230		ret
231	
232	// length of .L._printc_str0
233		.word 2
234	.L._printc_str0:
235		.asciz "%c"
236	.align 4
237	_printc:
238		// push {lr}
239		stp lr, xzr, [sp, #-16]!
240		mov x1, x0
241		adr x0, .L._printc_str0
242		bl printf
243		mov x0, #0
244		bl fflush
245		// pop {lr}
246		ldp lr, xzr, [sp], #16
247		ret
248	
249	// length of .L._println_str0
250		.word 0
251	.L._println_str0:
252		.asciz ""
253	.align 4
254	_println:
255		// push {lr}
256		stp lr, xzr, [sp, #-16]!
257		adr x0, .L._println_str0
258		bl puts
259		mov x0, #0
260		bl fflush
261		// pop {lr}
262		ldp lr, xzr, [sp], #16
263		ret
264	
265	// length of .L._errOutOfMemory_str0
266		.word 27
267	.L._errOutOfMemory_str0:
268		.asciz "fatal error: out of memory\n"
269	.align 4
270	_errOutOfMemory:
271		adr x0, .L._errOutOfMemory_str0
272		bl _prints
273		mov w0, #-1
274		bl exit
275	
276	// length of .L._errNull_str0
277		.word 45
278	.L._errNull_str0:
279		.asciz "fatal error: null pair dereferenced or freed\n"
280	.align 4
281	_errNull:
282		adr x0, .L._errNull_str0
283		bl _prints
284		mov w0, #-1
285		bl exit
===========================================================
-- Finished

