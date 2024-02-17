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
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19, x20, x21, x22, x23, x24}
8		stp x19, x20, [sp, #-48]!
9		stp x21, x22, [sp, #16]
10		stp x23, x24, [sp, #32]
11		mov fp, sp
12		// Stack pointer unchanged, no stack allocated variables
13		mov w0, #16
14		bl _malloc
15		mov x16, x0
16		mov x8, #10
17		str x8, [x16, #0]
18		mov x8, #97
19		str x8, [x16, #8]
20		mov x8, x16
21		mov x19, x8
22		mov x8, x19
23		mov x20, x8
24		// Stack pointer unchanged, no stack allocated arguments
25		mov x8, x19
26		mov x0, x8
27		// statement primitives do not return results (but will clobber r0/rax)
28		bl _printp
29		bl _println
30		// Stack pointer unchanged, no stack allocated arguments
31		mov x8, x20
32		mov x0, x8
33		// statement primitives do not return results (but will clobber r0/rax)
34		bl _printp
35		bl _println
36		// Stack pointer unchanged, no stack allocated arguments
37		cmp x19, x20
38		cset x8, eq
39		// push {x8}
40		stp x8, xzr, [sp, #-16]!
41		// pop {x8}
42		ldp x8, xzr, [sp], #16
43		mov x8, x8
44		mov x0, x8
45		// statement primitives do not return results (but will clobber r0/rax)
46		bl _printb
47		bl _println
48		cbz x19, _errNull
49		ldr x8, [x19, #0]
50		mov x8, x8
51		mov x21, x8
52		cbz x20, _errNull
53		ldr x8, [x20, #0]
54		mov x8, x8
55		mov x22, x8
56		// Stack pointer unchanged, no stack allocated arguments
57		mov x8, x21
58		mov x0, x8
59		// statement primitives do not return results (but will clobber r0/rax)
60		bl _printi
61		bl _println
62		// Stack pointer unchanged, no stack allocated arguments
63		mov x8, x22
64		mov x0, x8
65		// statement primitives do not return results (but will clobber r0/rax)
66		bl _printi
67		bl _println
68		// Stack pointer unchanged, no stack allocated arguments
69		cmp x21, x22
70		cset x8, eq
71		// push {x8}
72		stp x8, xzr, [sp, #-16]!
73		// pop {x8}
74		ldp x8, xzr, [sp], #16
75		mov x8, x8
76		mov x0, x8
77		// statement primitives do not return results (but will clobber r0/rax)
78		bl _printb
79		bl _println
80		cbz x19, _errNull
81		ldr x8, [x19, #8]
82		mov x8, x8
83		mov x23, x8
84		cbz x20, _errNull
85		ldr x8, [x20, #8]
86		mov x8, x8
87		mov x24, x8
88		// Stack pointer unchanged, no stack allocated arguments
89		mov x8, x23
90		mov x0, x8
91		// statement primitives do not return results (but will clobber r0/rax)
92		bl _printc
93		bl _println
94		// Stack pointer unchanged, no stack allocated arguments
95		mov x8, x24
96		mov x0, x8
97		// statement primitives do not return results (but will clobber r0/rax)
98		bl _printc
99		bl _println
100		// Stack pointer unchanged, no stack allocated arguments
101		cmp x23, x24
102		cset x8, eq
103		// push {x8}
104		stp x8, xzr, [sp, #-16]!
105		// pop {x8}
106		ldp x8, xzr, [sp], #16
107		mov x8, x8
108		mov x0, x8
109		// statement primitives do not return results (but will clobber r0/rax)
110		bl _printb
111		bl _println
112		// Stack pointer unchanged, no stack allocated variables
113		mov x0, #0
114		// pop {x19, x20, x21, x22, x23, x24}
115		ldp x21, x22, [sp, #16]
116		ldp x23, x24, [sp, #32]
117		ldp x19, x20, [sp], #48
118		// pop {fp, lr}
119		ldp fp, lr, [sp], #16
120		ret
121	
122	// length of .L._prints_str0
123		.word 4
124	.L._prints_str0:
125		.asciz "%.*s"
126	.align 4
127	_prints:
128		// push {lr}
129		stp lr, xzr, [sp, #-16]!
130		mov x2, x0
131		ldrsw x1, [x0, #-4]
132		adr x0, .L._prints_str0
133		bl printf
134		mov x0, #0
135		bl fflush
136		// pop {lr}
137		ldp lr, xzr, [sp], #16
138		ret
139	
140	// length of .L._printp_str0
141		.word 2
142	.L._printp_str0:
143		.asciz "%p"
144	.align 4
145	_printp:
146		// push {lr}
147		stp lr, xzr, [sp, #-16]!
148		mov x1, x0
149		adr x0, .L._printp_str0
150		bl printf
151		mov x0, #0
152		bl fflush
153		// pop {lr}
154		ldp lr, xzr, [sp], #16
155		ret
156	
157	_malloc:
158		// push {lr}
159		stp lr, xzr, [sp, #-16]!
160		bl malloc
161		cbz x0, _errOutOfMemory
162		// pop {lr}
163		ldp lr, xzr, [sp], #16
164		ret
165	
166	// length of .L._printi_str0
167		.word 2
168	.L._printi_str0:
169		.asciz "%d"
170	.align 4
171	_printi:
172		// push {lr}
173		stp lr, xzr, [sp, #-16]!
174		mov x1, x0
175		adr x0, .L._printi_str0
176		bl printf
177		mov x0, #0
178		bl fflush
179		// pop {lr}
180		ldp lr, xzr, [sp], #16
181		ret
182	
183	// length of .L._printb_str0
184		.word 5
185	.L._printb_str0:
186		.asciz "false"
187	// length of .L._printb_str1
188		.word 4
189	.L._printb_str1:
190		.asciz "true"
191	// length of .L._printb_str2
192		.word 4
193	.L._printb_str2:
194		.asciz "%.*s"
195	.align 4
196	_printb:
197		// push {lr}
198		stp lr, xzr, [sp, #-16]!
199		cmp w0, #0
200		b.ne .L_printb0
201		adr x2, .L._printb_str0
202		b .L_printb1
203	.L_printb0:
204		adr x2, .L._printb_str1
205	.L_printb1:
206		ldrsw x1, [x2, #-4]
207		adr x0, .L._printb_str2
208		bl printf
209		mov x0, #0
210		bl fflush
211		// pop {lr}
212		ldp lr, xzr, [sp], #16
213		ret
214	
215	// length of .L._printc_str0
216		.word 2
217	.L._printc_str0:
218		.asciz "%c"
219	.align 4
220	_printc:
221		// push {lr}
222		stp lr, xzr, [sp, #-16]!
223		mov x1, x0
224		adr x0, .L._printc_str0
225		bl printf
226		mov x0, #0
227		bl fflush
228		// pop {lr}
229		ldp lr, xzr, [sp], #16
230		ret
231	
232	// length of .L._println_str0
233		.word 0
234	.L._println_str0:
235		.asciz ""
236	.align 4
237	_println:
238		// push {lr}
239		stp lr, xzr, [sp, #-16]!
240		adr x0, .L._println_str0
241		bl puts
242		mov x0, #0
243		bl fflush
244		// pop {lr}
245		ldp lr, xzr, [sp], #16
246		ret
247	
248	// length of .L._errOutOfMemory_str0
249		.word 27
250	.L._errOutOfMemory_str0:
251		.asciz "fatal error: out of memory\n"
252	.align 4
253	_errOutOfMemory:
254		adr x0, .L._errOutOfMemory_str0
255		bl _prints
256		mov w0, #-1
257		bl exit
258	
259	// length of .L._errNull_str0
260		.word 45
261	.L._errNull_str0:
262		.asciz "fatal error: null pair dereferenced or freed\n"
263	.align 4
264	_errNull:
265		adr x0, .L._errNull_str0
266		bl _prints
267		mov w0, #-1
268		bl exit
===========================================================
-- Finished

