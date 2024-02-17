./valid/array/arrayPrint.wacc
calling the reference compiler on ./valid/array/arrayPrint.wacc
-- Test: arrayPrint.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print the contents of a simple array

# Output:
# #addrs# = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
#

# Program:

begin
  int[] a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] ;
  int i = 0 ;
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
arrayPrint.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 4
3	.L.str0:
4		.asciz " = {"
5	// length of .L.str1
6		.word 2
7	.L.str1:
8		.asciz ", "
9	// length of .L.str2
10		.word 1
11	.L.str2:
12		.asciz "}"
13	.align 4
14	.text
15	.global main
16	main:
17		// push {fp, lr}
18		stp fp, lr, [sp, #-16]!
19		// push {x19, x20}
20		stp x19, x20, [sp, #-16]!
21		mov fp, sp
22		// Stack pointer unchanged, no stack allocated variables
23		// 10 element array
24		mov w0, #44
25		bl _malloc
26		mov x16, x0
27		// array pointers are shifted forwards by 4 bytes (to account for size)
28		add x16, x16, #4
29		mov x8, #10
30		str w8, [x16, #-4]
31		mov x8, #0
32		str w8, [x16, #0]
33		mov x8, #1
34		str w8, [x16, #4]
35		mov x8, #2
36		str w8, [x16, #8]
37		mov x8, #3
38		str w8, [x16, #12]
39		mov x8, #4
40		str w8, [x16, #16]
41		mov x8, #5
42		str w8, [x16, #20]
43		mov x8, #6
44		str w8, [x16, #24]
45		mov x8, #7
46		str w8, [x16, #28]
47		mov x8, #8
48		str w8, [x16, #32]
49		mov x8, #9
50		str w8, [x16, #36]
51		mov x8, x16
52		mov x19, x8
53		mov x8, #0
54		mov x20, x8
55		// Stack pointer unchanged, no stack allocated arguments
56		mov x8, x19
57		mov x0, x8
58		// statement primitives do not return results (but will clobber r0/rax)
59		bl _printp
60		// Stack pointer unchanged, no stack allocated arguments
61		adrp x8, .L.str0
62		add x8, x8, :lo12:.L.str0
63		// push {x8}
64		stp x8, xzr, [sp, #-16]!
65		// pop {x8}
66		ldp x8, xzr, [sp], #16
67		mov x8, x8
68		mov x0, x8
69		// statement primitives do not return results (but will clobber r0/rax)
70		bl _prints
71		mov x8, #0
72		mov x20, x8
73		b .L0
74	.L1:
75		// Stack pointer unchanged, no stack allocated arguments
76		mov w17, w20
77		mov x7, x19
78		bl _arrLoad4
79		mov w8, w7
80		mov x8, x8
81		mov x0, x8
82		// statement primitives do not return results (but will clobber r0/rax)
83		bl _printi
84		cmp x20, #9
85		b.lt .L2
86		b .L3
87	.L2:
88		// Stack pointer unchanged, no stack allocated arguments
89		adrp x8, .L.str1
90		add x8, x8, :lo12:.L.str1
91		// push {x8}
92		stp x8, xzr, [sp, #-16]!
93		// pop {x8}
94		ldp x8, xzr, [sp], #16
95		mov x8, x8
96		mov x0, x8
97		// statement primitives do not return results (but will clobber r0/rax)
98		bl _prints
99	.L3:
100		adds w8, w20, #1
101		b.vs _errOverflow
102		sxtw x8, w8
103		// push {x8}
104		stp x8, xzr, [sp, #-16]!
105		// pop {x8}
106		ldp x8, xzr, [sp], #16
107		mov x8, x8
108		mov x20, x8
109	.L0:
110		cmp x20, #10
111		b.lt .L1
112		// Stack pointer unchanged, no stack allocated arguments
113		adrp x8, .L.str2
114		add x8, x8, :lo12:.L.str2
115		// push {x8}
116		stp x8, xzr, [sp, #-16]!
117		// pop {x8}
118		ldp x8, xzr, [sp], #16
119		mov x8, x8
120		mov x0, x8
121		// statement primitives do not return results (but will clobber r0/rax)
122		bl _prints
123		bl _println
124		// Stack pointer unchanged, no stack allocated variables
125		mov x0, #0
126		// pop {x19, x20}
127		ldp x19, x20, [sp], #16
128		// pop {fp, lr}
129		ldp fp, lr, [sp], #16
130		ret
131	
132	// length of .L._prints_str0
133		.word 4
134	.L._prints_str0:
135		.asciz "%.*s"
136	.align 4
137	_prints:
138		// push {lr}
139		stp lr, xzr, [sp, #-16]!
140		mov x2, x0
141		ldrsw x1, [x0, #-4]
142		adr x0, .L._prints_str0
143		bl printf
144		mov x0, #0
145		bl fflush
146		// pop {lr}
147		ldp lr, xzr, [sp], #16
148		ret
149	
150	// length of .L._printp_str0
151		.word 2
152	.L._printp_str0:
153		.asciz "%p"
154	.align 4
155	_printp:
156		// push {lr}
157		stp lr, xzr, [sp, #-16]!
158		mov x1, x0
159		adr x0, .L._printp_str0
160		bl printf
161		mov x0, #0
162		bl fflush
163		// pop {lr}
164		ldp lr, xzr, [sp], #16
165		ret
166	
167	_malloc:
168		// push {lr}
169		stp lr, xzr, [sp, #-16]!
170		bl malloc
171		cbz x0, _errOutOfMemory
172		// pop {lr}
173		ldp lr, xzr, [sp], #16
174		ret
175	
176	// length of .L._printi_str0
177		.word 2
178	.L._printi_str0:
179		.asciz "%d"
180	.align 4
181	_printi:
182		// push {lr}
183		stp lr, xzr, [sp, #-16]!
184		mov x1, x0
185		adr x0, .L._printi_str0
186		bl printf
187		mov x0, #0
188		bl fflush
189		// pop {lr}
190		ldp lr, xzr, [sp], #16
191		ret
192	
193	// length of .L._println_str0
194		.word 0
195	.L._println_str0:
196		.asciz ""
197	.align 4
198	_println:
199		// push {lr}
200		stp lr, xzr, [sp, #-16]!
201		adr x0, .L._println_str0
202		bl puts
203		mov x0, #0
204		bl fflush
205		// pop {lr}
206		ldp lr, xzr, [sp], #16
207		ret
208	
209	_arrLoad4:
210		// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7
211		// push {lr}
212		stp lr, xzr, [sp, #-16]!
213		sxtw x17, w17
214		cmp w17, #0
215		csel x1, x17, x1, lt
216		b.lt _errOutOfBounds
217		ldrsw lr, [x7, #-4]
218		cmp w17, w30
219		csel x1, x17, x1, ge
220		b.ge _errOutOfBounds
221		ldrsw x7, [x7, x17, lsl #2]
222		// pop {lr}
223		ldp lr, xzr, [sp], #16
224		ret
225	
226	// length of .L._errOutOfMemory_str0
227		.word 27
228	.L._errOutOfMemory_str0:
229		.asciz "fatal error: out of memory\n"
230	.align 4
231	_errOutOfMemory:
232		adr x0, .L._errOutOfMemory_str0
233		bl _prints
234		mov w0, #-1
235		bl exit
236	
237	// length of .L._errOutOfBounds_str0
238		.word 42
239	.L._errOutOfBounds_str0:
240		.asciz "fatal error: array index %d out of bounds\n"
241	.align 4
242	_errOutOfBounds:
243		adr x0, .L._errOutOfBounds_str0
244		bl printf
245		mov x0, #0
246		bl fflush
247		mov w0, #-1
248		bl exit
249	
250	// length of .L._errOverflow_str0
251		.word 52
252	.L._errOverflow_str0:
253		.asciz "fatal error: integer overflow or underflow occurred\n"
254	.align 4
255	_errOverflow:
256		adr x0, .L._errOverflow_str0
257		bl _prints
258		mov w0, #-1
259		bl exit
===========================================================
-- Finished

