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
1	// length of .L.str0
2		.word 8
3	.L.str0:
4		.asciz "list = {"
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
19		// push {x19, x20, x21, x22, x23, x24, x25}
20		stp x19, x20, [sp, #-64]!
21		stp x21, x22, [sp, #16]
22		stp x23, x24, [sp, #32]
23		stp x25, xzr, [sp, #48]
24		mov fp, sp
25		// Stack pointer unchanged, no stack allocated variables
26		mov w0, #16
27		bl _malloc
28		mov x16, x0
29		mov x8, #11
30		str x8, [x16, #0]
31		mov x8, #0
32		str x8, [x16, #8]
33		mov x8, x16
34		mov x19, x8
35		mov w0, #16
36		bl _malloc
37		mov x16, x0
38		mov x8, #4
39		str x8, [x16, #0]
40		mov x8, x19
41		str x8, [x16, #8]
42		mov x8, x16
43		mov x20, x8
44		mov w0, #16
45		bl _malloc
46		mov x16, x0
47		mov x8, #2
48		str x8, [x16, #0]
49		mov x8, x20
50		str x8, [x16, #8]
51		mov x8, x16
52		mov x21, x8
53		mov w0, #16
54		bl _malloc
55		mov x16, x0
56		mov x8, #1
57		str x8, [x16, #0]
58		mov x8, x21
59		str x8, [x16, #8]
60		mov x8, x16
61		mov x22, x8
62		// Stack pointer unchanged, no stack allocated arguments
63		adrp x8, .L.str0
64		add x8, x8, :lo12:.L.str0
65		// push {x8}
66		stp x8, xzr, [sp, #-16]!
67		// pop {x8}
68		ldp x8, xzr, [sp], #16
69		mov x8, x8
70		mov x0, x8
71		// statement primitives do not return results (but will clobber r0/rax)
72		bl _prints
73		mov x8, x22
74		mov x23, x8
75		cbz x23, _errNull
76		ldr x8, [x23, #8]
77		mov x8, x8
78		mov x24, x8
79		mov x8, #0
80		mov x25, x8
81		b .L0
82	.L1:
83		cbz x23, _errNull
84		ldr x8, [x23, #0]
85		mov x8, x8
86		// push {x8}
87		stp x8, xzr, [sp, #-16]!
88		// pop {x8}
89		ldp x8, xzr, [sp], #16
90		mov x8, x8
91		mov x25, x8
92		// Stack pointer unchanged, no stack allocated arguments
93		mov x8, x25
94		mov x0, x8
95		// statement primitives do not return results (but will clobber r0/rax)
96		bl _printi
97		// Stack pointer unchanged, no stack allocated arguments
98		adrp x8, .L.str1
99		add x8, x8, :lo12:.L.str1
100		// push {x8}
101		stp x8, xzr, [sp, #-16]!
102		// pop {x8}
103		ldp x8, xzr, [sp], #16
104		mov x8, x8
105		mov x0, x8
106		// statement primitives do not return results (but will clobber r0/rax)
107		bl _prints
108		mov x8, x24
109		mov x23, x8
110		cbz x23, _errNull
111		ldr x8, [x23, #8]
112		mov x8, x8
113		// push {x8}
114		stp x8, xzr, [sp, #-16]!
115		// pop {x8}
116		ldp x8, xzr, [sp], #16
117		mov x8, x8
118		mov x24, x8
119	.L0:
120		cmp x24, #0
121		b.ne .L1
122		cbz x23, _errNull
123		ldr x8, [x23, #0]
124		mov x8, x8
125		// push {x8}
126		stp x8, xzr, [sp, #-16]!
127		// pop {x8}
128		ldp x8, xzr, [sp], #16
129		mov x8, x8
130		mov x25, x8
131		// Stack pointer unchanged, no stack allocated arguments
132		mov x8, x25
133		mov x0, x8
134		// statement primitives do not return results (but will clobber r0/rax)
135		bl _printi
136		// Stack pointer unchanged, no stack allocated arguments
137		adrp x8, .L.str2
138		add x8, x8, :lo12:.L.str2
139		// push {x8}
140		stp x8, xzr, [sp, #-16]!
141		// pop {x8}
142		ldp x8, xzr, [sp], #16
143		mov x8, x8
144		mov x0, x8
145		// statement primitives do not return results (but will clobber r0/rax)
146		bl _prints
147		bl _println
148		// Stack pointer unchanged, no stack allocated variables
149		mov x0, #0
150		// pop {x19, x20, x21, x22, x23, x24, x25}
151		ldp x21, x22, [sp, #16]
152		ldp x23, x24, [sp, #32]
153		ldp x25, xzr, [sp, #48]
154		ldp x19, x20, [sp], #64
155		// pop {fp, lr}
156		ldp fp, lr, [sp], #16
157		ret
158	
159	// length of .L._prints_str0
160		.word 4
161	.L._prints_str0:
162		.asciz "%.*s"
163	.align 4
164	_prints:
165		// push {lr}
166		stp lr, xzr, [sp, #-16]!
167		mov x2, x0
168		ldrsw x1, [x0, #-4]
169		adr x0, .L._prints_str0
170		bl printf
171		mov x0, #0
172		bl fflush
173		// pop {lr}
174		ldp lr, xzr, [sp], #16
175		ret
176	
177	_malloc:
178		// push {lr}
179		stp lr, xzr, [sp, #-16]!
180		bl malloc
181		cbz x0, _errOutOfMemory
182		// pop {lr}
183		ldp lr, xzr, [sp], #16
184		ret
185	
186	// length of .L._printi_str0
187		.word 2
188	.L._printi_str0:
189		.asciz "%d"
190	.align 4
191	_printi:
192		// push {lr}
193		stp lr, xzr, [sp, #-16]!
194		mov x1, x0
195		adr x0, .L._printi_str0
196		bl printf
197		mov x0, #0
198		bl fflush
199		// pop {lr}
200		ldp lr, xzr, [sp], #16
201		ret
202	
203	// length of .L._println_str0
204		.word 0
205	.L._println_str0:
206		.asciz ""
207	.align 4
208	_println:
209		// push {lr}
210		stp lr, xzr, [sp, #-16]!
211		adr x0, .L._println_str0
212		bl puts
213		mov x0, #0
214		bl fflush
215		// pop {lr}
216		ldp lr, xzr, [sp], #16
217		ret
218	
219	// length of .L._errOutOfMemory_str0
220		.word 27
221	.L._errOutOfMemory_str0:
222		.asciz "fatal error: out of memory\n"
223	.align 4
224	_errOutOfMemory:
225		adr x0, .L._errOutOfMemory_str0
226		bl _prints
227		mov w0, #-1
228		bl exit
229	
230	// length of .L._errNull_str0
231		.word 45
232	.L._errNull_str0:
233		.asciz "fatal error: null pair dereferenced or freed\n"
234	.align 4
235	_errNull:
236		adr x0, .L._errNull_str0
237		bl _prints
238		mov w0, #-1
239		bl exit
===========================================================
-- Finished

