./valid/pairs/pairarray.wacc
calling the reference compiler on ./valid/pairs/pairarray.wacc
-- Test: pairarray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# ensures that pairs can be unpacked directly from arrays

# Output:
# 3
#

# Program:

begin
  pair(int, int) p = newpair(5, 6);
  pair(int, int)[] a = [p, p];
  fst a[0] = 3 ;
  int x = fst a[1] ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
pairarray.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19, x20, x21}
8		stp x19, x20, [sp, #-32]!
9		stp x21, xzr, [sp, #16]
10		mov fp, sp
11		// Stack pointer unchanged, no stack allocated variables
12		mov w0, #16
13		bl _malloc
14		mov x16, x0
15		mov x8, #5
16		str x8, [x16, #0]
17		mov x8, #6
18		str x8, [x16, #8]
19		mov x8, x16
20		mov x19, x8
21		// 2 element array
22		mov w0, #20
23		bl _malloc
24		mov x16, x0
25		// array pointers are shifted forwards by 4 bytes (to account for size)
26		add x16, x16, #4
27		mov x8, #2
28		str w8, [x16, #-4]
29		mov x8, x19
30		str x8, [x16, #0]
31		mov x8, x19
32		str x8, [x16, #8]
33		mov x8, x16
34		mov x20, x8
35		mov w17, #0
36		mov x7, x20
37		bl _arrLoad8
38		mov x8, x7
39		mov x8, x8
40		// push {x8}
41		stp x8, xzr, [sp, #-16]!
42		// pop {x9}
43		ldp x9, xzr, [sp], #16
44		cbz x9, _errNull
45		mov x8, #3
46		str x8, [x9, #0]
47		mov w17, #1
48		mov x7, x20
49		bl _arrLoad8
50		mov x8, x7
51		mov x8, x8
52		// push {x8}
53		stp x8, xzr, [sp, #-16]!
54		// pop {x8}
55		ldp x8, xzr, [sp], #16
56		cbz x8, _errNull
57		ldr x8, [x8, #0]
58		mov x8, x8
59		mov x21, x8
60		// Stack pointer unchanged, no stack allocated arguments
61		mov x8, x21
62		mov x0, x8
63		// statement primitives do not return results (but will clobber r0/rax)
64		bl _printi
65		bl _println
66		// Stack pointer unchanged, no stack allocated variables
67		mov x0, #0
68		// pop {x19, x20, x21}
69		ldp x21, xzr, [sp, #16]
70		ldp x19, x20, [sp], #32
71		// pop {fp, lr}
72		ldp fp, lr, [sp], #16
73		ret
74	
75	// length of .L._prints_str0
76		.word 4
77	.L._prints_str0:
78		.asciz "%.*s"
79	.align 4
80	_prints:
81		// push {lr}
82		stp lr, xzr, [sp, #-16]!
83		mov x2, x0
84		ldrsw x1, [x0, #-4]
85		adr x0, .L._prints_str0
86		bl printf
87		mov x0, #0
88		bl fflush
89		// pop {lr}
90		ldp lr, xzr, [sp], #16
91		ret
92	
93	_malloc:
94		// push {lr}
95		stp lr, xzr, [sp, #-16]!
96		bl malloc
97		cbz x0, _errOutOfMemory
98		// pop {lr}
99		ldp lr, xzr, [sp], #16
100		ret
101	
102	// length of .L._printi_str0
103		.word 2
104	.L._printi_str0:
105		.asciz "%d"
106	.align 4
107	_printi:
108		// push {lr}
109		stp lr, xzr, [sp, #-16]!
110		mov x1, x0
111		adr x0, .L._printi_str0
112		bl printf
113		mov x0, #0
114		bl fflush
115		// pop {lr}
116		ldp lr, xzr, [sp], #16
117		ret
118	
119	// length of .L._println_str0
120		.word 0
121	.L._println_str0:
122		.asciz ""
123	.align 4
124	_println:
125		// push {lr}
126		stp lr, xzr, [sp, #-16]!
127		adr x0, .L._println_str0
128		bl puts
129		mov x0, #0
130		bl fflush
131		// pop {lr}
132		ldp lr, xzr, [sp], #16
133		ret
134	
135	_arrLoad8:
136		// Special calling convention: array ptr passed in X7, index in X17, LR (W30) is used as general register, and return into X7
137		// push {lr}
138		stp lr, xzr, [sp, #-16]!
139		sxtw x17, w17
140		cmp w17, #0
141		csel x1, x17, x1, lt
142		b.lt _errOutOfBounds
143		ldrsw lr, [x7, #-4]
144		cmp w17, w30
145		csel x1, x17, x1, ge
146		b.ge _errOutOfBounds
147		ldr x7, [x7, x17, lsl #3]
148		// pop {lr}
149		ldp lr, xzr, [sp], #16
150		ret
151	
152	// length of .L._errOutOfMemory_str0
153		.word 27
154	.L._errOutOfMemory_str0:
155		.asciz "fatal error: out of memory\n"
156	.align 4
157	_errOutOfMemory:
158		adr x0, .L._errOutOfMemory_str0
159		bl _prints
160		mov w0, #-1
161		bl exit
162	
163	// length of .L._errNull_str0
164		.word 45
165	.L._errNull_str0:
166		.asciz "fatal error: null pair dereferenced or freed\n"
167	.align 4
168	_errNull:
169		adr x0, .L._errNull_str0
170		bl _prints
171		mov w0, #-1
172		bl exit
173	
174	// length of .L._errOutOfBounds_str0
175		.word 42
176	.L._errOutOfBounds_str0:
177		.asciz "fatal error: array index %d out of bounds\n"
178	.align 4
179	_errOutOfBounds:
180		adr x0, .L._errOutOfBounds_str0
181		bl printf
182		mov x0, #0
183		bl fflush
184		mov w0, #-1
185		bl exit
===========================================================
-- Finished

