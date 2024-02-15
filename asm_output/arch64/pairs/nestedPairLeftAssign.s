./valid/pairs/nestedPairLeftAssign.wacc
calling the reference compiler on ./valid/pairs/nestedPairLeftAssign.wacc
-- Test: nestedPairLeftAssign.wacc

-- Uploaded file: 
---------------------------------------------------------------
# nested pair assignments are legal as long as the right hand-side type is known

# Output:
# 7
#

# Program:

begin
  pair(int, int) p = newpair(2, 3) ;
  pair(int, pair) q = newpair(1, p) ;
  fst snd q = 7 ;
  int x = fst p ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
nestedPairLeftAssign.s contents are:
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
15		mov x8, #2
16		str x8, [x16, #0]
17		mov x8, #3
18		str x8, [x16, #8]
19		mov x8, x16
20		mov x19, x8
21		mov w0, #16
22		bl _malloc
23		mov x16, x0
24		mov x8, #1
25		str x8, [x16, #0]
26		mov x8, x19
27		str x8, [x16, #8]
28		mov x8, x16
29		mov x20, x8
30		cbz x20, _errNull
31		ldr x8, [x20, #8]
32		mov x8, x8
33		// push {x8}
34		stp x8, xzr, [sp, #-16]!
35		// pop {x9}
36		ldp x9, xzr, [sp], #16
37		cbz x9, _errNull
38		mov x8, #7
39		str x8, [x9, #0]
40		cbz x19, _errNull
41		ldr x8, [x19, #0]
42		mov x8, x8
43		mov x21, x8
44		// Stack pointer unchanged, no stack allocated arguments
45		mov x8, x21
46		mov x0, x8
47		// statement primitives do not return results (but will clobber r0/rax)
48		bl _printi
49		bl _println
50		// Stack pointer unchanged, no stack allocated variables
51		mov x0, #0
52		// pop {x19, x20, x21}
53		ldp x21, xzr, [sp, #16]
54		ldp x19, x20, [sp], #32
55		// pop {fp, lr}
56		ldp fp, lr, [sp], #16
57		ret
58	
59	// length of .L._prints_str0
60		.word 4
61	.L._prints_str0:
62		.asciz "%.*s"
63	.align 4
64	_prints:
65		// push {lr}
66		stp lr, xzr, [sp, #-16]!
67		mov x2, x0
68		ldrsw x1, [x0, #-4]
69		adr x0, .L._prints_str0
70		bl printf
71		mov x0, #0
72		bl fflush
73		// pop {lr}
74		ldp lr, xzr, [sp], #16
75		ret
76	
77	_malloc:
78		// push {lr}
79		stp lr, xzr, [sp, #-16]!
80		bl malloc
81		cbz x0, _errOutOfMemory
82		// pop {lr}
83		ldp lr, xzr, [sp], #16
84		ret
85	
86	// length of .L._printi_str0
87		.word 2
88	.L._printi_str0:
89		.asciz "%d"
90	.align 4
91	_printi:
92		// push {lr}
93		stp lr, xzr, [sp, #-16]!
94		mov x1, x0
95		adr x0, .L._printi_str0
96		bl printf
97		mov x0, #0
98		bl fflush
99		// pop {lr}
100		ldp lr, xzr, [sp], #16
101		ret
102	
103	// length of .L._println_str0
104		.word 0
105	.L._println_str0:
106		.asciz ""
107	.align 4
108	_println:
109		// push {lr}
110		stp lr, xzr, [sp, #-16]!
111		adr x0, .L._println_str0
112		bl puts
113		mov x0, #0
114		bl fflush
115		// pop {lr}
116		ldp lr, xzr, [sp], #16
117		ret
118	
119	// length of .L._errOutOfMemory_str0
120		.word 27
121	.L._errOutOfMemory_str0:
122		.asciz "fatal error: out of memory\n"
123	.align 4
124	_errOutOfMemory:
125		adr x0, .L._errOutOfMemory_str0
126		bl _prints
127		mov w0, #-1
128		bl exit
129	
130	// length of .L._errNull_str0
131		.word 45
132	.L._errNull_str0:
133		.asciz "fatal error: null pair dereferenced or freed\n"
134	.align 4
135	_errNull:
136		adr x0, .L._errNull_str0
137		bl _prints
138		mov w0, #-1
139		bl exit
===========================================================
-- Finished

