./valid/pairs/nestedPairRightExtract.wacc
calling the reference compiler on ./valid/pairs/nestedPairRightExtract.wacc
-- Test: nestedPairRightExtract.wacc

-- Uploaded file: 
---------------------------------------------------------------
# nested pair extractions are legal as long as the left hand-side type is known

# Output:
# 2
#

# Program:

begin
  pair(int, int) p = newpair(2, 3) ;
  pair(int, pair) q = newpair(1, p) ;
  int x = fst snd q ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
nestedPairRightExtract.s contents are:
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
35		// pop {x8}
36		ldp x8, xzr, [sp], #16
37		cbz x8, _errNull
38		ldr x8, [x8, #0]
39		mov x8, x8
40		mov x21, x8
41		// Stack pointer unchanged, no stack allocated arguments
42		mov x8, x21
43		mov x0, x8
44		// statement primitives do not return results (but will clobber r0/rax)
45		bl _printi
46		bl _println
47		// Stack pointer unchanged, no stack allocated variables
48		mov x0, #0
49		// pop {x19, x20, x21}
50		ldp x21, xzr, [sp, #16]
51		ldp x19, x20, [sp], #32
52		// pop {fp, lr}
53		ldp fp, lr, [sp], #16
54		ret
55	
56	// length of .L._prints_str0
57		.word 4
58	.L._prints_str0:
59		.asciz "%.*s"
60	.align 4
61	_prints:
62		// push {lr}
63		stp lr, xzr, [sp, #-16]!
64		mov x2, x0
65		ldrsw x1, [x0, #-4]
66		adr x0, .L._prints_str0
67		bl printf
68		mov x0, #0
69		bl fflush
70		// pop {lr}
71		ldp lr, xzr, [sp], #16
72		ret
73	
74	_malloc:
75		// push {lr}
76		stp lr, xzr, [sp, #-16]!
77		bl malloc
78		cbz x0, _errOutOfMemory
79		// pop {lr}
80		ldp lr, xzr, [sp], #16
81		ret
82	
83	// length of .L._printi_str0
84		.word 2
85	.L._printi_str0:
86		.asciz "%d"
87	.align 4
88	_printi:
89		// push {lr}
90		stp lr, xzr, [sp, #-16]!
91		mov x1, x0
92		adr x0, .L._printi_str0
93		bl printf
94		mov x0, #0
95		bl fflush
96		// pop {lr}
97		ldp lr, xzr, [sp], #16
98		ret
99	
100	// length of .L._println_str0
101		.word 0
102	.L._println_str0:
103		.asciz ""
104	.align 4
105	_println:
106		// push {lr}
107		stp lr, xzr, [sp, #-16]!
108		adr x0, .L._println_str0
109		bl puts
110		mov x0, #0
111		bl fflush
112		// pop {lr}
113		ldp lr, xzr, [sp], #16
114		ret
115	
116	// length of .L._errOutOfMemory_str0
117		.word 27
118	.L._errOutOfMemory_str0:
119		.asciz "fatal error: out of memory\n"
120	.align 4
121	_errOutOfMemory:
122		adr x0, .L._errOutOfMemory_str0
123		bl _prints
124		mov w0, #-1
125		bl exit
126	
127	// length of .L._errNull_str0
128		.word 45
129	.L._errNull_str0:
130		.asciz "fatal error: null pair dereferenced or freed\n"
131	.align 4
132	_errNull:
133		adr x0, .L._errNull_str0
134		bl _prints
135		mov w0, #-1
136		bl exit
===========================================================
-- Finished

