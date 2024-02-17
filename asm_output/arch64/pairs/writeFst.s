./valid/pairs/writeFst.wacc
calling the reference compiler on ./valid/pairs/writeFst.wacc
-- Test: writeFst.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair and write to its first element

# Output:
# 10
# 42
#

# Program:

begin
  pair(int, char) p = newpair(10, 'a') ;
  int f = fst p ;
  println f ;
  fst p = 42 ;
  f = fst p ;
  println f
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
writeFst.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19, x20}
8		stp x19, x20, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		mov w0, #16
12		bl _malloc
13		mov x16, x0
14		mov x8, #10
15		str x8, [x16, #0]
16		mov x8, #97
17		str x8, [x16, #8]
18		mov x8, x16
19		mov x19, x8
20		cbz x19, _errNull
21		ldr x8, [x19, #0]
22		mov x8, x8
23		mov x20, x8
24		// Stack pointer unchanged, no stack allocated arguments
25		mov x8, x20
26		mov x0, x8
27		// statement primitives do not return results (but will clobber r0/rax)
28		bl _printi
29		bl _println
30		cbz x19, _errNull
31		mov x8, #42
32		str x8, [x19, #0]
33		cbz x19, _errNull
34		ldr x8, [x19, #0]
35		mov x8, x8
36		// push {x8}
37		stp x8, xzr, [sp, #-16]!
38		// pop {x8}
39		ldp x8, xzr, [sp], #16
40		mov x8, x8
41		mov x20, x8
42		// Stack pointer unchanged, no stack allocated arguments
43		mov x8, x20
44		mov x0, x8
45		// statement primitives do not return results (but will clobber r0/rax)
46		bl _printi
47		bl _println
48		// Stack pointer unchanged, no stack allocated variables
49		mov x0, #0
50		// pop {x19, x20}
51		ldp x19, x20, [sp], #16
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

