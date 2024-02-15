./valid/pairs/printPairOfNulls.wacc
calling the reference compiler on ./valid/pairs/printPairOfNulls.wacc
-- Test: printPairOfNulls.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print a pair of null pairs

# Output:
# #addrs# = ((nil),(nil))
#

# Program:

begin
  pair(pair, pair) p = newpair(null, null) ;
  print p ;
  print " = (" ;
  pair(pair, pair) q = fst p ;
  print q ;
  print "," ;
  pair(int, bool) r = snd p ;
  print r ;
  println ")"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printPairOfNulls.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 4
3	.L.str0:
4		.asciz " = ("
5	// length of .L.str1
6		.word 1
7	.L.str1:
8		.asciz ","
9	// length of .L.str2
10		.word 1
11	.L.str2:
12		.asciz ")"
13	.align 4
14	.text
15	.global main
16	main:
17		// push {fp, lr}
18		stp fp, lr, [sp, #-16]!
19		// push {x19, x20, x21}
20		stp x19, x20, [sp, #-32]!
21		stp x21, xzr, [sp, #16]
22		mov fp, sp
23		// Stack pointer unchanged, no stack allocated variables
24		mov w0, #16
25		bl _malloc
26		mov x16, x0
27		mov x8, #0
28		str x8, [x16, #0]
29		mov x8, #0
30		str x8, [x16, #8]
31		mov x8, x16
32		mov x19, x8
33		// Stack pointer unchanged, no stack allocated arguments
34		mov x8, x19
35		mov x0, x8
36		// statement primitives do not return results (but will clobber r0/rax)
37		bl _printp
38		// Stack pointer unchanged, no stack allocated arguments
39		adrp x8, .L.str0
40		add x8, x8, :lo12:.L.str0
41		// push {x8}
42		stp x8, xzr, [sp, #-16]!
43		// pop {x8}
44		ldp x8, xzr, [sp], #16
45		mov x8, x8
46		mov x0, x8
47		// statement primitives do not return results (but will clobber r0/rax)
48		bl _prints
49		cbz x19, _errNull
50		ldr x8, [x19, #0]
51		mov x8, x8
52		mov x20, x8
53		// Stack pointer unchanged, no stack allocated arguments
54		mov x8, x20
55		mov x0, x8
56		// statement primitives do not return results (but will clobber r0/rax)
57		bl _printp
58		// Stack pointer unchanged, no stack allocated arguments
59		adrp x8, .L.str1
60		add x8, x8, :lo12:.L.str1
61		// push {x8}
62		stp x8, xzr, [sp, #-16]!
63		// pop {x8}
64		ldp x8, xzr, [sp], #16
65		mov x8, x8
66		mov x0, x8
67		// statement primitives do not return results (but will clobber r0/rax)
68		bl _prints
69		cbz x19, _errNull
70		ldr x8, [x19, #8]
71		mov x8, x8
72		mov x21, x8
73		// Stack pointer unchanged, no stack allocated arguments
74		mov x8, x21
75		mov x0, x8
76		// statement primitives do not return results (but will clobber r0/rax)
77		bl _printp
78		// Stack pointer unchanged, no stack allocated arguments
79		adrp x8, .L.str2
80		add x8, x8, :lo12:.L.str2
81		// push {x8}
82		stp x8, xzr, [sp, #-16]!
83		// pop {x8}
84		ldp x8, xzr, [sp], #16
85		mov x8, x8
86		mov x0, x8
87		// statement primitives do not return results (but will clobber r0/rax)
88		bl _prints
89		bl _println
90		// Stack pointer unchanged, no stack allocated variables
91		mov x0, #0
92		// pop {x19, x20, x21}
93		ldp x21, xzr, [sp, #16]
94		ldp x19, x20, [sp], #32
95		// pop {fp, lr}
96		ldp fp, lr, [sp], #16
97		ret
98	
99	// length of .L._prints_str0
100		.word 4
101	.L._prints_str0:
102		.asciz "%.*s"
103	.align 4
104	_prints:
105		// push {lr}
106		stp lr, xzr, [sp, #-16]!
107		mov x2, x0
108		ldrsw x1, [x0, #-4]
109		adr x0, .L._prints_str0
110		bl printf
111		mov x0, #0
112		bl fflush
113		// pop {lr}
114		ldp lr, xzr, [sp], #16
115		ret
116	
117	// length of .L._printp_str0
118		.word 2
119	.L._printp_str0:
120		.asciz "%p"
121	.align 4
122	_printp:
123		// push {lr}
124		stp lr, xzr, [sp, #-16]!
125		mov x1, x0
126		adr x0, .L._printp_str0
127		bl printf
128		mov x0, #0
129		bl fflush
130		// pop {lr}
131		ldp lr, xzr, [sp], #16
132		ret
133	
134	_malloc:
135		// push {lr}
136		stp lr, xzr, [sp, #-16]!
137		bl malloc
138		cbz x0, _errOutOfMemory
139		// pop {lr}
140		ldp lr, xzr, [sp], #16
141		ret
142	
143	// length of .L._println_str0
144		.word 0
145	.L._println_str0:
146		.asciz ""
147	.align 4
148	_println:
149		// push {lr}
150		stp lr, xzr, [sp, #-16]!
151		adr x0, .L._println_str0
152		bl puts
153		mov x0, #0
154		bl fflush
155		// pop {lr}
156		ldp lr, xzr, [sp], #16
157		ret
158	
159	// length of .L._errOutOfMemory_str0
160		.word 27
161	.L._errOutOfMemory_str0:
162		.asciz "fatal error: out of memory\n"
163	.align 4
164	_errOutOfMemory:
165		adr x0, .L._errOutOfMemory_str0
166		bl _prints
167		mov w0, #-1
168		bl exit
169	
170	// length of .L._errNull_str0
171		.word 45
172	.L._errNull_str0:
173		.asciz "fatal error: null pair dereferenced or freed\n"
174	.align 4
175	_errNull:
176		adr x0, .L._errNull_str0
177		bl _prints
178		mov w0, #-1
179		bl exit
===========================================================
-- Finished

