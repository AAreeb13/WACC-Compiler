./valid/pairs/printPair.wacc
calling the reference compiler on ./valid/pairs/printPair.wacc
-- Test: printPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print pair program

# Output:
# #addrs# = (10, a)
#

# Program:

begin
  pair(int, char) p = newpair(10, 'a') ;
  print p ;
  print " = (" ;
  int x = fst p ;
  print x ;
  print ", " ;
  char c = snd p ;
  print c ;
  println ')'
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printPair.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 4
3	.L.str0:
4		.asciz " = ("
5	// length of .L.str1
6		.word 2
7	.L.str1:
8		.asciz ", "
9	.align 4
10	.text
11	.global main
12	main:
13		// push {fp, lr}
14		stp fp, lr, [sp, #-16]!
15		// push {x19, x20, x21}
16		stp x19, x20, [sp, #-32]!
17		stp x21, xzr, [sp, #16]
18		mov fp, sp
19		// Stack pointer unchanged, no stack allocated variables
20		mov w0, #16
21		bl _malloc
22		mov x16, x0
23		mov x8, #10
24		str x8, [x16, #0]
25		mov x8, #97
26		str x8, [x16, #8]
27		mov x8, x16
28		mov x19, x8
29		// Stack pointer unchanged, no stack allocated arguments
30		mov x8, x19
31		mov x0, x8
32		// statement primitives do not return results (but will clobber r0/rax)
33		bl _printp
34		// Stack pointer unchanged, no stack allocated arguments
35		adrp x8, .L.str0
36		add x8, x8, :lo12:.L.str0
37		// push {x8}
38		stp x8, xzr, [sp, #-16]!
39		// pop {x8}
40		ldp x8, xzr, [sp], #16
41		mov x8, x8
42		mov x0, x8
43		// statement primitives do not return results (but will clobber r0/rax)
44		bl _prints
45		cbz x19, _errNull
46		ldr x8, [x19, #0]
47		mov x8, x8
48		mov x20, x8
49		// Stack pointer unchanged, no stack allocated arguments
50		mov x8, x20
51		mov x0, x8
52		// statement primitives do not return results (but will clobber r0/rax)
53		bl _printi
54		// Stack pointer unchanged, no stack allocated arguments
55		adrp x8, .L.str1
56		add x8, x8, :lo12:.L.str1
57		// push {x8}
58		stp x8, xzr, [sp, #-16]!
59		// pop {x8}
60		ldp x8, xzr, [sp], #16
61		mov x8, x8
62		mov x0, x8
63		// statement primitives do not return results (but will clobber r0/rax)
64		bl _prints
65		cbz x19, _errNull
66		ldr x8, [x19, #8]
67		mov x8, x8
68		mov x21, x8
69		// Stack pointer unchanged, no stack allocated arguments
70		mov x8, x21
71		mov x0, x8
72		// statement primitives do not return results (but will clobber r0/rax)
73		bl _printc
74		// Stack pointer unchanged, no stack allocated arguments
75		mov x8, #41
76		mov x0, x8
77		// statement primitives do not return results (but will clobber r0/rax)
78		bl _printc
79		bl _println
80		// Stack pointer unchanged, no stack allocated variables
81		mov x0, #0
82		// pop {x19, x20, x21}
83		ldp x21, xzr, [sp, #16]
84		ldp x19, x20, [sp], #32
85		// pop {fp, lr}
86		ldp fp, lr, [sp], #16
87		ret
88	
89	// length of .L._prints_str0
90		.word 4
91	.L._prints_str0:
92		.asciz "%.*s"
93	.align 4
94	_prints:
95		// push {lr}
96		stp lr, xzr, [sp, #-16]!
97		mov x2, x0
98		ldrsw x1, [x0, #-4]
99		adr x0, .L._prints_str0
100		bl printf
101		mov x0, #0
102		bl fflush
103		// pop {lr}
104		ldp lr, xzr, [sp], #16
105		ret
106	
107	// length of .L._printp_str0
108		.word 2
109	.L._printp_str0:
110		.asciz "%p"
111	.align 4
112	_printp:
113		// push {lr}
114		stp lr, xzr, [sp, #-16]!
115		mov x1, x0
116		adr x0, .L._printp_str0
117		bl printf
118		mov x0, #0
119		bl fflush
120		// pop {lr}
121		ldp lr, xzr, [sp], #16
122		ret
123	
124	_malloc:
125		// push {lr}
126		stp lr, xzr, [sp, #-16]!
127		bl malloc
128		cbz x0, _errOutOfMemory
129		// pop {lr}
130		ldp lr, xzr, [sp], #16
131		ret
132	
133	// length of .L._printi_str0
134		.word 2
135	.L._printi_str0:
136		.asciz "%d"
137	.align 4
138	_printi:
139		// push {lr}
140		stp lr, xzr, [sp, #-16]!
141		mov x1, x0
142		adr x0, .L._printi_str0
143		bl printf
144		mov x0, #0
145		bl fflush
146		// pop {lr}
147		ldp lr, xzr, [sp], #16
148		ret
149	
150	// length of .L._printc_str0
151		.word 2
152	.L._printc_str0:
153		.asciz "%c"
154	.align 4
155	_printc:
156		// push {lr}
157		stp lr, xzr, [sp, #-16]!
158		mov x1, x0
159		adr x0, .L._printc_str0
160		bl printf
161		mov x0, #0
162		bl fflush
163		// pop {lr}
164		ldp lr, xzr, [sp], #16
165		ret
166	
167	// length of .L._println_str0
168		.word 0
169	.L._println_str0:
170		.asciz ""
171	.align 4
172	_println:
173		// push {lr}
174		stp lr, xzr, [sp, #-16]!
175		adr x0, .L._println_str0
176		bl puts
177		mov x0, #0
178		bl fflush
179		// pop {lr}
180		ldp lr, xzr, [sp], #16
181		ret
182	
183	// length of .L._errOutOfMemory_str0
184		.word 27
185	.L._errOutOfMemory_str0:
186		.asciz "fatal error: out of memory\n"
187	.align 4
188	_errOutOfMemory:
189		adr x0, .L._errOutOfMemory_str0
190		bl _prints
191		mov w0, #-1
192		bl exit
193	
194	// length of .L._errNull_str0
195		.word 45
196	.L._errNull_str0:
197		.asciz "fatal error: null pair dereferenced or freed\n"
198	.align 4
199	_errNull:
200		adr x0, .L._errNull_str0
201		bl _prints
202		mov w0, #-1
203		bl exit
===========================================================
-- Finished

