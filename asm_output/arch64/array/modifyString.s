./valid/array/modifyString.wacc
calling the reference compiler on ./valid/array/modifyString.wacc
-- Test: modifyString.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create and modify string as array of characters

# Output:
# hello world!
# Hello world!
# Hi!
#

# Program:

begin
  char[] str = ['h','e','l','l','o',' ','w','o','r','l','d','!'] ;
  println str ;
  str[0] = 'H' ;
  println str ;
  str = ['H','i','!'] ;
  println str
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
modifyString.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19}
8		stp x19, xzr, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		// 12 element array
12		mov w0, #16
13		bl _malloc
14		mov x16, x0
15		// array pointers are shifted forwards by 4 bytes (to account for size)
16		add x16, x16, #4
17		mov x8, #12
18		str w8, [x16, #-4]
19		mov x8, #104
20		strb w8, [x16, #0]
21		mov x8, #101
22		strb w8, [x16, #1]
23		mov x8, #108
24		strb w8, [x16, #2]
25		mov x8, #108
26		strb w8, [x16, #3]
27		mov x8, #111
28		strb w8, [x16, #4]
29		mov x8, #32
30		strb w8, [x16, #5]
31		mov x8, #119
32		strb w8, [x16, #6]
33		mov x8, #111
34		strb w8, [x16, #7]
35		mov x8, #114
36		strb w8, [x16, #8]
37		mov x8, #108
38		strb w8, [x16, #9]
39		mov x8, #100
40		strb w8, [x16, #10]
41		mov x8, #33
42		strb w8, [x16, #11]
43		mov x8, x16
44		mov x19, x8
45		// Stack pointer unchanged, no stack allocated arguments
46		mov x8, x19
47		mov x0, x8
48		// statement primitives do not return results (but will clobber r0/rax)
49		bl _prints
50		bl _println
51		mov w17, #0
52		mov x8, #72
53		mov x7, x19
54		bl _arrStore1
55		// Stack pointer unchanged, no stack allocated arguments
56		mov x8, x19
57		mov x0, x8
58		// statement primitives do not return results (but will clobber r0/rax)
59		bl _prints
60		bl _println
61		// 3 element array
62		mov w0, #7
63		bl _malloc
64		mov x16, x0
65		// array pointers are shifted forwards by 4 bytes (to account for size)
66		add x16, x16, #4
67		mov x8, #3
68		str w8, [x16, #-4]
69		mov x8, #72
70		strb w8, [x16, #0]
71		mov x8, #105
72		strb w8, [x16, #1]
73		mov x8, #33
74		strb w8, [x16, #2]
75		mov x8, x16
76		mov x19, x8
77		// Stack pointer unchanged, no stack allocated arguments
78		mov x8, x19
79		mov x0, x8
80		// statement primitives do not return results (but will clobber r0/rax)
81		bl _prints
82		bl _println
83		// Stack pointer unchanged, no stack allocated variables
84		mov x0, #0
85		// pop {x19}
86		ldp x19, xzr, [sp], #16
87		// pop {fp, lr}
88		ldp fp, lr, [sp], #16
89		ret
90	
91	// length of .L._prints_str0
92		.word 4
93	.L._prints_str0:
94		.asciz "%.*s"
95	.align 4
96	_prints:
97		// push {lr}
98		stp lr, xzr, [sp, #-16]!
99		mov x2, x0
100		ldrsw x1, [x0, #-4]
101		adr x0, .L._prints_str0
102		bl printf
103		mov x0, #0
104		bl fflush
105		// pop {lr}
106		ldp lr, xzr, [sp], #16
107		ret
108	
109	_malloc:
110		// push {lr}
111		stp lr, xzr, [sp, #-16]!
112		bl malloc
113		cbz x0, _errOutOfMemory
114		// pop {lr}
115		ldp lr, xzr, [sp], #16
116		ret
117	
118	// length of .L._println_str0
119		.word 0
120	.L._println_str0:
121		.asciz ""
122	.align 4
123	_println:
124		// push {lr}
125		stp lr, xzr, [sp, #-16]!
126		adr x0, .L._println_str0
127		bl puts
128		mov x0, #0
129		bl fflush
130		// pop {lr}
131		ldp lr, xzr, [sp], #16
132		ret
133	
134	_arrStore1:
135		// Special calling convention: array ptr passed in X7, index in X17, value to store in X8, LR (W30) is used as general register
136		// push {lr}
137		stp lr, xzr, [sp, #-16]!
138		sxtw x17, w17
139		cmp w17, #0
140		csel x1, x17, x1, lt
141		b.lt _errOutOfBounds
142		ldrsw lr, [x7, #-4]
143		cmp w17, w30
144		csel x1, x17, x1, ge
145		b.ge _errOutOfBounds
146		strb w8, [x7, x17]
147		// pop {lr}
148		ldp lr, xzr, [sp], #16
149		ret
150	
151	// length of .L._errOutOfMemory_str0
152		.word 27
153	.L._errOutOfMemory_str0:
154		.asciz "fatal error: out of memory\n"
155	.align 4
156	_errOutOfMemory:
157		adr x0, .L._errOutOfMemory_str0
158		bl _prints
159		mov w0, #-1
160		bl exit
161	
162	// length of .L._errOutOfBounds_str0
163		.word 42
164	.L._errOutOfBounds_str0:
165		.asciz "fatal error: array index %d out of bounds\n"
166	.align 4
167	_errOutOfBounds:
168		adr x0, .L._errOutOfBounds_str0
169		bl printf
170		mov x0, #0
171		bl fflush
172		mov w0, #-1
173		bl exit
===========================================================
-- Finished

