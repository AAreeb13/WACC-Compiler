./valid/array/emptyArrayReplace.wacc
calling the reference compiler on ./valid/array/emptyArrayReplace.wacc
-- Test: emptyArrayReplace.wacc

-- Uploaded file: 
---------------------------------------------------------------
# it should be possible to reassign to an array

# Output:
# true
#

# Program:

begin
  int[] x = [] ;
  x = [0] ;
  println true
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayReplace.s contents are:
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
11		// 0 element array
12		mov w0, #4
13		bl _malloc
14		mov x16, x0
15		// array pointers are shifted forwards by 4 bytes (to account for size)
16		add x16, x16, #4
17		mov x8, #0
18		str w8, [x16, #-4]
19		mov x8, x16
20		mov x19, x8
21		// 1 element array
22		mov w0, #8
23		bl _malloc
24		mov x16, x0
25		// array pointers are shifted forwards by 4 bytes (to account for size)
26		add x16, x16, #4
27		mov x8, #1
28		str w8, [x16, #-4]
29		mov x8, #0
30		str w8, [x16, #0]
31		mov x8, x16
32		mov x19, x8
33		// Stack pointer unchanged, no stack allocated arguments
34		mov x8, #1
35		mov x0, x8
36		// statement primitives do not return results (but will clobber r0/rax)
37		bl _printb
38		bl _println
39		// Stack pointer unchanged, no stack allocated variables
40		mov x0, #0
41		// pop {x19}
42		ldp x19, xzr, [sp], #16
43		// pop {fp, lr}
44		ldp fp, lr, [sp], #16
45		ret
46	
47	// length of .L._prints_str0
48		.word 4
49	.L._prints_str0:
50		.asciz "%.*s"
51	.align 4
52	_prints:
53		// push {lr}
54		stp lr, xzr, [sp, #-16]!
55		mov x2, x0
56		ldrsw x1, [x0, #-4]
57		adr x0, .L._prints_str0
58		bl printf
59		mov x0, #0
60		bl fflush
61		// pop {lr}
62		ldp lr, xzr, [sp], #16
63		ret
64	
65	_malloc:
66		// push {lr}
67		stp lr, xzr, [sp, #-16]!
68		bl malloc
69		cbz x0, _errOutOfMemory
70		// pop {lr}
71		ldp lr, xzr, [sp], #16
72		ret
73	
74	// length of .L._printb_str0
75		.word 5
76	.L._printb_str0:
77		.asciz "false"
78	// length of .L._printb_str1
79		.word 4
80	.L._printb_str1:
81		.asciz "true"
82	// length of .L._printb_str2
83		.word 4
84	.L._printb_str2:
85		.asciz "%.*s"
86	.align 4
87	_printb:
88		// push {lr}
89		stp lr, xzr, [sp, #-16]!
90		cmp w0, #0
91		b.ne .L_printb0
92		adr x2, .L._printb_str0
93		b .L_printb1
94	.L_printb0:
95		adr x2, .L._printb_str1
96	.L_printb1:
97		ldrsw x1, [x2, #-4]
98		adr x0, .L._printb_str2
99		bl printf
100		mov x0, #0
101		bl fflush
102		// pop {lr}
103		ldp lr, xzr, [sp], #16
104		ret
105	
106	// length of .L._println_str0
107		.word 0
108	.L._println_str0:
109		.asciz ""
110	.align 4
111	_println:
112		// push {lr}
113		stp lr, xzr, [sp, #-16]!
114		adr x0, .L._println_str0
115		bl puts
116		mov x0, #0
117		bl fflush
118		// pop {lr}
119		ldp lr, xzr, [sp], #16
120		ret
121	
122	// length of .L._errOutOfMemory_str0
123		.word 27
124	.L._errOutOfMemory_str0:
125		.asciz "fatal error: out of memory\n"
126	.align 4
127	_errOutOfMemory:
128		adr x0, .L._errOutOfMemory_str0
129		bl _prints
130		mov w0, #-1
131		bl exit
===========================================================
-- Finished

