./valid/array/charArrayInStringArray.wacc
calling the reference compiler on ./valid/array/charArrayInStringArray.wacc
-- Test: charArrayInStringArray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a char[] can fit into a string[]
# Thanks to Iurii Zamiatin for finding this

# Output:

# Program:

begin
    char[] testArr = ['a', 'b', 'c'];
    string[] arr = [testArr, "box", "foo", "bar"]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
charArrayInStringArray.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 3
3	.L.str0:
4		.asciz "box"
5	// length of .L.str1
6		.word 3
7	.L.str1:
8		.asciz "foo"
9	// length of .L.str2
10		.word 3
11	.L.str2:
12		.asciz "bar"
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
23		// 3 element array
24		mov w0, #7
25		bl _malloc
26		mov x16, x0
27		// array pointers are shifted forwards by 4 bytes (to account for size)
28		add x16, x16, #4
29		mov x8, #3
30		str w8, [x16, #-4]
31		mov x8, #97
32		strb w8, [x16, #0]
33		mov x8, #98
34		strb w8, [x16, #1]
35		mov x8, #99
36		strb w8, [x16, #2]
37		mov x8, x16
38		mov x19, x8
39		// 4 element array
40		mov w0, #36
41		bl _malloc
42		mov x16, x0
43		// array pointers are shifted forwards by 4 bytes (to account for size)
44		add x16, x16, #4
45		mov x8, #4
46		str w8, [x16, #-4]
47		mov x8, x19
48		str x8, [x16, #0]
49		adrp x8, .L.str0
50		add x8, x8, :lo12:.L.str0
51		// push {x8}
52		stp x8, xzr, [sp, #-16]!
53		// pop {x8}
54		ldp x8, xzr, [sp], #16
55		mov x8, x8
56		str x8, [x16, #8]
57		adrp x8, .L.str1
58		add x8, x8, :lo12:.L.str1
59		// push {x8}
60		stp x8, xzr, [sp, #-16]!
61		// pop {x8}
62		ldp x8, xzr, [sp], #16
63		mov x8, x8
64		str x8, [x16, #16]
65		adrp x8, .L.str2
66		add x8, x8, :lo12:.L.str2
67		// push {x8}
68		stp x8, xzr, [sp, #-16]!
69		// pop {x8}
70		ldp x8, xzr, [sp], #16
71		mov x8, x8
72		str x8, [x16, #24]
73		mov x8, x16
74		mov x20, x8
75		// Stack pointer unchanged, no stack allocated variables
76		mov x0, #0
77		// pop {x19, x20}
78		ldp x19, x20, [sp], #16
79		// pop {fp, lr}
80		ldp fp, lr, [sp], #16
81		ret
82	
83	// length of .L._prints_str0
84		.word 4
85	.L._prints_str0:
86		.asciz "%.*s"
87	.align 4
88	_prints:
89		// push {lr}
90		stp lr, xzr, [sp, #-16]!
91		mov x2, x0
92		ldrsw x1, [x0, #-4]
93		adr x0, .L._prints_str0
94		bl printf
95		mov x0, #0
96		bl fflush
97		// pop {lr}
98		ldp lr, xzr, [sp], #16
99		ret
100	
101	_malloc:
102		// push {lr}
103		stp lr, xzr, [sp, #-16]!
104		bl malloc
105		cbz x0, _errOutOfMemory
106		// pop {lr}
107		ldp lr, xzr, [sp], #16
108		ret
109	
110	// length of .L._errOutOfMemory_str0
111		.word 27
112	.L._errOutOfMemory_str0:
113		.asciz "fatal error: out of memory\n"
114	.align 4
115	_errOutOfMemory:
116		adr x0, .L._errOutOfMemory_str0
117		bl _prints
118		mov w0, #-1
119		bl exit
===========================================================
-- Finished

