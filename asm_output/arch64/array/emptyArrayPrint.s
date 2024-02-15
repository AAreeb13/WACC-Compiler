./valid/array/emptyArrayPrint.wacc
calling the reference compiler on ./valid/array/emptyArrayPrint.wacc
-- Test: emptyArrayPrint.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array declaration and print after

# Output:
# true
#

# Program:

begin
  int[] x = [] ;
  println true
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayPrint.s contents are:
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
21		// Stack pointer unchanged, no stack allocated arguments
22		mov x8, #1
23		mov x0, x8
24		// statement primitives do not return results (but will clobber r0/rax)
25		bl _printb
26		bl _println
27		// Stack pointer unchanged, no stack allocated variables
28		mov x0, #0
29		// pop {x19}
30		ldp x19, xzr, [sp], #16
31		// pop {fp, lr}
32		ldp fp, lr, [sp], #16
33		ret
34	
35	// length of .L._prints_str0
36		.word 4
37	.L._prints_str0:
38		.asciz "%.*s"
39	.align 4
40	_prints:
41		// push {lr}
42		stp lr, xzr, [sp, #-16]!
43		mov x2, x0
44		ldrsw x1, [x0, #-4]
45		adr x0, .L._prints_str0
46		bl printf
47		mov x0, #0
48		bl fflush
49		// pop {lr}
50		ldp lr, xzr, [sp], #16
51		ret
52	
53	_malloc:
54		// push {lr}
55		stp lr, xzr, [sp, #-16]!
56		bl malloc
57		cbz x0, _errOutOfMemory
58		// pop {lr}
59		ldp lr, xzr, [sp], #16
60		ret
61	
62	// length of .L._printb_str0
63		.word 5
64	.L._printb_str0:
65		.asciz "false"
66	// length of .L._printb_str1
67		.word 4
68	.L._printb_str1:
69		.asciz "true"
70	// length of .L._printb_str2
71		.word 4
72	.L._printb_str2:
73		.asciz "%.*s"
74	.align 4
75	_printb:
76		// push {lr}
77		stp lr, xzr, [sp, #-16]!
78		cmp w0, #0
79		b.ne .L_printb0
80		adr x2, .L._printb_str0
81		b .L_printb1
82	.L_printb0:
83		adr x2, .L._printb_str1
84	.L_printb1:
85		ldrsw x1, [x2, #-4]
86		adr x0, .L._printb_str2
87		bl printf
88		mov x0, #0
89		bl fflush
90		// pop {lr}
91		ldp lr, xzr, [sp], #16
92		ret
93	
94	// length of .L._println_str0
95		.word 0
96	.L._println_str0:
97		.asciz ""
98	.align 4
99	_println:
100		// push {lr}
101		stp lr, xzr, [sp, #-16]!
102		adr x0, .L._println_str0
103		bl puts
104		mov x0, #0
105		bl fflush
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

