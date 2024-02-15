./valid/array/arrayLength.wacc
calling the reference compiler on ./valid/array/arrayLength.wacc
-- Test: arrayLength.wacc

-- Uploaded file: 
---------------------------------------------------------------
# check length of array

# Output:
# 4
#

# Program:

begin
  int[] a = [43, 2, 18, 1] ;
  println len a
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayLength.s contents are:
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
11		// 4 element array
12		mov w0, #20
13		bl _malloc
14		mov x16, x0
15		// array pointers are shifted forwards by 4 bytes (to account for size)
16		add x16, x16, #4
17		mov x8, #4
18		str w8, [x16, #-4]
19		mov x8, #43
20		str w8, [x16, #0]
21		mov x8, #2
22		str w8, [x16, #4]
23		mov x8, #18
24		str w8, [x16, #8]
25		mov x8, #1
26		str w8, [x16, #12]
27		mov x8, x16
28		mov x19, x8
29		// Stack pointer unchanged, no stack allocated arguments
30		ldrsw x8, [x19, #-4]
31		mov x8, x8
32		mov x0, x8
33		// statement primitives do not return results (but will clobber r0/rax)
34		bl _printi
35		bl _println
36		// Stack pointer unchanged, no stack allocated variables
37		mov x0, #0
38		// pop {x19}
39		ldp x19, xzr, [sp], #16
40		// pop {fp, lr}
41		ldp fp, lr, [sp], #16
42		ret
43	
44	// length of .L._prints_str0
45		.word 4
46	.L._prints_str0:
47		.asciz "%.*s"
48	.align 4
49	_prints:
50		// push {lr}
51		stp lr, xzr, [sp, #-16]!
52		mov x2, x0
53		ldrsw x1, [x0, #-4]
54		adr x0, .L._prints_str0
55		bl printf
56		mov x0, #0
57		bl fflush
58		// pop {lr}
59		ldp lr, xzr, [sp], #16
60		ret
61	
62	_malloc:
63		// push {lr}
64		stp lr, xzr, [sp, #-16]!
65		bl malloc
66		cbz x0, _errOutOfMemory
67		// pop {lr}
68		ldp lr, xzr, [sp], #16
69		ret
70	
71	// length of .L._printi_str0
72		.word 2
73	.L._printi_str0:
74		.asciz "%d"
75	.align 4
76	_printi:
77		// push {lr}
78		stp lr, xzr, [sp, #-16]!
79		mov x1, x0
80		adr x0, .L._printi_str0
81		bl printf
82		mov x0, #0
83		bl fflush
84		// pop {lr}
85		ldp lr, xzr, [sp], #16
86		ret
87	
88	// length of .L._println_str0
89		.word 0
90	.L._println_str0:
91		.asciz ""
92	.align 4
93	_println:
94		// push {lr}
95		stp lr, xzr, [sp, #-16]!
96		adr x0, .L._println_str0
97		bl puts
98		mov x0, #0
99		bl fflush
100		// pop {lr}
101		ldp lr, xzr, [sp], #16
102		ret
103	
104	// length of .L._errOutOfMemory_str0
105		.word 27
106	.L._errOutOfMemory_str0:
107		.asciz "fatal error: out of memory\n"
108	.align 4
109	_errOutOfMemory:
110		adr x0, .L._errOutOfMemory_str0
111		bl _prints
112		mov w0, #-1
113		bl exit
===========================================================
-- Finished

