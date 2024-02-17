./valid/IO/print/printCharAsString.wacc
calling the reference compiler on ./valid/IO/print/printCharAsString.wacc
-- Test: printCharAsString.wacc

-- Uploaded file: 
---------------------------------------------------------------
# character array treated as a string

# Output:
# foo
# bar
#

# Program:

begin
  char[] str = ['f','o','o'];
  println str;
  str = ['b','a','r'];
  println str
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printCharAsString.s contents are:
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
11		// 3 element array
12		mov w0, #7
13		bl _malloc
14		mov x16, x0
15		// array pointers are shifted forwards by 4 bytes (to account for size)
16		add x16, x16, #4
17		mov x8, #3
18		str w8, [x16, #-4]
19		mov x8, #102
20		strb w8, [x16, #0]
21		mov x8, #111
22		strb w8, [x16, #1]
23		mov x8, #111
24		strb w8, [x16, #2]
25		mov x8, x16
26		mov x19, x8
27		// Stack pointer unchanged, no stack allocated arguments
28		mov x8, x19
29		mov x0, x8
30		// statement primitives do not return results (but will clobber r0/rax)
31		bl _prints
32		bl _println
33		// 3 element array
34		mov w0, #7
35		bl _malloc
36		mov x16, x0
37		// array pointers are shifted forwards by 4 bytes (to account for size)
38		add x16, x16, #4
39		mov x8, #3
40		str w8, [x16, #-4]
41		mov x8, #98
42		strb w8, [x16, #0]
43		mov x8, #97
44		strb w8, [x16, #1]
45		mov x8, #114
46		strb w8, [x16, #2]
47		mov x8, x16
48		mov x19, x8
49		// Stack pointer unchanged, no stack allocated arguments
50		mov x8, x19
51		mov x0, x8
52		// statement primitives do not return results (but will clobber r0/rax)
53		bl _prints
54		bl _println
55		// Stack pointer unchanged, no stack allocated variables
56		mov x0, #0
57		// pop {x19}
58		ldp x19, xzr, [sp], #16
59		// pop {fp, lr}
60		ldp fp, lr, [sp], #16
61		ret
62	
63	// length of .L._prints_str0
64		.word 4
65	.L._prints_str0:
66		.asciz "%.*s"
67	.align 4
68	_prints:
69		// push {lr}
70		stp lr, xzr, [sp, #-16]!
71		mov x2, x0
72		ldrsw x1, [x0, #-4]
73		adr x0, .L._prints_str0
74		bl printf
75		mov x0, #0
76		bl fflush
77		// pop {lr}
78		ldp lr, xzr, [sp], #16
79		ret
80	
81	_malloc:
82		// push {lr}
83		stp lr, xzr, [sp, #-16]!
84		bl malloc
85		cbz x0, _errOutOfMemory
86		// pop {lr}
87		ldp lr, xzr, [sp], #16
88		ret
89	
90	// length of .L._println_str0
91		.word 0
92	.L._println_str0:
93		.asciz ""
94	.align 4
95	_println:
96		// push {lr}
97		stp lr, xzr, [sp, #-16]!
98		adr x0, .L._println_str0
99		bl puts
100		mov x0, #0
101		bl fflush
102		// pop {lr}
103		ldp lr, xzr, [sp], #16
104		ret
105	
106	// length of .L._errOutOfMemory_str0
107		.word 27
108	.L._errOutOfMemory_str0:
109		.asciz "fatal error: out of memory\n"
110	.align 4
111	_errOutOfMemory:
112		adr x0, .L._errOutOfMemory_str0
113		bl _prints
114		mov w0, #-1
115		bl exit
===========================================================
-- Finished

