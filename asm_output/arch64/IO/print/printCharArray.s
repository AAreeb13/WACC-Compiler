./valid/IO/print/printCharArray.wacc
calling the reference compiler on ./valid/IO/print/printCharArray.wacc
-- Test: printCharArray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# printing the contents of a char[] is possible via an intermediate variable

# Output:
# hi!
#

# Exit:
# 0

# Program:

begin
  char[] s = ['h','i','!'];
  println s
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printCharArray.s contents are:
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
19		mov x8, #104
20		strb w8, [x16, #0]
21		mov x8, #105
22		strb w8, [x16, #1]
23		mov x8, #33
24		strb w8, [x16, #2]
25		mov x8, x16
26		mov x19, x8
27		// Stack pointer unchanged, no stack allocated arguments
28		mov x8, x19
29		mov x0, x8
30		// statement primitives do not return results (but will clobber r0/rax)
31		bl _prints
32		bl _println
33		// Stack pointer unchanged, no stack allocated variables
34		mov x0, #0
35		// pop {x19}
36		ldp x19, xzr, [sp], #16
37		// pop {fp, lr}
38		ldp fp, lr, [sp], #16
39		ret
40	
41	// length of .L._prints_str0
42		.word 4
43	.L._prints_str0:
44		.asciz "%.*s"
45	.align 4
46	_prints:
47		// push {lr}
48		stp lr, xzr, [sp, #-16]!
49		mov x2, x0
50		ldrsw x1, [x0, #-4]
51		adr x0, .L._prints_str0
52		bl printf
53		mov x0, #0
54		bl fflush
55		// pop {lr}
56		ldp lr, xzr, [sp], #16
57		ret
58	
59	_malloc:
60		// push {lr}
61		stp lr, xzr, [sp, #-16]!
62		bl malloc
63		cbz x0, _errOutOfMemory
64		// pop {lr}
65		ldp lr, xzr, [sp], #16
66		ret
67	
68	// length of .L._println_str0
69		.word 0
70	.L._println_str0:
71		.asciz ""
72	.align 4
73	_println:
74		// push {lr}
75		stp lr, xzr, [sp, #-16]!
76		adr x0, .L._println_str0
77		bl puts
78		mov x0, #0
79		bl fflush
80		// pop {lr}
81		ldp lr, xzr, [sp], #16
82		ret
83	
84	// length of .L._errOutOfMemory_str0
85		.word 27
86	.L._errOutOfMemory_str0:
87		.asciz "fatal error: out of memory\n"
88	.align 4
89	_errOutOfMemory:
90		adr x0, .L._errOutOfMemory_str0
91		bl _prints
92		mov w0, #-1
93		bl exit
===========================================================
-- Finished

