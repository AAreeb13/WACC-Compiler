./valid/pairs/free.wacc
calling the reference compiler on ./valid/pairs/free.wacc
-- Test: free.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Create and free a simple pair

# Output:

# Program:

begin
  pair(int, char) a = newpair(10, 'a') ;
  free a
end

---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
free.s contents are:
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
11		mov w0, #16
12		bl _malloc
13		mov x16, x0
14		mov x8, #10
15		str x8, [x16, #0]
16		mov x8, #97
17		str x8, [x16, #8]
18		mov x8, x16
19		mov x19, x8
20		// Stack pointer unchanged, no stack allocated arguments
21		mov x8, x19
22		mov x0, x8
23		// statement primitives do not return results (but will clobber r0/rax)
24		bl _freepair
25		// Stack pointer unchanged, no stack allocated variables
26		mov x0, #0
27		// pop {x19}
28		ldp x19, xzr, [sp], #16
29		// pop {fp, lr}
30		ldp fp, lr, [sp], #16
31		ret
32	
33	_freepair:
34		// push {lr}
35		stp lr, xzr, [sp, #-16]!
36		cbz x0, _errNull
37		bl free
38		// pop {lr}
39		ldp lr, xzr, [sp], #16
40		ret
41	
42	// length of .L._prints_str0
43		.word 4
44	.L._prints_str0:
45		.asciz "%.*s"
46	.align 4
47	_prints:
48		// push {lr}
49		stp lr, xzr, [sp, #-16]!
50		mov x2, x0
51		ldrsw x1, [x0, #-4]
52		adr x0, .L._prints_str0
53		bl printf
54		mov x0, #0
55		bl fflush
56		// pop {lr}
57		ldp lr, xzr, [sp], #16
58		ret
59	
60	_malloc:
61		// push {lr}
62		stp lr, xzr, [sp, #-16]!
63		bl malloc
64		cbz x0, _errOutOfMemory
65		// pop {lr}
66		ldp lr, xzr, [sp], #16
67		ret
68	
69	// length of .L._errOutOfMemory_str0
70		.word 27
71	.L._errOutOfMemory_str0:
72		.asciz "fatal error: out of memory\n"
73	.align 4
74	_errOutOfMemory:
75		adr x0, .L._errOutOfMemory_str0
76		bl _prints
77		mov w0, #-1
78		bl exit
79	
80	// length of .L._errNull_str0
81		.word 45
82	.L._errNull_str0:
83		.asciz "fatal error: null pair dereferenced or freed\n"
84	.align 4
85	_errNull:
86		adr x0, .L._errNull_str0
87		bl _prints
88		mov w0, #-1
89		bl exit
===========================================================
-- Finished

