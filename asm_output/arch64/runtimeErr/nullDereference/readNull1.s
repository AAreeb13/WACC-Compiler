./valid/runtimeErr/nullDereference/readNull1.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/readNull1.wacc
-- Test: readNull1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt dereference of a null pair by reading into an element of it

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	pair(int, int) p = null ;
	read fst p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
readNull1.s contents are:
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
11		mov x8, #0
12		mov x19, x8
13		// Stack pointer unchanged, no stack allocated arguments
14		// load the current value in the destination of the read so it supports defaults
15		cbz x19, _errNull
16		ldr x8, [x19, #0]
17		mov x8, x8
18		mov x0, x8
19		bl _readi
20		mov x16, x0
21		cbz x19, _errNull
22		mov x8, x16
23		str x8, [x19, #0]
24		// Stack pointer unchanged, no stack allocated variables
25		mov x0, #0
26		// pop {x19}
27		ldp x19, xzr, [sp], #16
28		// pop {fp, lr}
29		ldp fp, lr, [sp], #16
30		ret
31	
32	// length of .L._prints_str0
33		.word 4
34	.L._prints_str0:
35		.asciz "%.*s"
36	.align 4
37	_prints:
38		// push {lr}
39		stp lr, xzr, [sp, #-16]!
40		mov x2, x0
41		ldrsw x1, [x0, #-4]
42		adr x0, .L._prints_str0
43		bl printf
44		mov x0, #0
45		bl fflush
46		// pop {lr}
47		ldp lr, xzr, [sp], #16
48		ret
49	
50	// length of .L._readi_str0
51		.word 2
52	.L._readi_str0:
53		.asciz "%d"
54	.align 4
55	_readi:
56		// X0 contains the "original" value of the destination of the read
57		// allocate space on the stack to store the read: preserve alignment!
58		// the passed default argument should be stored in case of EOF
59		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
60		// push {x0, lr}
61		stp x0, lr, [sp, #-16]!
62		mov x1, sp
63		adr x0, .L._readi_str0
64		bl scanf
65		// pop {x0, lr}
66		ldp x0, lr, [sp], #16
67		ret
68	
69	// length of .L._errNull_str0
70		.word 45
71	.L._errNull_str0:
72		.asciz "fatal error: null pair dereferenced or freed\n"
73	.align 4
74	_errNull:
75		adr x0, .L._errNull_str0
76		bl _prints
77		mov w0, #-1
78		bl exit
===========================================================
-- Finished

