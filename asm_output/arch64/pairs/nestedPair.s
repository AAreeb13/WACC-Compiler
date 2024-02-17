./valid/pairs/nestedPair.wacc
calling the reference compiler on ./valid/pairs/nestedPair.wacc
-- Test: nestedPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair (int, pair (int, int) )

# Output:

# Program:

begin
  pair(int, int) p = newpair(2, 3) ;
  pair(int, pair) q = newpair(1, p)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
nestedPair.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19, x20}
8		stp x19, x20, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		mov w0, #16
12		bl _malloc
13		mov x16, x0
14		mov x8, #2
15		str x8, [x16, #0]
16		mov x8, #3
17		str x8, [x16, #8]
18		mov x8, x16
19		mov x19, x8
20		mov w0, #16
21		bl _malloc
22		mov x16, x0
23		mov x8, #1
24		str x8, [x16, #0]
25		mov x8, x19
26		str x8, [x16, #8]
27		mov x8, x16
28		mov x20, x8
29		// Stack pointer unchanged, no stack allocated variables
30		mov x0, #0
31		// pop {x19, x20}
32		ldp x19, x20, [sp], #16
33		// pop {fp, lr}
34		ldp fp, lr, [sp], #16
35		ret
36	
37	// length of .L._prints_str0
38		.word 4
39	.L._prints_str0:
40		.asciz "%.*s"
41	.align 4
42	_prints:
43		// push {lr}
44		stp lr, xzr, [sp, #-16]!
45		mov x2, x0
46		ldrsw x1, [x0, #-4]
47		adr x0, .L._prints_str0
48		bl printf
49		mov x0, #0
50		bl fflush
51		// pop {lr}
52		ldp lr, xzr, [sp], #16
53		ret
54	
55	_malloc:
56		// push {lr}
57		stp lr, xzr, [sp, #-16]!
58		bl malloc
59		cbz x0, _errOutOfMemory
60		// pop {lr}
61		ldp lr, xzr, [sp], #16
62		ret
63	
64	// length of .L._errOutOfMemory_str0
65		.word 27
66	.L._errOutOfMemory_str0:
67		.asciz "fatal error: out of memory\n"
68	.align 4
69	_errOutOfMemory:
70		adr x0, .L._errOutOfMemory_str0
71		bl _prints
72		mov w0, #-1
73		bl exit
===========================================================
-- Finished

