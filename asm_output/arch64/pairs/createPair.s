./valid/pairs/createPair.wacc
calling the reference compiler on ./valid/pairs/createPair.wacc
-- Test: createPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair (int, int)

# Output:

# Program:

begin
  pair(int, int) p = newpair(10, 3)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
createPair.s contents are:
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
16		mov x8, #3
17		str x8, [x16, #8]
18		mov x8, x16
19		mov x19, x8
20		// Stack pointer unchanged, no stack allocated variables
21		mov x0, #0
22		// pop {x19}
23		ldp x19, xzr, [sp], #16
24		// pop {fp, lr}
25		ldp fp, lr, [sp], #16
26		ret
27	
28	// length of .L._prints_str0
29		.word 4
30	.L._prints_str0:
31		.asciz "%.*s"
32	.align 4
33	_prints:
34		// push {lr}
35		stp lr, xzr, [sp, #-16]!
36		mov x2, x0
37		ldrsw x1, [x0, #-4]
38		adr x0, .L._prints_str0
39		bl printf
40		mov x0, #0
41		bl fflush
42		// pop {lr}
43		ldp lr, xzr, [sp], #16
44		ret
45	
46	_malloc:
47		// push {lr}
48		stp lr, xzr, [sp, #-16]!
49		bl malloc
50		cbz x0, _errOutOfMemory
51		// pop {lr}
52		ldp lr, xzr, [sp], #16
53		ret
54	
55	// length of .L._errOutOfMemory_str0
56		.word 27
57	.L._errOutOfMemory_str0:
58		.asciz "fatal error: out of memory\n"
59	.align 4
60	_errOutOfMemory:
61		adr x0, .L._errOutOfMemory_str0
62		bl _prints
63		mov w0, #-1
64		bl exit
===========================================================
-- Finished

