./valid/pairs/createRefPair.wacc
calling the reference compiler on ./valid/pairs/createRefPair.wacc
-- Test: createRefPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# create a pair(int, char) with a second reference to it

# Output:

# Program:

begin
  pair(int, char) p = newpair(10, 'a') ;
  pair(int, char) q = p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
createRefPair.s contents are:
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
14		mov x8, #10
15		str x8, [x16, #0]
16		mov x8, #97
17		str x8, [x16, #8]
18		mov x8, x16
19		mov x19, x8
20		mov x8, x19
21		mov x20, x8
22		// Stack pointer unchanged, no stack allocated variables
23		mov x0, #0
24		// pop {x19, x20}
25		ldp x19, x20, [sp], #16
26		// pop {fp, lr}
27		ldp fp, lr, [sp], #16
28		ret
29	
30	// length of .L._prints_str0
31		.word 4
32	.L._prints_str0:
33		.asciz "%.*s"
34	.align 4
35	_prints:
36		// push {lr}
37		stp lr, xzr, [sp, #-16]!
38		mov x2, x0
39		ldrsw x1, [x0, #-4]
40		adr x0, .L._prints_str0
41		bl printf
42		mov x0, #0
43		bl fflush
44		// pop {lr}
45		ldp lr, xzr, [sp], #16
46		ret
47	
48	_malloc:
49		// push {lr}
50		stp lr, xzr, [sp, #-16]!
51		bl malloc
52		cbz x0, _errOutOfMemory
53		// pop {lr}
54		ldp lr, xzr, [sp], #16
55		ret
56	
57	// length of .L._errOutOfMemory_str0
58		.word 27
59	.L._errOutOfMemory_str0:
60		.asciz "fatal error: out of memory\n"
61	.align 4
62	_errOutOfMemory:
63		adr x0, .L._errOutOfMemory_str0
64		bl _prints
65		mov w0, #-1
66		bl exit
===========================================================
-- Finished

