./valid/runtimeErr/nullDereference/freeNull.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/freeNull.wacc
-- Test: freeNull.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Create and free a null pair

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
  pair(pair, pair) a = null ;
  free a
end

---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
freeNull.s contents are:
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
14		mov x8, x19
15		mov x0, x8
16		// statement primitives do not return results (but will clobber r0/rax)
17		bl _freepair
18		// Stack pointer unchanged, no stack allocated variables
19		mov x0, #0
20		// pop {x19}
21		ldp x19, xzr, [sp], #16
22		// pop {fp, lr}
23		ldp fp, lr, [sp], #16
24		ret
25	
26	_freepair:
27		// push {lr}
28		stp lr, xzr, [sp, #-16]!
29		cbz x0, _errNull
30		bl free
31		// pop {lr}
32		ldp lr, xzr, [sp], #16
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
53	// length of .L._errNull_str0
54		.word 45
55	.L._errNull_str0:
56		.asciz "fatal error: null pair dereferenced or freed\n"
57	.align 4
58	_errNull:
59		adr x0, .L._errNull_str0
60		bl _prints
61		mov w0, #-1
62		bl exit
===========================================================
-- Finished

