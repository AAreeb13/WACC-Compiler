./valid/runtimeErr/nullDereference/useNull2.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/useNull2.wacc
-- Test: useNull2.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt dereference of a null pair by using it

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	pair(int, int) p = null ;
	int x = snd p
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
useNull2.s contents are:
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
11		mov x8, #0
12		mov x19, x8
13		cbz x19, _errNull
14		ldr x8, [x19, #8]
15		mov x8, x8
16		mov x20, x8
17		// Stack pointer unchanged, no stack allocated variables
18		mov x0, #0
19		// pop {x19, x20}
20		ldp x19, x20, [sp], #16
21		// pop {fp, lr}
22		ldp fp, lr, [sp], #16
23		ret
24	
25	// length of .L._prints_str0
26		.word 4
27	.L._prints_str0:
28		.asciz "%.*s"
29	.align 4
30	_prints:
31		// push {lr}
32		stp lr, xzr, [sp, #-16]!
33		mov x2, x0
34		ldrsw x1, [x0, #-4]
35		adr x0, .L._prints_str0
36		bl printf
37		mov x0, #0
38		bl fflush
39		// pop {lr}
40		ldp lr, xzr, [sp], #16
41		ret
42	
43	// length of .L._errNull_str0
44		.word 45
45	.L._errNull_str0:
46		.asciz "fatal error: null pair dereferenced or freed\n"
47	.align 4
48	_errNull:
49		adr x0, .L._errNull_str0
50		bl _prints
51		mov w0, #-1
52		bl exit
===========================================================
-- Finished

