./valid/runtimeErr/nullDereference/setNull1.wacc
calling the reference compiler on ./valid/runtimeErr/nullDereference/setNull1.wacc
-- Test: setNull1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# attempt dereference of a null pair by setting an element of it

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
	pair(int, int) p = null ;
	fst p = 1
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
setNull1.s contents are:
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
13		cbz x19, _errNull
14		mov x8, #1
15		str x8, [x19, #0]
16		// Stack pointer unchanged, no stack allocated variables
17		mov x0, #0
18		// pop {x19}
19		ldp x19, xzr, [sp], #16
20		// pop {fp, lr}
21		ldp fp, lr, [sp], #16
22		ret
23	
24	// length of .L._prints_str0
25		.word 4
26	.L._prints_str0:
27		.asciz "%.*s"
28	.align 4
29	_prints:
30		// push {lr}
31		stp lr, xzr, [sp, #-16]!
32		mov x2, x0
33		ldrsw x1, [x0, #-4]
34		adr x0, .L._prints_str0
35		bl printf
36		mov x0, #0
37		bl fflush
38		// pop {lr}
39		ldp lr, xzr, [sp], #16
40		ret
41	
42	// length of .L._errNull_str0
43		.word 45
44	.L._errNull_str0:
45		.asciz "fatal error: null pair dereferenced or freed\n"
46	.align 4
47	_errNull:
48		adr x0, .L._errNull_str0
49		bl _prints
50		mov w0, #-1
51		bl exit
===========================================================
-- Finished

