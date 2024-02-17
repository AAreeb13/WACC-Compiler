./valid/runtimeErr/badChar/tooBigChr.wacc
calling the reference compiler on ./valid/runtimeErr/badChar/tooBigChr.wacc
-- Test: tooBigChr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# try to convert a too large number to character

# Output:
# #runtime_error#

# Exit:
# 255

# Program:

begin
    char c = chr 128
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
tooBigChr.s contents are:
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
11		mov x8, #128
12		mov x8, x8
13		tst x8, #0xffffffffffffff80
14		csel x1, x8, x1, ne
15		b.ne _errBadChar
16		// push {x8}
17		stp x8, xzr, [sp, #-16]!
18		// pop {x8}
19		ldp x8, xzr, [sp], #16
20		mov x8, x8
21		mov x19, x8
22		// Stack pointer unchanged, no stack allocated variables
23		mov x0, #0
24		// pop {x19}
25		ldp x19, xzr, [sp], #16
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
48	// length of .L._errBadChar_str0
49		.word 50
50	.L._errBadChar_str0:
51		.asciz "fatal error: int %d is not ascii character 0-127 \n"
52	.align 4
53	_errBadChar:
54		adr x0, .L._errBadChar_str0
55		bl printf
56		mov x0, #0
57		bl fflush
58		mov w0, #-1
59		bl exit
===========================================================
-- Finished

