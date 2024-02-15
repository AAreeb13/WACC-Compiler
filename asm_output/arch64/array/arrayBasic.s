./valid/array/arrayBasic.wacc
calling the reference compiler on ./valid/array/arrayBasic.wacc
-- Test: arrayBasic.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic array declaration and assignment

# Output:

# Program:

begin
  int[] a = [0]
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayBasic.s contents are:
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
11		// 1 element array
12		mov w0, #8
13		bl _malloc
14		mov x16, x0
15		// array pointers are shifted forwards by 4 bytes (to account for size)
16		add x16, x16, #4
17		mov x8, #1
18		str w8, [x16, #-4]
19		mov x8, #0
20		str w8, [x16, #0]
21		mov x8, x16
22		mov x19, x8
23		// Stack pointer unchanged, no stack allocated variables
24		mov x0, #0
25		// pop {x19}
26		ldp x19, xzr, [sp], #16
27		// pop {fp, lr}
28		ldp fp, lr, [sp], #16
29		ret
30	
31	// length of .L._prints_str0
32		.word 4
33	.L._prints_str0:
34		.asciz "%.*s"
35	.align 4
36	_prints:
37		// push {lr}
38		stp lr, xzr, [sp, #-16]!
39		mov x2, x0
40		ldrsw x1, [x0, #-4]
41		adr x0, .L._prints_str0
42		bl printf
43		mov x0, #0
44		bl fflush
45		// pop {lr}
46		ldp lr, xzr, [sp], #16
47		ret
48	
49	_malloc:
50		// push {lr}
51		stp lr, xzr, [sp, #-16]!
52		bl malloc
53		cbz x0, _errOutOfMemory
54		// pop {lr}
55		ldp lr, xzr, [sp], #16
56		ret
57	
58	// length of .L._errOutOfMemory_str0
59		.word 27
60	.L._errOutOfMemory_str0:
61		.asciz "fatal error: out of memory\n"
62	.align 4
63	_errOutOfMemory:
64		adr x0, .L._errOutOfMemory_str0
65		bl _prints
66		mov w0, #-1
67		bl exit
===========================================================
-- Finished

