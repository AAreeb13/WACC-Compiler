./valid/array/arrayEmpty.wacc
calling the reference compiler on ./valid/array/arrayEmpty.wacc
-- Test: arrayEmpty.wacc

-- Uploaded file: 
---------------------------------------------------------------
# empty array declaration

# Output:

# Program:

begin
  int[] a = []
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
arrayEmpty.s contents are:
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
11		// 0 element array
12		mov w0, #4
13		bl _malloc
14		mov x16, x0
15		// array pointers are shifted forwards by 4 bytes (to account for size)
16		add x16, x16, #4
17		mov x8, #0
18		str w8, [x16, #-4]
19		mov x8, x16
20		mov x19, x8
21		// Stack pointer unchanged, no stack allocated variables
22		mov x0, #0
23		// pop {x19}
24		ldp x19, xzr, [sp], #16
25		// pop {fp, lr}
26		ldp fp, lr, [sp], #16
27		ret
28	
29	// length of .L._prints_str0
30		.word 4
31	.L._prints_str0:
32		.asciz "%.*s"
33	.align 4
34	_prints:
35		// push {lr}
36		stp lr, xzr, [sp, #-16]!
37		mov x2, x0
38		ldrsw x1, [x0, #-4]
39		adr x0, .L._prints_str0
40		bl printf
41		mov x0, #0
42		bl fflush
43		// pop {lr}
44		ldp lr, xzr, [sp], #16
45		ret
46	
47	_malloc:
48		// push {lr}
49		stp lr, xzr, [sp, #-16]!
50		bl malloc
51		cbz x0, _errOutOfMemory
52		// pop {lr}
53		ldp lr, xzr, [sp], #16
54		ret
55	
56	// length of .L._errOutOfMemory_str0
57		.word 27
58	.L._errOutOfMemory_str0:
59		.asciz "fatal error: out of memory\n"
60	.align 4
61	_errOutOfMemory:
62		adr x0, .L._errOutOfMemory_str0
63		bl _prints
64		mov w0, #-1
65		bl exit
===========================================================
-- Finished

