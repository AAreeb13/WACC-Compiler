./valid/array/free.wacc
calling the reference compiler on ./valid/array/free.wacc
-- Test: free.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Create and free an array

# Output:

# Program:

begin
  int[] arr = [1, 2, 3] ;
  free arr
end


---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
free.s contents are:
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
11		// 3 element array
12		mov w0, #16
13		bl _malloc
14		mov x16, x0
15		// array pointers are shifted forwards by 4 bytes (to account for size)
16		add x16, x16, #4
17		mov x8, #3
18		str w8, [x16, #-4]
19		mov x8, #1
20		str w8, [x16, #0]
21		mov x8, #2
22		str w8, [x16, #4]
23		mov x8, #3
24		str w8, [x16, #8]
25		mov x8, x16
26		mov x19, x8
27		// Stack pointer unchanged, no stack allocated arguments
28		// array pointers are shifted forward by 4 bytes, so correct it back to original pointer before free
29		sub x8, x19, #4
30		// push {x8}
31		stp x8, xzr, [sp, #-16]!
32		// pop {x8}
33		ldp x8, xzr, [sp], #16
34		mov x8, x8
35		mov x0, x8
36		// statement primitives do not return results (but will clobber r0/rax)
37		bl free
38		// Stack pointer unchanged, no stack allocated variables
39		mov x0, #0
40		// pop {x19}
41		ldp x19, xzr, [sp], #16
42		// pop {fp, lr}
43		ldp fp, lr, [sp], #16
44		ret
45	
46	// length of .L._prints_str0
47		.word 4
48	.L._prints_str0:
49		.asciz "%.*s"
50	.align 4
51	_prints:
52		// push {lr}
53		stp lr, xzr, [sp, #-16]!
54		mov x2, x0
55		ldrsw x1, [x0, #-4]
56		adr x0, .L._prints_str0
57		bl printf
58		mov x0, #0
59		bl fflush
60		// pop {lr}
61		ldp lr, xzr, [sp], #16
62		ret
63	
64	_malloc:
65		// push {lr}
66		stp lr, xzr, [sp, #-16]!
67		bl malloc
68		cbz x0, _errOutOfMemory
69		// pop {lr}
70		ldp lr, xzr, [sp], #16
71		ret
72	
73	// length of .L._errOutOfMemory_str0
74		.word 27
75	.L._errOutOfMemory_str0:
76		.asciz "fatal error: out of memory\n"
77	.align 4
78	_errOutOfMemory:
79		adr x0, .L._errOutOfMemory_str0
80		bl _prints
81		mov w0, #-1
82		bl exit
===========================================================
-- Finished

