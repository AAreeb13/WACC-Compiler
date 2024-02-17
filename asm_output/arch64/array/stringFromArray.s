./valid/array/stringFromArray.wacc
calling the reference compiler on ./valid/array/stringFromArray.wacc
-- Test: stringFromArray.wacc

-- Uploaded file: 
---------------------------------------------------------------
# char[] can be directly assigned to string

# Output:

# Program:

begin
  string a = ['a', 'b'] ;
  exit 0
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
stringFromArray.s contents are:
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
11		// 2 element array
12		mov w0, #6
13		bl _malloc
14		mov x16, x0
15		// array pointers are shifted forwards by 4 bytes (to account for size)
16		add x16, x16, #4
17		mov x8, #2
18		str w8, [x16, #-4]
19		mov x8, #97
20		strb w8, [x16, #0]
21		mov x8, #98
22		strb w8, [x16, #1]
23		mov x8, x16
24		mov x19, x8
25		// Stack pointer unchanged, no stack allocated arguments
26		mov x8, #0
27		mov x0, x8
28		// statement primitives do not return results (but will clobber r0/rax)
29		bl exit
30		// Stack pointer unchanged, no stack allocated variables
31		mov x0, #0
32		// pop {x19}
33		ldp x19, xzr, [sp], #16
34		// pop {fp, lr}
35		ldp fp, lr, [sp], #16
36		ret
37	
38	// length of .L._prints_str0
39		.word 4
40	.L._prints_str0:
41		.asciz "%.*s"
42	.align 4
43	_prints:
44		// push {lr}
45		stp lr, xzr, [sp, #-16]!
46		mov x2, x0
47		ldrsw x1, [x0, #-4]
48		adr x0, .L._prints_str0
49		bl printf
50		mov x0, #0
51		bl fflush
52		// pop {lr}
53		ldp lr, xzr, [sp], #16
54		ret
55	
56	_malloc:
57		// push {lr}
58		stp lr, xzr, [sp, #-16]!
59		bl malloc
60		cbz x0, _errOutOfMemory
61		// pop {lr}
62		ldp lr, xzr, [sp], #16
63		ret
64	
65	// length of .L._errOutOfMemory_str0
66		.word 27
67	.L._errOutOfMemory_str0:
68		.asciz "fatal error: out of memory\n"
69	.align 4
70	_errOutOfMemory:
71		adr x0, .L._errOutOfMemory_str0
72		bl _prints
73		mov w0, #-1
74		bl exit
===========================================================
-- Finished

