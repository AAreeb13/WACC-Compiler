./valid/array/emptyArrayScope.wacc
calling the reference compiler on ./valid/array/emptyArrayScope.wacc
-- Test: emptyArrayScope.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Scoping shouldn't affect arrays

# Output:

# Program:

begin
  int[] x = [] ;
  begin
    bool y = true
  end
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
emptyArrayScope.s contents are:
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
22		mov x8, #1
23		mov x20, x8
24		// Stack pointer unchanged, no stack allocated variables
25		// Stack pointer unchanged, no stack allocated variables
26		mov x0, #0
27		// pop {x19, x20}
28		ldp x19, x20, [sp], #16
29		// pop {fp, lr}
30		ldp fp, lr, [sp], #16
31		ret
32	
33	// length of .L._prints_str0
34		.word 4
35	.L._prints_str0:
36		.asciz "%.*s"
37	.align 4
38	_prints:
39		// push {lr}
40		stp lr, xzr, [sp, #-16]!
41		mov x2, x0
42		ldrsw x1, [x0, #-4]
43		adr x0, .L._prints_str0
44		bl printf
45		mov x0, #0
46		bl fflush
47		// pop {lr}
48		ldp lr, xzr, [sp], #16
49		ret
50	
51	_malloc:
52		// push {lr}
53		stp lr, xzr, [sp, #-16]!
54		bl malloc
55		cbz x0, _errOutOfMemory
56		// pop {lr}
57		ldp lr, xzr, [sp], #16
58		ret
59	
60	// length of .L._errOutOfMemory_str0
61		.word 27
62	.L._errOutOfMemory_str0:
63		.asciz "fatal error: out of memory\n"
64	.align 4
65	_errOutOfMemory:
66		adr x0, .L._errOutOfMemory_str0
67		bl _prints
68		mov w0, #-1
69		bl exit
===========================================================
-- Finished

