./valid/while/whileBoolFlip.wacc
calling the reference compiler on ./valid/while/whileBoolFlip.wacc
-- Test: whileBoolFlip.wacc

-- Uploaded file: 
---------------------------------------------------------------
# while loop flips bool to terminate

# Output:
# flip b!
# end of loop
#

# Program:

begin
  bool b = true ;
  while b do
    println "flip b!" ;
    b = !b
  done ;
  println "end of loop"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
whileBoolFlip.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 7
3	.L.str0:
4		.asciz "flip b!"
5	// length of .L.str1
6		.word 11
7	.L.str1:
8		.asciz "end of loop"
9	.align 4
10	.text
11	.global main
12	main:
13		// push {fp, lr}
14		stp fp, lr, [sp, #-16]!
15		// push {x19}
16		stp x19, xzr, [sp, #-16]!
17		mov fp, sp
18		// Stack pointer unchanged, no stack allocated variables
19		mov x8, #1
20		mov x19, x8
21		b .L0
22	.L1:
23		// Stack pointer unchanged, no stack allocated arguments
24		adrp x8, .L.str0
25		add x8, x8, :lo12:.L.str0
26		// push {x8}
27		stp x8, xzr, [sp, #-16]!
28		// pop {x8}
29		ldp x8, xzr, [sp], #16
30		mov x8, x8
31		mov x0, x8
32		// statement primitives do not return results (but will clobber r0/rax)
33		bl _prints
34		bl _println
35		cmp x19, #1
36		cset x8, ne
37		// push {x8}
38		stp x8, xzr, [sp, #-16]!
39		// pop {x8}
40		ldp x8, xzr, [sp], #16
41		mov x8, x8
42		mov x19, x8
43	.L0:
44		cmp x19, #1
45		b.eq .L1
46		// Stack pointer unchanged, no stack allocated arguments
47		adrp x8, .L.str1
48		add x8, x8, :lo12:.L.str1
49		// push {x8}
50		stp x8, xzr, [sp, #-16]!
51		// pop {x8}
52		ldp x8, xzr, [sp], #16
53		mov x8, x8
54		mov x0, x8
55		// statement primitives do not return results (but will clobber r0/rax)
56		bl _prints
57		bl _println
58		// Stack pointer unchanged, no stack allocated variables
59		mov x0, #0
60		// pop {x19}
61		ldp x19, xzr, [sp], #16
62		// pop {fp, lr}
63		ldp fp, lr, [sp], #16
64		ret
65	
66	// length of .L._prints_str0
67		.word 4
68	.L._prints_str0:
69		.asciz "%.*s"
70	.align 4
71	_prints:
72		// push {lr}
73		stp lr, xzr, [sp, #-16]!
74		mov x2, x0
75		ldrsw x1, [x0, #-4]
76		adr x0, .L._prints_str0
77		bl printf
78		mov x0, #0
79		bl fflush
80		// pop {lr}
81		ldp lr, xzr, [sp], #16
82		ret
83	
84	// length of .L._println_str0
85		.word 0
86	.L._println_str0:
87		.asciz ""
88	.align 4
89	_println:
90		// push {lr}
91		stp lr, xzr, [sp, #-16]!
92		adr x0, .L._println_str0
93		bl puts
94		mov x0, #0
95		bl fflush
96		// pop {lr}
97		ldp lr, xzr, [sp], #16
98		ret
===========================================================
-- Finished

