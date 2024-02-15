./valid/while/whileFalse.wacc
calling the reference compiler on ./valid/while/whileFalse.wacc
-- Test: whileFalse.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple unentered while loop

# Output:
# end of loop
#

# Program:

begin
  while false do
    println "looping..."
  done ;
  println "end of loop"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
whileFalse.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 10
3	.L.str0:
4		.asciz "looping..."
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
15		mov fp, sp
16		b .L0
17	.L1:
18		// Stack pointer unchanged, no stack allocated arguments
19		adrp x8, .L.str0
20		add x8, x8, :lo12:.L.str0
21		// push {x8}
22		stp x8, xzr, [sp, #-16]!
23		// pop {x8}
24		ldp x8, xzr, [sp], #16
25		mov x8, x8
26		mov x0, x8
27		// statement primitives do not return results (but will clobber r0/rax)
28		bl _prints
29		bl _println
30	.L0:
31		mov x8, #0
32		cmp x8, #1
33		b.eq .L1
34		// Stack pointer unchanged, no stack allocated arguments
35		adrp x8, .L.str1
36		add x8, x8, :lo12:.L.str1
37		// push {x8}
38		stp x8, xzr, [sp, #-16]!
39		// pop {x8}
40		ldp x8, xzr, [sp], #16
41		mov x8, x8
42		mov x0, x8
43		// statement primitives do not return results (but will clobber r0/rax)
44		bl _prints
45		bl _println
46		mov x0, #0
47		// pop {fp, lr}
48		ldp fp, lr, [sp], #16
49		ret
50	
51	// length of .L._prints_str0
52		.word 4
53	.L._prints_str0:
54		.asciz "%.*s"
55	.align 4
56	_prints:
57		// push {lr}
58		stp lr, xzr, [sp, #-16]!
59		mov x2, x0
60		ldrsw x1, [x0, #-4]
61		adr x0, .L._prints_str0
62		bl printf
63		mov x0, #0
64		bl fflush
65		// pop {lr}
66		ldp lr, xzr, [sp], #16
67		ret
68	
69	// length of .L._println_str0
70		.word 0
71	.L._println_str0:
72		.asciz ""
73	.align 4
74	_println:
75		// push {lr}
76		stp lr, xzr, [sp, #-16]!
77		adr x0, .L._println_str0
78		bl puts
79		mov x0, #0
80		bl fflush
81		// pop {lr}
82		ldp lr, xzr, [sp], #16
83		ret
===========================================================
-- Finished

