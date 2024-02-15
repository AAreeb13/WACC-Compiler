./valid/while/loopIntCondition.wacc
calling the reference compiler on ./valid/while/loopIntCondition.wacc
-- Test: loopIntCondition.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Use an integer as a loop condition. Enter the loop once only, then exit the loop.

# Output:
# Change n
# Should print "Change n" once before.
#

# Program:

begin
	int n = 0 ;
	while n != 1 do
		n = 1 ;
		println "Change n"
	done ;
	println "Should print \"Change n\" once before."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
loopIntCondition.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 8
3	.L.str0:
4		.asciz "Change n"
5	// length of .L.str1
6		.word 36
7	.L.str1:
8		.asciz "Should print \"Change n\" once before."
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
19		mov x8, #0
20		mov x19, x8
21		b .L0
22	.L1:
23		mov x8, #1
24		mov x19, x8
25		// Stack pointer unchanged, no stack allocated arguments
26		adrp x8, .L.str0
27		add x8, x8, :lo12:.L.str0
28		// push {x8}
29		stp x8, xzr, [sp, #-16]!
30		// pop {x8}
31		ldp x8, xzr, [sp], #16
32		mov x8, x8
33		mov x0, x8
34		// statement primitives do not return results (but will clobber r0/rax)
35		bl _prints
36		bl _println
37	.L0:
38		cmp x19, #1
39		b.ne .L1
40		// Stack pointer unchanged, no stack allocated arguments
41		adrp x8, .L.str1
42		add x8, x8, :lo12:.L.str1
43		// push {x8}
44		stp x8, xzr, [sp, #-16]!
45		// pop {x8}
46		ldp x8, xzr, [sp], #16
47		mov x8, x8
48		mov x0, x8
49		// statement primitives do not return results (but will clobber r0/rax)
50		bl _prints
51		bl _println
52		// Stack pointer unchanged, no stack allocated variables
53		mov x0, #0
54		// pop {x19}
55		ldp x19, xzr, [sp], #16
56		// pop {fp, lr}
57		ldp fp, lr, [sp], #16
58		ret
59	
60	// length of .L._prints_str0
61		.word 4
62	.L._prints_str0:
63		.asciz "%.*s"
64	.align 4
65	_prints:
66		// push {lr}
67		stp lr, xzr, [sp, #-16]!
68		mov x2, x0
69		ldrsw x1, [x0, #-4]
70		adr x0, .L._prints_str0
71		bl printf
72		mov x0, #0
73		bl fflush
74		// pop {lr}
75		ldp lr, xzr, [sp], #16
76		ret
77	
78	// length of .L._println_str0
79		.word 0
80	.L._println_str0:
81		.asciz ""
82	.align 4
83	_println:
84		// push {lr}
85		stp lr, xzr, [sp, #-16]!
86		adr x0, .L._println_str0
87		bl puts
88		mov x0, #0
89		bl fflush
90		// pop {lr}
91		ldp lr, xzr, [sp], #16
92		ret
===========================================================
-- Finished

