./valid/IO/read/read.wacc
calling the reference compiler on ./valid/IO/read/read.wacc
-- Test: read.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple read statement

#Input: 350

# Output:
# input an integer to continue...
#

# Program:

begin
	int x = 10;
  println "input an integer to continue..." ;
	read x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
read.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 31
3	.L.str0:
4		.asciz "input an integer to continue..."
5	.align 4
6	.text
7	.global main
8	main:
9		// push {fp, lr}
10		stp fp, lr, [sp, #-16]!
11		// push {x19}
12		stp x19, xzr, [sp, #-16]!
13		mov fp, sp
14		// Stack pointer unchanged, no stack allocated variables
15		mov x8, #10
16		mov x19, x8
17		// Stack pointer unchanged, no stack allocated arguments
18		adrp x8, .L.str0
19		add x8, x8, :lo12:.L.str0
20		// push {x8}
21		stp x8, xzr, [sp, #-16]!
22		// pop {x8}
23		ldp x8, xzr, [sp], #16
24		mov x8, x8
25		mov x0, x8
26		// statement primitives do not return results (but will clobber r0/rax)
27		bl _prints
28		bl _println
29		// Stack pointer unchanged, no stack allocated arguments
30		// load the current value in the destination of the read so it supports defaults
31		mov x8, x19
32		mov x0, x8
33		bl _readi
34		mov x16, x0
35		mov x8, x16
36		mov x19, x8
37		// Stack pointer unchanged, no stack allocated variables
38		mov x0, #0
39		// pop {x19}
40		ldp x19, xzr, [sp], #16
41		// pop {fp, lr}
42		ldp fp, lr, [sp], #16
43		ret
44	
45	// length of .L._prints_str0
46		.word 4
47	.L._prints_str0:
48		.asciz "%.*s"
49	.align 4
50	_prints:
51		// push {lr}
52		stp lr, xzr, [sp, #-16]!
53		mov x2, x0
54		ldrsw x1, [x0, #-4]
55		adr x0, .L._prints_str0
56		bl printf
57		mov x0, #0
58		bl fflush
59		// pop {lr}
60		ldp lr, xzr, [sp], #16
61		ret
62	
63	// length of .L._readi_str0
64		.word 2
65	.L._readi_str0:
66		.asciz "%d"
67	.align 4
68	_readi:
69		// X0 contains the "original" value of the destination of the read
70		// allocate space on the stack to store the read: preserve alignment!
71		// the passed default argument should be stored in case of EOF
72		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
73		// push {x0, lr}
74		stp x0, lr, [sp, #-16]!
75		mov x1, sp
76		adr x0, .L._readi_str0
77		bl scanf
78		// pop {x0, lr}
79		ldp x0, lr, [sp], #16
80		ret
81	
82	// length of .L._println_str0
83		.word 0
84	.L._println_str0:
85		.asciz ""
86	.align 4
87	_println:
88		// push {lr}
89		stp lr, xzr, [sp, #-16]!
90		adr x0, .L._println_str0
91		bl puts
92		mov x0, #0
93		bl fflush
94		// pop {lr}
95		ldp lr, xzr, [sp], #16
96		ret
===========================================================
-- Finished

