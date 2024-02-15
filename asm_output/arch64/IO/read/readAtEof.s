./valid/IO/read/readAtEof.wacc
calling the reference compiler on ./valid/IO/read/readAtEof.wacc
-- Test: readAtEof.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Read should not alter nothing if it can't read anything
# Thanks for Nixon Moony-Enraght for catching this!

# Input: X

# Output:
# XZ
#

# Program:
begin
    char c = 'Z';
    read c;
    print c;

    char c2 = 'Z';
    read c2;
    println c2
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
readAtEof.s contents are:
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
11		mov x8, #90
12		mov x19, x8
13		// Stack pointer unchanged, no stack allocated arguments
14		// load the current value in the destination of the read so it supports defaults
15		mov x8, x19
16		mov x0, x8
17		bl _readc
18		mov x16, x0
19		mov x8, x16
20		mov x19, x8
21		// Stack pointer unchanged, no stack allocated arguments
22		mov x8, x19
23		mov x0, x8
24		// statement primitives do not return results (but will clobber r0/rax)
25		bl _printc
26		mov x8, #90
27		mov x20, x8
28		// Stack pointer unchanged, no stack allocated arguments
29		// load the current value in the destination of the read so it supports defaults
30		mov x8, x20
31		mov x0, x8
32		bl _readc
33		mov x16, x0
34		mov x8, x16
35		mov x20, x8
36		// Stack pointer unchanged, no stack allocated arguments
37		mov x8, x20
38		mov x0, x8
39		// statement primitives do not return results (but will clobber r0/rax)
40		bl _printc
41		bl _println
42		// Stack pointer unchanged, no stack allocated variables
43		mov x0, #0
44		// pop {x19, x20}
45		ldp x19, x20, [sp], #16
46		// pop {fp, lr}
47		ldp fp, lr, [sp], #16
48		ret
49	
50	// length of .L._readc_str0
51		.word 3
52	.L._readc_str0:
53		.asciz " %c"
54	.align 4
55	_readc:
56		// X0 contains the "original" value of the destination of the read
57		// allocate space on the stack to store the read: preserve alignment!
58		// the passed default argument should be stored in case of EOF
59		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
60		// push {x0, lr}
61		stp x0, lr, [sp, #-16]!
62		mov x1, sp
63		adr x0, .L._readc_str0
64		bl scanf
65		// pop {x0, lr}
66		ldp x0, lr, [sp], #16
67		ret
68	
69	// length of .L._printc_str0
70		.word 2
71	.L._printc_str0:
72		.asciz "%c"
73	.align 4
74	_printc:
75		// push {lr}
76		stp lr, xzr, [sp, #-16]!
77		mov x1, x0
78		adr x0, .L._printc_str0
79		bl printf
80		mov x0, #0
81		bl fflush
82		// pop {lr}
83		ldp lr, xzr, [sp], #16
84		ret
85	
86	// length of .L._println_str0
87		.word 0
88	.L._println_str0:
89		.asciz ""
90	.align 4
91	_println:
92		// push {lr}
93		stp lr, xzr, [sp, #-16]!
94		adr x0, .L._println_str0
95		bl puts
96		mov x0, #0
97		bl fflush
98		// pop {lr}
99		ldp lr, xzr, [sp], #16
100		ret
===========================================================
-- Finished

