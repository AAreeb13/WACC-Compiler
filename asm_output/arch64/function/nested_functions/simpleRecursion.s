./valid/function/nested_functions/simpleRecursion.wacc
calling the reference compiler on ./valid/function/nested_functions/simpleRecursion.wacc
-- Test: simpleRecursion.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple recursive function

# Output:

# Program:

begin
  int rec(int x) is
    if x == 0
    then
      skip
    else
      int y = call rec(x - 1)
    fi ;
    return 42
  end

  int x = 0 ;
  x = call rec(8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
simpleRecursion.s contents are:
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
11		mov x8, #0
12		mov x19, x8
13		// Stack pointer unchanged, no stack allocated arguments
14		mov x8, #8
15		mov x0, x8
16		bl wacc_rec
17		mov x16, x0
18		// Stack pointer unchanged, no stack allocated arguments
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
29	wacc_rec:
30		// push {fp, lr}
31		stp fp, lr, [sp, #-16]!
32		// push {x19}
33		stp x19, xzr, [sp, #-16]!
34		mov fp, sp
35		cmp x0, #0
36		b.eq .L0
37		// Stack pointer unchanged, no stack allocated variables
38		// push {x0}
39		stp x0, xzr, [sp, #-16]!
40		// Set up X16 as a temporary second base pointer for the caller saved things
41		mov x16, sp
42		// Stack pointer unchanged, no stack allocated arguments
43		subs w8, w0, #1
44		b.vs _errOverflow
45		sxtw x8, w8
46		// push {x8}
47		stp x8, xzr, [sp, #-16]!
48		// pop {x8}
49		ldp x8, xzr, [sp], #16
50		mov x8, x8
51		mov x0, x8
52		bl wacc_rec
53		mov x16, x0
54		// Stack pointer unchanged, no stack allocated arguments
55		// pop {x0}
56		ldp x0, xzr, [sp], #16
57		mov x8, x16
58		mov x19, x8
59		// Stack pointer unchanged, no stack allocated variables
60		b .L1
61	.L0:
62	.L1:
63		mov x0, #42
64		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
65		mov sp, fp
66		// pop {x19}
67		ldp x19, xzr, [sp], #16
68		// pop {fp, lr}
69		ldp fp, lr, [sp], #16
70		ret
71		// 'ere be dragons: this is 100% dead code, functions always end in returns!
72	
73	// length of .L._prints_str0
74		.word 4
75	.L._prints_str0:
76		.asciz "%.*s"
77	.align 4
78	_prints:
79		// push {lr}
80		stp lr, xzr, [sp, #-16]!
81		mov x2, x0
82		ldrsw x1, [x0, #-4]
83		adr x0, .L._prints_str0
84		bl printf
85		mov x0, #0
86		bl fflush
87		// pop {lr}
88		ldp lr, xzr, [sp], #16
89		ret
90	
91	// length of .L._errOverflow_str0
92		.word 52
93	.L._errOverflow_str0:
94		.asciz "fatal error: integer overflow or underflow occurred\n"
95	.align 4
96	_errOverflow:
97		adr x0, .L._errOverflow_str0
98		bl _prints
99		mov w0, #-1
100		bl exit
===========================================================
-- Finished

