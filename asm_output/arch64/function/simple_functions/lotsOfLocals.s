./valid/function/simple_functions/lotsOfLocals.wacc
calling the reference compiler on ./valid/function/simple_functions/lotsOfLocals.wacc
-- Test: lotsOfLocals.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Functions with lots of local arguments should work ok
# Thanks to Kristina Zimina for finding this

# Output:
# 5
# 8
#

# Program:

begin
    int f (int a, int b, int c, int d, int e, int k) is
        int x1 = 1;
        int x2 = 2;
        int x3 = 3;
        int x4 = 4;
        char x5 = '5';
        char x6 = '6';
        char x7 = '7';
        int x8 = 8;
        begin
            char y1 = 'a';
            char y2 = 'b';
            int y = 5 ;
            println y
        end ;
        return x8
    end

    int r = call f(1, 2, 3, 4, 5, 6);
    println r
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
lotsOfLocals.s contents are:
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
11		// Stack pointer unchanged, no stack allocated arguments
12		mov x8, #1
13		mov x0, x8
14		mov x8, #2
15		mov x1, x8
16		mov x8, #3
17		mov x2, x8
18		mov x8, #4
19		mov x3, x8
20		mov x8, #5
21		mov x4, x8
22		mov x8, #6
23		mov x5, x8
24		bl wacc_f
25		mov x16, x0
26		// Stack pointer unchanged, no stack allocated arguments
27		mov x8, x16
28		mov x19, x8
29		// Stack pointer unchanged, no stack allocated arguments
30		mov x8, x19
31		mov x0, x8
32		// statement primitives do not return results (but will clobber r0/rax)
33		bl _printi
34		bl _println
35		// Stack pointer unchanged, no stack allocated variables
36		mov x0, #0
37		// pop {x19}
38		ldp x19, xzr, [sp], #16
39		// pop {fp, lr}
40		ldp fp, lr, [sp], #16
41		ret
42	
43	wacc_f:
44		// push {fp, lr}
45		stp fp, lr, [sp, #-16]!
46		// push {x19, x20, x21, x22, x23, x24, x25, x26, x27, x28}
47		stp x19, x20, [sp, #-80]!
48		stp x21, x22, [sp, #16]
49		stp x23, x24, [sp, #32]
50		stp x25, x26, [sp, #48]
51		stp x27, x28, [sp, #64]
52		mov fp, sp
53		// Stack pointer unchanged, no stack allocated variables
54		mov x8, #1
55		mov x19, x8
56		mov x8, #2
57		mov x20, x8
58		mov x8, #3
59		mov x21, x8
60		mov x8, #4
61		mov x22, x8
62		mov x8, #53
63		mov x23, x8
64		mov x8, #54
65		mov x24, x8
66		mov x8, #55
67		mov x25, x8
68		mov x8, #8
69		mov x26, x8
70		// Stack pointer unchanged, no stack allocated variables
71		mov x8, #97
72		mov x27, x8
73		mov x8, #98
74		mov x28, x8
75		mov x8, #5
76		mov x6, x8
77		// push {x0, x1, x2, x3, x4, x5, x6}
78		stp x0, x1, [sp, #-64]!
79		stp x2, x3, [sp, #16]
80		stp x4, x5, [sp, #32]
81		stp x6, xzr, [sp, #48]
82		// Set up X16 as a temporary second base pointer for the caller saved things
83		mov x16, sp
84		// Stack pointer unchanged, no stack allocated arguments
85		mov x8, x6
86		mov x0, x8
87		// statement primitives do not return results (but will clobber r0/rax)
88		bl _printi
89		bl _println
90		// pop {x0, x1, x2, x3, x4, x5, x6}
91		ldp x2, x3, [sp, #16]
92		ldp x4, x5, [sp, #32]
93		ldp x6, xzr, [sp, #48]
94		ldp x0, x1, [sp], #64
95		// Stack pointer unchanged, no stack allocated variables
96		mov x0, x26
97		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
98		mov sp, fp
99		// pop {x19, x20, x21, x22, x23, x24, x25, x26, x27, x28}
100		ldp x21, x22, [sp, #16]
101		ldp x23, x24, [sp, #32]
102		ldp x25, x26, [sp, #48]
103		ldp x27, x28, [sp, #64]
104		ldp x19, x20, [sp], #80
105		// pop {fp, lr}
106		ldp fp, lr, [sp], #16
107		ret
108		// Stack pointer unchanged, no stack allocated variables
109		// 'ere be dragons: this is 100% dead code, functions always end in returns!
110	
111	// length of .L._printi_str0
112		.word 2
113	.L._printi_str0:
114		.asciz "%d"
115	.align 4
116	_printi:
117		// push {lr}
118		stp lr, xzr, [sp, #-16]!
119		mov x1, x0
120		adr x0, .L._printi_str0
121		bl printf
122		mov x0, #0
123		bl fflush
124		// pop {lr}
125		ldp lr, xzr, [sp], #16
126		ret
127	
128	// length of .L._println_str0
129		.word 0
130	.L._println_str0:
131		.asciz ""
132	.align 4
133	_println:
134		// push {lr}
135		stp lr, xzr, [sp, #-16]!
136		adr x0, .L._println_str0
137		bl puts
138		mov x0, #0
139		bl fflush
140		// pop {lr}
141		ldp lr, xzr, [sp], #16
142		ret
===========================================================
-- Finished

