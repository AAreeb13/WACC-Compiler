./valid/function/simple_functions/punning.wacc
calling the reference compiler on ./valid/function/simple_functions/punning.wacc
-- Test: punning.wacc

-- Uploaded file: 
---------------------------------------------------------------
# Functions should be able to have the same name as variables
# Thanks to Jordan Hall

# Output:
# 0
#

begin
    int inc(int x) is
        return x + 1
    end
    int inc = 0;
    int tmp = call inc(inc);
    println inc
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
punning.s contents are:
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
11		mov x8, #0
12		mov x19, x8
13		// Stack pointer unchanged, no stack allocated arguments
14		mov x8, x19
15		mov x0, x8
16		bl wacc_inc
17		mov x16, x0
18		// Stack pointer unchanged, no stack allocated arguments
19		mov x8, x16
20		mov x20, x8
21		// Stack pointer unchanged, no stack allocated arguments
22		mov x8, x19
23		mov x0, x8
24		// statement primitives do not return results (but will clobber r0/rax)
25		bl _printi
26		bl _println
27		// Stack pointer unchanged, no stack allocated variables
28		mov x0, #0
29		// pop {x19, x20}
30		ldp x19, x20, [sp], #16
31		// pop {fp, lr}
32		ldp fp, lr, [sp], #16
33		ret
34	
35	wacc_inc:
36		// push {fp, lr}
37		stp fp, lr, [sp, #-16]!
38		mov fp, sp
39		adds w8, w0, #1
40		b.vs _errOverflow
41		sxtw x8, w8
42		// push {x8}
43		stp x8, xzr, [sp, #-16]!
44		// pop {x0}
45		ldp x0, xzr, [sp], #16
46		mov x0, x0
47		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
48		mov sp, fp
49		// pop {fp, lr}
50		ldp fp, lr, [sp], #16
51		ret
52		// 'ere be dragons: this is 100% dead code, functions always end in returns!
53	
54	// length of .L._prints_str0
55		.word 4
56	.L._prints_str0:
57		.asciz "%.*s"
58	.align 4
59	_prints:
60		// push {lr}
61		stp lr, xzr, [sp, #-16]!
62		mov x2, x0
63		ldrsw x1, [x0, #-4]
64		adr x0, .L._prints_str0
65		bl printf
66		mov x0, #0
67		bl fflush
68		// pop {lr}
69		ldp lr, xzr, [sp], #16
70		ret
71	
72	// length of .L._printi_str0
73		.word 2
74	.L._printi_str0:
75		.asciz "%d"
76	.align 4
77	_printi:
78		// push {lr}
79		stp lr, xzr, [sp, #-16]!
80		mov x1, x0
81		adr x0, .L._printi_str0
82		bl printf
83		mov x0, #0
84		bl fflush
85		// pop {lr}
86		ldp lr, xzr, [sp], #16
87		ret
88	
89	// length of .L._println_str0
90		.word 0
91	.L._println_str0:
92		.asciz ""
93	.align 4
94	_println:
95		// push {lr}
96		stp lr, xzr, [sp, #-16]!
97		adr x0, .L._println_str0
98		bl puts
99		mov x0, #0
100		bl fflush
101		// pop {lr}
102		ldp lr, xzr, [sp], #16
103		ret
104	
105	// length of .L._errOverflow_str0
106		.word 52
107	.L._errOverflow_str0:
108		.asciz "fatal error: integer overflow or underflow occurred\n"
109	.align 4
110	_errOverflow:
111		adr x0, .L._errOverflow_str0
112		bl _prints
113		mov w0, #-1
114		bl exit
===========================================================
-- Finished

