./valid/function/simple_functions/functionSimpleLoop.wacc
calling the reference compiler on ./valid/function/simple_functions/functionSimpleLoop.wacc
-- Test: functionSimpleLoop.wacc

-- Uploaded file: 
---------------------------------------------------------------
# define and call a function with a simple loop

# Output:
# 10
#

# Program:

begin
  int f(int n) is
    int i = 0;
    while i < n do
      i = i + 1
    done;
    return i
  end
  int x = call f(10) ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionSimpleLoop.s contents are:
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
12		mov x8, #10
13		mov x0, x8
14		bl wacc_f
15		mov x16, x0
16		// Stack pointer unchanged, no stack allocated arguments
17		mov x8, x16
18		mov x19, x8
19		// Stack pointer unchanged, no stack allocated arguments
20		mov x8, x19
21		mov x0, x8
22		// statement primitives do not return results (but will clobber r0/rax)
23		bl _printi
24		bl _println
25		// Stack pointer unchanged, no stack allocated variables
26		mov x0, #0
27		// pop {x19}
28		ldp x19, xzr, [sp], #16
29		// pop {fp, lr}
30		ldp fp, lr, [sp], #16
31		ret
32	
33	wacc_f:
34		// push {fp, lr}
35		stp fp, lr, [sp, #-16]!
36		// push {x19}
37		stp x19, xzr, [sp, #-16]!
38		mov fp, sp
39		// Stack pointer unchanged, no stack allocated variables
40		mov x8, #0
41		mov x19, x8
42		b .L0
43	.L1:
44		adds w8, w19, #1
45		b.vs _errOverflow
46		sxtw x8, w8
47		// push {x8}
48		stp x8, xzr, [sp, #-16]!
49		// pop {x8}
50		ldp x8, xzr, [sp], #16
51		mov x8, x8
52		mov x19, x8
53	.L0:
54		cmp x19, x0
55		b.lt .L1
56		mov x0, x19
57		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
58		mov sp, fp
59		// pop {x19}
60		ldp x19, xzr, [sp], #16
61		// pop {fp, lr}
62		ldp fp, lr, [sp], #16
63		ret
64		// Stack pointer unchanged, no stack allocated variables
65		// 'ere be dragons: this is 100% dead code, functions always end in returns!
66	
67	// length of .L._prints_str0
68		.word 4
69	.L._prints_str0:
70		.asciz "%.*s"
71	.align 4
72	_prints:
73		// push {lr}
74		stp lr, xzr, [sp, #-16]!
75		mov x2, x0
76		ldrsw x1, [x0, #-4]
77		adr x0, .L._prints_str0
78		bl printf
79		mov x0, #0
80		bl fflush
81		// pop {lr}
82		ldp lr, xzr, [sp], #16
83		ret
84	
85	// length of .L._printi_str0
86		.word 2
87	.L._printi_str0:
88		.asciz "%d"
89	.align 4
90	_printi:
91		// push {lr}
92		stp lr, xzr, [sp, #-16]!
93		mov x1, x0
94		adr x0, .L._printi_str0
95		bl printf
96		mov x0, #0
97		bl fflush
98		// pop {lr}
99		ldp lr, xzr, [sp], #16
100		ret
101	
102	// length of .L._println_str0
103		.word 0
104	.L._println_str0:
105		.asciz ""
106	.align 4
107	_println:
108		// push {lr}
109		stp lr, xzr, [sp, #-16]!
110		adr x0, .L._println_str0
111		bl puts
112		mov x0, #0
113		bl fflush
114		// pop {lr}
115		ldp lr, xzr, [sp], #16
116		ret
117	
118	// length of .L._errOverflow_str0
119		.word 52
120	.L._errOverflow_str0:
121		.asciz "fatal error: integer overflow or underflow occurred\n"
122	.align 4
123	_errOverflow:
124		adr x0, .L._errOverflow_str0
125		bl _prints
126		mov w0, #-1
127		bl exit
===========================================================
-- Finished

