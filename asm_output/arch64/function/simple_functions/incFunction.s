./valid/function/simple_functions/incFunction.wacc
calling the reference compiler on ./valid/function/simple_functions/incFunction.wacc
-- Test: incFunction.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple increment function definition and usage

# Output:
# 1
# 4
#

# Program:

begin
  int inc(int x) is
    return x + 1
  end
  int x = 0 ;
  x = call inc(x) ;
  println x ;
  x = call inc(x) ;
  x = call inc(x) ;
  x = call inc(x) ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
incFunction.s contents are:
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
14		mov x8, x19
15		mov x0, x8
16		bl wacc_inc
17		mov x16, x0
18		// Stack pointer unchanged, no stack allocated arguments
19		mov x8, x16
20		mov x19, x8
21		// Stack pointer unchanged, no stack allocated arguments
22		mov x8, x19
23		mov x0, x8
24		// statement primitives do not return results (but will clobber r0/rax)
25		bl _printi
26		bl _println
27		// Stack pointer unchanged, no stack allocated arguments
28		mov x8, x19
29		mov x0, x8
30		bl wacc_inc
31		mov x16, x0
32		// Stack pointer unchanged, no stack allocated arguments
33		mov x8, x16
34		mov x19, x8
35		// Stack pointer unchanged, no stack allocated arguments
36		mov x8, x19
37		mov x0, x8
38		bl wacc_inc
39		mov x16, x0
40		// Stack pointer unchanged, no stack allocated arguments
41		mov x8, x16
42		mov x19, x8
43		// Stack pointer unchanged, no stack allocated arguments
44		mov x8, x19
45		mov x0, x8
46		bl wacc_inc
47		mov x16, x0
48		// Stack pointer unchanged, no stack allocated arguments
49		mov x8, x16
50		mov x19, x8
51		// Stack pointer unchanged, no stack allocated arguments
52		mov x8, x19
53		mov x0, x8
54		// statement primitives do not return results (but will clobber r0/rax)
55		bl _printi
56		bl _println
57		// Stack pointer unchanged, no stack allocated variables
58		mov x0, #0
59		// pop {x19}
60		ldp x19, xzr, [sp], #16
61		// pop {fp, lr}
62		ldp fp, lr, [sp], #16
63		ret
64	
65	wacc_inc:
66		// push {fp, lr}
67		stp fp, lr, [sp, #-16]!
68		mov fp, sp
69		adds w8, w0, #1
70		b.vs _errOverflow
71		sxtw x8, w8
72		// push {x8}
73		stp x8, xzr, [sp, #-16]!
74		// pop {x0}
75		ldp x0, xzr, [sp], #16
76		mov x0, x0
77		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
78		mov sp, fp
79		// pop {fp, lr}
80		ldp fp, lr, [sp], #16
81		ret
82		// 'ere be dragons: this is 100% dead code, functions always end in returns!
83	
84	// length of .L._prints_str0
85		.word 4
86	.L._prints_str0:
87		.asciz "%.*s"
88	.align 4
89	_prints:
90		// push {lr}
91		stp lr, xzr, [sp, #-16]!
92		mov x2, x0
93		ldrsw x1, [x0, #-4]
94		adr x0, .L._prints_str0
95		bl printf
96		mov x0, #0
97		bl fflush
98		// pop {lr}
99		ldp lr, xzr, [sp], #16
100		ret
101	
102	// length of .L._printi_str0
103		.word 2
104	.L._printi_str0:
105		.asciz "%d"
106	.align 4
107	_printi:
108		// push {lr}
109		stp lr, xzr, [sp, #-16]!
110		mov x1, x0
111		adr x0, .L._printi_str0
112		bl printf
113		mov x0, #0
114		bl fflush
115		// pop {lr}
116		ldp lr, xzr, [sp], #16
117		ret
118	
119	// length of .L._println_str0
120		.word 0
121	.L._println_str0:
122		.asciz ""
123	.align 4
124	_println:
125		// push {lr}
126		stp lr, xzr, [sp, #-16]!
127		adr x0, .L._println_str0
128		bl puts
129		mov x0, #0
130		bl fflush
131		// pop {lr}
132		ldp lr, xzr, [sp], #16
133		ret
134	
135	// length of .L._errOverflow_str0
136		.word 52
137	.L._errOverflow_str0:
138		.asciz "fatal error: integer overflow or underflow occurred\n"
139	.align 4
140	_errOverflow:
141		adr x0, .L._errOverflow_str0
142		bl _prints
143		mov w0, #-1
144		bl exit
===========================================================
-- Finished

