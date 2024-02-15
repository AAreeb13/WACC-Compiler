./valid/function/simple_functions/manyArgumentsInt.wacc
calling the reference compiler on ./valid/function/simple_functions/manyArgumentsInt.wacc
-- Test: manyArgumentsInt.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests a function with more arguments than fit into registers

# Output:
# 23
#

# Program:

begin
  # w, x, y and z are stack allocated on aarch32
  # y and z are stack allocated on x86-64
  int f(int s, int t, int u, int v, int w, int x, int y, int z) is
    int i = u + v ;
    int j = w * x ;
    int k = y - z ;
    return i + j * k
  end

  int r = call f(0, 0, 1, 4, 2, 3, 7, 4) ;
  println r
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
manyArgumentsInt.s contents are:
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
12		mov x8, #0
13		mov x0, x8
14		mov x8, #0
15		mov x1, x8
16		mov x8, #1
17		mov x2, x8
18		mov x8, #4
19		mov x3, x8
20		mov x8, #2
21		mov x4, x8
22		mov x8, #3
23		mov x5, x8
24		mov x8, #7
25		mov x6, x8
26		mov x8, #4
27		mov x7, x8
28		bl wacc_f
29		mov x16, x0
30		// Stack pointer unchanged, no stack allocated arguments
31		mov x8, x16
32		mov x19, x8
33		// Stack pointer unchanged, no stack allocated arguments
34		mov x8, x19
35		mov x0, x8
36		// statement primitives do not return results (but will clobber r0/rax)
37		bl _printi
38		bl _println
39		// Stack pointer unchanged, no stack allocated variables
40		mov x0, #0
41		// pop {x19}
42		ldp x19, xzr, [sp], #16
43		// pop {fp, lr}
44		ldp fp, lr, [sp], #16
45		ret
46	
47	wacc_f:
48		// push {fp, lr}
49		stp fp, lr, [sp, #-16]!
50		// push {x19, x20, x21}
51		stp x19, x20, [sp, #-32]!
52		stp x21, xzr, [sp, #16]
53		mov fp, sp
54		// Stack pointer unchanged, no stack allocated variables
55		adds w8, w2, w3
56		b.vs _errOverflow
57		sxtw x8, w8
58		// push {x8}
59		stp x8, xzr, [sp, #-16]!
60		// pop {x8}
61		ldp x8, xzr, [sp], #16
62		mov x8, x8
63		mov x19, x8
64		smull x8, w4, w5
65		// take the 31st bit, sign extend it to 64 bits
66		sbfx x9, x8, #31, #1
67		// now take the top 32 bits of the result, shift and sign extend to 64 bits
68		cmp x9, x8, asr #32
69		// if they are not equal then overflow occured
70		b.ne _errOverflow
71		sxtw x8, w8
72		// push {x8}
73		stp x8, xzr, [sp, #-16]!
74		// pop {x8}
75		ldp x8, xzr, [sp], #16
76		mov x8, x8
77		mov x20, x8
78		subs w8, w6, w7
79		b.vs _errOverflow
80		sxtw x8, w8
81		// push {x8}
82		stp x8, xzr, [sp, #-16]!
83		// pop {x8}
84		ldp x8, xzr, [sp], #16
85		mov x8, x8
86		mov x21, x8
87		smull x8, w20, w21
88		// take the 31st bit, sign extend it to 64 bits
89		sbfx x9, x8, #31, #1
90		// now take the top 32 bits of the result, shift and sign extend to 64 bits
91		cmp x9, x8, asr #32
92		// if they are not equal then overflow occured
93		b.ne _errOverflow
94		sxtw x8, w8
95		// push {x8}
96		stp x8, xzr, [sp, #-16]!
97		// pop {x9}
98		ldp x9, xzr, [sp], #16
99		adds w8, w19, w9
100		b.vs _errOverflow
101		sxtw x8, w8
102		// push {x8}
103		stp x8, xzr, [sp, #-16]!
104		// pop {x0}
105		ldp x0, xzr, [sp], #16
106		mov x0, x0
107		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
108		mov sp, fp
109		// pop {x19, x20, x21}
110		ldp x21, xzr, [sp, #16]
111		ldp x19, x20, [sp], #32
112		// pop {fp, lr}
113		ldp fp, lr, [sp], #16
114		ret
115		// Stack pointer unchanged, no stack allocated variables
116		// 'ere be dragons: this is 100% dead code, functions always end in returns!
117	
118	// length of .L._prints_str0
119		.word 4
120	.L._prints_str0:
121		.asciz "%.*s"
122	.align 4
123	_prints:
124		// push {lr}
125		stp lr, xzr, [sp, #-16]!
126		mov x2, x0
127		ldrsw x1, [x0, #-4]
128		adr x0, .L._prints_str0
129		bl printf
130		mov x0, #0
131		bl fflush
132		// pop {lr}
133		ldp lr, xzr, [sp], #16
134		ret
135	
136	// length of .L._printi_str0
137		.word 2
138	.L._printi_str0:
139		.asciz "%d"
140	.align 4
141	_printi:
142		// push {lr}
143		stp lr, xzr, [sp, #-16]!
144		mov x1, x0
145		adr x0, .L._printi_str0
146		bl printf
147		mov x0, #0
148		bl fflush
149		// pop {lr}
150		ldp lr, xzr, [sp], #16
151		ret
152	
153	// length of .L._println_str0
154		.word 0
155	.L._println_str0:
156		.asciz ""
157	.align 4
158	_println:
159		// push {lr}
160		stp lr, xzr, [sp, #-16]!
161		adr x0, .L._println_str0
162		bl puts
163		mov x0, #0
164		bl fflush
165		// pop {lr}
166		ldp lr, xzr, [sp], #16
167		ret
168	
169	// length of .L._errOverflow_str0
170		.word 52
171	.L._errOverflow_str0:
172		.asciz "fatal error: integer overflow or underflow occurred\n"
173	.align 4
174	_errOverflow:
175		adr x0, .L._errOverflow_str0
176		bl _prints
177		mov w0, #-1
178		bl exit
===========================================================
-- Finished

