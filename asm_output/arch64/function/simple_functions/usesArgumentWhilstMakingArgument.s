./valid/function/simple_functions/usesArgumentWhilstMakingArgument.wacc
calling the reference compiler on ./valid/function/simple_functions/usesArgumentWhilstMakingArgument.wacc
-- Test: usesArgumentWhilstMakingArgument.wacc

-- Uploaded file: 
---------------------------------------------------------------
# tests a function with more arguments than fit into registers

# Output:
# 12
# -4
# 32
#

# Program:

begin
  int f(int u, int v) is
    int x = call g(u + v, u - v, u * v) ;
    return x
  end

  int g(int x, int y, int z) is
    println(x) ;
    println(y) ;
    println(z) ;
    return 0
  end

  int r = call f(4, 8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
usesArgumentWhilstMakingArgument.s contents are:
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
12		mov x8, #4
13		mov x0, x8
14		mov x8, #8
15		mov x1, x8
16		bl wacc_f
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
29	wacc_f:
30		// push {fp, lr}
31		stp fp, lr, [sp, #-16]!
32		// push {x19}
33		stp x19, xzr, [sp, #-16]!
34		mov fp, sp
35		// Stack pointer unchanged, no stack allocated variables
36		// push {x0, x1}
37		stp x0, x1, [sp, #-16]!
38		// Set up X16 as a temporary second base pointer for the caller saved things
39		mov x16, sp
40		// Stack pointer unchanged, no stack allocated arguments
41		adds w8, w0, w1
42		b.vs _errOverflow
43		sxtw x8, w8
44		// push {x8}
45		stp x8, xzr, [sp, #-16]!
46		// pop {x8}
47		ldp x8, xzr, [sp], #16
48		mov x8, x8
49		mov x0, x8
50		ldr x8, [x16, #0]
51		subs w8, w8, w1
52		b.vs _errOverflow
53		sxtw x8, w8
54		// push {x8}
55		stp x8, xzr, [sp, #-16]!
56		// pop {x8}
57		ldp x8, xzr, [sp], #16
58		mov x8, x8
59		mov x1, x8
60		ldr x9, [x16, #8]
61		ldr x8, [x16, #0]
62		smull x8, w8, w9
63		// take the 31st bit, sign extend it to 64 bits
64		sbfx x9, x8, #31, #1
65		// now take the top 32 bits of the result, shift and sign extend to 64 bits
66		cmp x9, x8, asr #32
67		// if they are not equal then overflow occured
68		b.ne _errOverflow
69		sxtw x8, w8
70		// push {x8}
71		stp x8, xzr, [sp, #-16]!
72		// pop {x8}
73		ldp x8, xzr, [sp], #16
74		mov x8, x8
75		mov x2, x8
76		bl wacc_g
77		mov x16, x0
78		// Stack pointer unchanged, no stack allocated arguments
79		// pop {x0, x1}
80		ldp x0, x1, [sp], #16
81		mov x8, x16
82		mov x19, x8
83		mov x0, x19
84		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
85		mov sp, fp
86		// pop {x19}
87		ldp x19, xzr, [sp], #16
88		// pop {fp, lr}
89		ldp fp, lr, [sp], #16
90		ret
91		// Stack pointer unchanged, no stack allocated variables
92		// 'ere be dragons: this is 100% dead code, functions always end in returns!
93	
94	wacc_g:
95		// push {fp, lr}
96		stp fp, lr, [sp, #-16]!
97		mov fp, sp
98		// push {x0, x1, x2}
99		stp x0, x1, [sp, #-32]!
100		stp x2, xzr, [sp, #16]
101		// Set up X16 as a temporary second base pointer for the caller saved things
102		mov x16, sp
103		// Stack pointer unchanged, no stack allocated arguments
104		mov x8, x0
105		mov x0, x8
106		// statement primitives do not return results (but will clobber r0/rax)
107		bl _printi
108		bl _println
109		// pop {x0, x1, x2}
110		ldp x2, xzr, [sp, #16]
111		ldp x0, x1, [sp], #32
112		// push {x0, x1, x2}
113		stp x0, x1, [sp, #-32]!
114		stp x2, xzr, [sp, #16]
115		// Set up X16 as a temporary second base pointer for the caller saved things
116		mov x16, sp
117		// Stack pointer unchanged, no stack allocated arguments
118		mov x8, x1
119		mov x0, x8
120		// statement primitives do not return results (but will clobber r0/rax)
121		bl _printi
122		bl _println
123		// pop {x0, x1, x2}
124		ldp x2, xzr, [sp, #16]
125		ldp x0, x1, [sp], #32
126		// push {x0, x1, x2}
127		stp x0, x1, [sp, #-32]!
128		stp x2, xzr, [sp, #16]
129		// Set up X16 as a temporary second base pointer for the caller saved things
130		mov x16, sp
131		// Stack pointer unchanged, no stack allocated arguments
132		mov x8, x2
133		mov x0, x8
134		// statement primitives do not return results (but will clobber r0/rax)
135		bl _printi
136		bl _println
137		// pop {x0, x1, x2}
138		ldp x2, xzr, [sp, #16]
139		ldp x0, x1, [sp], #32
140		mov x0, #0
141		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
142		mov sp, fp
143		// pop {fp, lr}
144		ldp fp, lr, [sp], #16
145		ret
146		// 'ere be dragons: this is 100% dead code, functions always end in returns!
147	
148	// length of .L._prints_str0
149		.word 4
150	.L._prints_str0:
151		.asciz "%.*s"
152	.align 4
153	_prints:
154		// push {lr}
155		stp lr, xzr, [sp, #-16]!
156		mov x2, x0
157		ldrsw x1, [x0, #-4]
158		adr x0, .L._prints_str0
159		bl printf
160		mov x0, #0
161		bl fflush
162		// pop {lr}
163		ldp lr, xzr, [sp], #16
164		ret
165	
166	// length of .L._printi_str0
167		.word 2
168	.L._printi_str0:
169		.asciz "%d"
170	.align 4
171	_printi:
172		// push {lr}
173		stp lr, xzr, [sp, #-16]!
174		mov x1, x0
175		adr x0, .L._printi_str0
176		bl printf
177		mov x0, #0
178		bl fflush
179		// pop {lr}
180		ldp lr, xzr, [sp], #16
181		ret
182	
183	// length of .L._println_str0
184		.word 0
185	.L._println_str0:
186		.asciz ""
187	.align 4
188	_println:
189		// push {lr}
190		stp lr, xzr, [sp, #-16]!
191		adr x0, .L._println_str0
192		bl puts
193		mov x0, #0
194		bl fflush
195		// pop {lr}
196		ldp lr, xzr, [sp], #16
197		ret
198	
199	// length of .L._errOverflow_str0
200		.word 52
201	.L._errOverflow_str0:
202		.asciz "fatal error: integer overflow or underflow occurred\n"
203	.align 4
204	_errOverflow:
205		adr x0, .L._errOverflow_str0
206		bl _prints
207		mov w0, #-1
208		bl exit
===========================================================
-- Finished

