./valid/runtimeErr/integerOverflow/intmultOverflow.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intmultOverflow.wacc
-- Test: intmultOverflow.wacc

-- Uploaded file: 
---------------------------------------------------------------
# integer overflow - generates odd assembly error!

# Output:
# 2147483
# 2147483000
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = 2147483 ;
  println x ;
  x = x * 1000 ;
  println x ;
  x = x * 1000 ;
  println x ;
  x = x * 1000 ; #err here?
  println x 
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intmultOverflow.s contents are:
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
11		mov x8, #0xc49b
12		movk x8, #0x20, lsl #16
13		mov x8, x8
14		mov x19, x8
15		// Stack pointer unchanged, no stack allocated arguments
16		mov x8, x19
17		mov x0, x8
18		// statement primitives do not return results (but will clobber r0/rax)
19		bl _printi
20		bl _println
21		mov x9, #1000
22		smull x8, w19, w9
23		// take the 31st bit, sign extend it to 64 bits
24		sbfx x9, x8, #31, #1
25		// now take the top 32 bits of the result, shift and sign extend to 64 bits
26		cmp x9, x8, asr #32
27		// if they are not equal then overflow occured
28		b.ne _errOverflow
29		sxtw x8, w8
30		// push {x8}
31		stp x8, xzr, [sp, #-16]!
32		// pop {x8}
33		ldp x8, xzr, [sp], #16
34		mov x8, x8
35		mov x19, x8
36		// Stack pointer unchanged, no stack allocated arguments
37		mov x8, x19
38		mov x0, x8
39		// statement primitives do not return results (but will clobber r0/rax)
40		bl _printi
41		bl _println
42		mov x9, #1000
43		smull x8, w19, w9
44		// take the 31st bit, sign extend it to 64 bits
45		sbfx x9, x8, #31, #1
46		// now take the top 32 bits of the result, shift and sign extend to 64 bits
47		cmp x9, x8, asr #32
48		// if they are not equal then overflow occured
49		b.ne _errOverflow
50		sxtw x8, w8
51		// push {x8}
52		stp x8, xzr, [sp, #-16]!
53		// pop {x8}
54		ldp x8, xzr, [sp], #16
55		mov x8, x8
56		mov x19, x8
57		// Stack pointer unchanged, no stack allocated arguments
58		mov x8, x19
59		mov x0, x8
60		// statement primitives do not return results (but will clobber r0/rax)
61		bl _printi
62		bl _println
63		mov x9, #1000
64		smull x8, w19, w9
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
77		mov x19, x8
78		// Stack pointer unchanged, no stack allocated arguments
79		mov x8, x19
80		mov x0, x8
81		// statement primitives do not return results (but will clobber r0/rax)
82		bl _printi
83		bl _println
84		// Stack pointer unchanged, no stack allocated variables
85		mov x0, #0
86		// pop {x19}
87		ldp x19, xzr, [sp], #16
88		// pop {fp, lr}
89		ldp fp, lr, [sp], #16
90		ret
91	
92	// length of .L._prints_str0
93		.word 4
94	.L._prints_str0:
95		.asciz "%.*s"
96	.align 4
97	_prints:
98		// push {lr}
99		stp lr, xzr, [sp, #-16]!
100		mov x2, x0
101		ldrsw x1, [x0, #-4]
102		adr x0, .L._prints_str0
103		bl printf
104		mov x0, #0
105		bl fflush
106		// pop {lr}
107		ldp lr, xzr, [sp], #16
108		ret
109	
110	// length of .L._printi_str0
111		.word 2
112	.L._printi_str0:
113		.asciz "%d"
114	.align 4
115	_printi:
116		// push {lr}
117		stp lr, xzr, [sp, #-16]!
118		mov x1, x0
119		adr x0, .L._printi_str0
120		bl printf
121		mov x0, #0
122		bl fflush
123		// pop {lr}
124		ldp lr, xzr, [sp], #16
125		ret
126	
127	// length of .L._println_str0
128		.word 0
129	.L._println_str0:
130		.asciz ""
131	.align 4
132	_println:
133		// push {lr}
134		stp lr, xzr, [sp, #-16]!
135		adr x0, .L._println_str0
136		bl puts
137		mov x0, #0
138		bl fflush
139		// pop {lr}
140		ldp lr, xzr, [sp], #16
141		ret
142	
143	// length of .L._errOverflow_str0
144		.word 52
145	.L._errOverflow_str0:
146		.asciz "fatal error: integer overflow or underflow occurred\n"
147	.align 4
148	_errOverflow:
149		adr x0, .L._errOverflow_str0
150		bl _prints
151		mov w0, #-1
152		bl exit
===========================================================
-- Finished

