./valid/while/max.wacc
calling the reference compiler on ./valid/while/max.wacc
-- Test: max.wacc

-- Uploaded file: 
---------------------------------------------------------------
# find the max of two numbers

# Output:
# max value = 17
#

# Program:

begin
  int i = 0 ;
  int x = 10 ;
  int y = 17 ;
  while (y > 0 || x > 0) do
    x = x - 1 ;
    y = y - 1 ;
    i = i + 1
  done ;
  print "max value = ";
  println i
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
max.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 12
3	.L.str0:
4		.asciz "max value = "
5	.align 4
6	.text
7	.global main
8	main:
9		// push {fp, lr}
10		stp fp, lr, [sp, #-16]!
11		// push {x19, x20, x21}
12		stp x19, x20, [sp, #-32]!
13		stp x21, xzr, [sp, #16]
14		mov fp, sp
15		// Stack pointer unchanged, no stack allocated variables
16		mov x8, #0
17		mov x19, x8
18		mov x8, #10
19		mov x20, x8
20		mov x8, #17
21		mov x21, x8
22		b .L0
23	.L1:
24		subs w8, w20, #1
25		b.vs _errOverflow
26		sxtw x8, w8
27		// push {x8}
28		stp x8, xzr, [sp, #-16]!
29		// pop {x8}
30		ldp x8, xzr, [sp], #16
31		mov x8, x8
32		mov x20, x8
33		subs w8, w21, #1
34		b.vs _errOverflow
35		sxtw x8, w8
36		// push {x8}
37		stp x8, xzr, [sp, #-16]!
38		// pop {x8}
39		ldp x8, xzr, [sp], #16
40		mov x8, x8
41		mov x21, x8
42		adds w8, w19, #1
43		b.vs _errOverflow
44		sxtw x8, w8
45		// push {x8}
46		stp x8, xzr, [sp, #-16]!
47		// pop {x8}
48		ldp x8, xzr, [sp], #16
49		mov x8, x8
50		mov x19, x8
51	.L0:
52		cmp x21, #0
53		cset x8, gt
54		// push {x8}
55		stp x8, xzr, [sp, #-16]!
56		// pop {x8}
57		ldp x8, xzr, [sp], #16
58		cmp x8, #1
59		b.eq .L2
60		cmp x20, #0
61		cset x8, gt
62		// push {x8}
63		stp x8, xzr, [sp, #-16]!
64		// pop {x8}
65		ldp x8, xzr, [sp], #16
66		cmp x8, #1
67	.L2:
68		b.eq .L1
69		// Stack pointer unchanged, no stack allocated arguments
70		adrp x8, .L.str0
71		add x8, x8, :lo12:.L.str0
72		// push {x8}
73		stp x8, xzr, [sp, #-16]!
74		// pop {x8}
75		ldp x8, xzr, [sp], #16
76		mov x8, x8
77		mov x0, x8
78		// statement primitives do not return results (but will clobber r0/rax)
79		bl _prints
80		// Stack pointer unchanged, no stack allocated arguments
81		mov x8, x19
82		mov x0, x8
83		// statement primitives do not return results (but will clobber r0/rax)
84		bl _printi
85		bl _println
86		// Stack pointer unchanged, no stack allocated variables
87		mov x0, #0
88		// pop {x19, x20, x21}
89		ldp x21, xzr, [sp, #16]
90		ldp x19, x20, [sp], #32
91		// pop {fp, lr}
92		ldp fp, lr, [sp], #16
93		ret
94	
95	// length of .L._prints_str0
96		.word 4
97	.L._prints_str0:
98		.asciz "%.*s"
99	.align 4
100	_prints:
101		// push {lr}
102		stp lr, xzr, [sp, #-16]!
103		mov x2, x0
104		ldrsw x1, [x0, #-4]
105		adr x0, .L._prints_str0
106		bl printf
107		mov x0, #0
108		bl fflush
109		// pop {lr}
110		ldp lr, xzr, [sp], #16
111		ret
112	
113	// length of .L._printi_str0
114		.word 2
115	.L._printi_str0:
116		.asciz "%d"
117	.align 4
118	_printi:
119		// push {lr}
120		stp lr, xzr, [sp, #-16]!
121		mov x1, x0
122		adr x0, .L._printi_str0
123		bl printf
124		mov x0, #0
125		bl fflush
126		// pop {lr}
127		ldp lr, xzr, [sp], #16
128		ret
129	
130	// length of .L._println_str0
131		.word 0
132	.L._println_str0:
133		.asciz ""
134	.align 4
135	_println:
136		// push {lr}
137		stp lr, xzr, [sp, #-16]!
138		adr x0, .L._println_str0
139		bl puts
140		mov x0, #0
141		bl fflush
142		// pop {lr}
143		ldp lr, xzr, [sp], #16
144		ret
145	
146	// length of .L._errOverflow_str0
147		.word 52
148	.L._errOverflow_str0:
149		.asciz "fatal error: integer overflow or underflow occurred\n"
150	.align 4
151	_errOverflow:
152		adr x0, .L._errOverflow_str0
153		bl _prints
154		mov w0, #-1
155		bl exit
===========================================================
-- Finished

