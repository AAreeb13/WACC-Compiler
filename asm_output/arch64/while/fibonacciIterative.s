./valid/while/fibonacciIterative.wacc
calling the reference compiler on ./valid/while/fibonacciIterative.wacc
-- Test: fibonacciIterative.wacc

-- Uploaded file: 
---------------------------------------------------------------
# iterative calculation of the first 20 fibonacci numbers

# Output:
# The first 20 fibonacci numbers are:
# 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, ...
#

# Program:

begin
  int i = 0 ;
  int f0 = 0 ;
  int f1 = 1 ;
  int save = 0;
  println "The first 20 fibonacci numbers are:" ;
  while i < 20 do
    print f0 ;
    print ", " ;
    save = f0 ;
    f0 = f1 ;
    f1 = save + f1 ;
    i = i + 1
  done ;
  println "..."
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
fibonacciIterative.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 35
3	.L.str0:
4		.asciz "The first 20 fibonacci numbers are:"
5	// length of .L.str1
6		.word 2
7	.L.str1:
8		.asciz ", "
9	// length of .L.str2
10		.word 3
11	.L.str2:
12		.asciz "..."
13	.align 4
14	.text
15	.global main
16	main:
17		// push {fp, lr}
18		stp fp, lr, [sp, #-16]!
19		// push {x19, x20, x21, x22}
20		stp x19, x20, [sp, #-32]!
21		stp x21, x22, [sp, #16]
22		mov fp, sp
23		// Stack pointer unchanged, no stack allocated variables
24		mov x8, #0
25		mov x19, x8
26		mov x8, #0
27		mov x20, x8
28		mov x8, #1
29		mov x21, x8
30		mov x8, #0
31		mov x22, x8
32		// Stack pointer unchanged, no stack allocated arguments
33		adrp x8, .L.str0
34		add x8, x8, :lo12:.L.str0
35		// push {x8}
36		stp x8, xzr, [sp, #-16]!
37		// pop {x8}
38		ldp x8, xzr, [sp], #16
39		mov x8, x8
40		mov x0, x8
41		// statement primitives do not return results (but will clobber r0/rax)
42		bl _prints
43		bl _println
44		b .L0
45	.L1:
46		// Stack pointer unchanged, no stack allocated arguments
47		mov x8, x20
48		mov x0, x8
49		// statement primitives do not return results (but will clobber r0/rax)
50		bl _printi
51		// Stack pointer unchanged, no stack allocated arguments
52		adrp x8, .L.str1
53		add x8, x8, :lo12:.L.str1
54		// push {x8}
55		stp x8, xzr, [sp, #-16]!
56		// pop {x8}
57		ldp x8, xzr, [sp], #16
58		mov x8, x8
59		mov x0, x8
60		// statement primitives do not return results (but will clobber r0/rax)
61		bl _prints
62		mov x8, x20
63		mov x22, x8
64		mov x8, x21
65		mov x20, x8
66		adds w8, w22, w21
67		b.vs _errOverflow
68		sxtw x8, w8
69		// push {x8}
70		stp x8, xzr, [sp, #-16]!
71		// pop {x8}
72		ldp x8, xzr, [sp], #16
73		mov x8, x8
74		mov x21, x8
75		adds w8, w19, #1
76		b.vs _errOverflow
77		sxtw x8, w8
78		// push {x8}
79		stp x8, xzr, [sp, #-16]!
80		// pop {x8}
81		ldp x8, xzr, [sp], #16
82		mov x8, x8
83		mov x19, x8
84	.L0:
85		cmp x19, #20
86		b.lt .L1
87		// Stack pointer unchanged, no stack allocated arguments
88		adrp x8, .L.str2
89		add x8, x8, :lo12:.L.str2
90		// push {x8}
91		stp x8, xzr, [sp, #-16]!
92		// pop {x8}
93		ldp x8, xzr, [sp], #16
94		mov x8, x8
95		mov x0, x8
96		// statement primitives do not return results (but will clobber r0/rax)
97		bl _prints
98		bl _println
99		// Stack pointer unchanged, no stack allocated variables
100		mov x0, #0
101		// pop {x19, x20, x21, x22}
102		ldp x21, x22, [sp, #16]
103		ldp x19, x20, [sp], #32
104		// pop {fp, lr}
105		ldp fp, lr, [sp], #16
106		ret
107	
108	// length of .L._prints_str0
109		.word 4
110	.L._prints_str0:
111		.asciz "%.*s"
112	.align 4
113	_prints:
114		// push {lr}
115		stp lr, xzr, [sp, #-16]!
116		mov x2, x0
117		ldrsw x1, [x0, #-4]
118		adr x0, .L._prints_str0
119		bl printf
120		mov x0, #0
121		bl fflush
122		// pop {lr}
123		ldp lr, xzr, [sp], #16
124		ret
125	
126	// length of .L._printi_str0
127		.word 2
128	.L._printi_str0:
129		.asciz "%d"
130	.align 4
131	_printi:
132		// push {lr}
133		stp lr, xzr, [sp, #-16]!
134		mov x1, x0
135		adr x0, .L._printi_str0
136		bl printf
137		mov x0, #0
138		bl fflush
139		// pop {lr}
140		ldp lr, xzr, [sp], #16
141		ret
142	
143	// length of .L._println_str0
144		.word 0
145	.L._println_str0:
146		.asciz ""
147	.align 4
148	_println:
149		// push {lr}
150		stp lr, xzr, [sp, #-16]!
151		adr x0, .L._println_str0
152		bl puts
153		mov x0, #0
154		bl fflush
155		// pop {lr}
156		ldp lr, xzr, [sp], #16
157		ret
158	
159	// length of .L._errOverflow_str0
160		.word 52
161	.L._errOverflow_str0:
162		.asciz "fatal error: integer overflow or underflow occurred\n"
163	.align 4
164	_errOverflow:
165		adr x0, .L._errOverflow_str0
166		bl _prints
167		mov w0, #-1
168		bl exit
===========================================================
-- Finished

