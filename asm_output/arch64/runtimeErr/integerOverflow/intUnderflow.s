./valid/runtimeErr/integerOverflow/intUnderflow.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intUnderflow.wacc
-- Test: intUnderflow.wacc

-- Uploaded file: 
---------------------------------------------------------------
# integer underflow

# Output:
# -2147483647
# -2147483648
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = -2147483647 ;
  println x ;
  x = x - 1 ;
  println x ;
  x = x - 1 ; #err here?
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intUnderflow.s contents are:
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
11		mov x8, #-2147483647
12		mov x19, x8
13		// Stack pointer unchanged, no stack allocated arguments
14		mov x8, x19
15		mov x0, x8
16		// statement primitives do not return results (but will clobber r0/rax)
17		bl _printi
18		bl _println
19		subs w8, w19, #1
20		b.vs _errOverflow
21		sxtw x8, w8
22		// push {x8}
23		stp x8, xzr, [sp, #-16]!
24		// pop {x8}
25		ldp x8, xzr, [sp], #16
26		mov x8, x8
27		mov x19, x8
28		// Stack pointer unchanged, no stack allocated arguments
29		mov x8, x19
30		mov x0, x8
31		// statement primitives do not return results (but will clobber r0/rax)
32		bl _printi
33		bl _println
34		subs w8, w19, #1
35		b.vs _errOverflow
36		sxtw x8, w8
37		// push {x8}
38		stp x8, xzr, [sp, #-16]!
39		// pop {x8}
40		ldp x8, xzr, [sp], #16
41		mov x8, x8
42		mov x19, x8
43		// Stack pointer unchanged, no stack allocated arguments
44		mov x8, x19
45		mov x0, x8
46		// statement primitives do not return results (but will clobber r0/rax)
47		bl _printi
48		bl _println
49		// Stack pointer unchanged, no stack allocated variables
50		mov x0, #0
51		// pop {x19}
52		ldp x19, xzr, [sp], #16
53		// pop {fp, lr}
54		ldp fp, lr, [sp], #16
55		ret
56	
57	// length of .L._prints_str0
58		.word 4
59	.L._prints_str0:
60		.asciz "%.*s"
61	.align 4
62	_prints:
63		// push {lr}
64		stp lr, xzr, [sp, #-16]!
65		mov x2, x0
66		ldrsw x1, [x0, #-4]
67		adr x0, .L._prints_str0
68		bl printf
69		mov x0, #0
70		bl fflush
71		// pop {lr}
72		ldp lr, xzr, [sp], #16
73		ret
74	
75	// length of .L._printi_str0
76		.word 2
77	.L._printi_str0:
78		.asciz "%d"
79	.align 4
80	_printi:
81		// push {lr}
82		stp lr, xzr, [sp, #-16]!
83		mov x1, x0
84		adr x0, .L._printi_str0
85		bl printf
86		mov x0, #0
87		bl fflush
88		// pop {lr}
89		ldp lr, xzr, [sp], #16
90		ret
91	
92	// length of .L._println_str0
93		.word 0
94	.L._println_str0:
95		.asciz ""
96	.align 4
97	_println:
98		// push {lr}
99		stp lr, xzr, [sp, #-16]!
100		adr x0, .L._println_str0
101		bl puts
102		mov x0, #0
103		bl fflush
104		// pop {lr}
105		ldp lr, xzr, [sp], #16
106		ret
107	
108	// length of .L._errOverflow_str0
109		.word 52
110	.L._errOverflow_str0:
111		.asciz "fatal error: integer overflow or underflow occurred\n"
112	.align 4
113	_errOverflow:
114		adr x0, .L._errOverflow_str0
115		bl _prints
116		mov w0, #-1
117		bl exit
===========================================================
-- Finished

