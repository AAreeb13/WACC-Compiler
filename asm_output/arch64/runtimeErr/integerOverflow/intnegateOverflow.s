./valid/runtimeErr/integerOverflow/intnegateOverflow.wacc
calling the reference compiler on ./valid/runtimeErr/integerOverflow/intnegateOverflow.wacc
-- Test: intnegateOverflow.wacc

-- Uploaded file: 
---------------------------------------------------------------
# negating biggest possible negative integer

# Output:
# -2147483648
# #runtime_error#

# Exit:
# 255

# Program:

begin
  int x = -2147483648 ;
  println x ;
  x = -x; #err here?
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
intnegateOverflow.s contents are:
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
11		mov x8, #-2147483648
12		mov x19, x8
13		// Stack pointer unchanged, no stack allocated arguments
14		mov x8, x19
15		mov x0, x8
16		// statement primitives do not return results (but will clobber r0/rax)
17		bl _printi
18		bl _println
19		mov w8, #0
20		subs w8, w8, w19
21		b.vs _errOverflow
22		sxtw x8, w8
23		// push {x8}
24		stp x8, xzr, [sp, #-16]!
25		// pop {x8}
26		ldp x8, xzr, [sp], #16
27		mov x8, x8
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
43	// length of .L._prints_str0
44		.word 4
45	.L._prints_str0:
46		.asciz "%.*s"
47	.align 4
48	_prints:
49		// push {lr}
50		stp lr, xzr, [sp, #-16]!
51		mov x2, x0
52		ldrsw x1, [x0, #-4]
53		adr x0, .L._prints_str0
54		bl printf
55		mov x0, #0
56		bl fflush
57		// pop {lr}
58		ldp lr, xzr, [sp], #16
59		ret
60	
61	// length of .L._printi_str0
62		.word 2
63	.L._printi_str0:
64		.asciz "%d"
65	.align 4
66	_printi:
67		// push {lr}
68		stp lr, xzr, [sp, #-16]!
69		mov x1, x0
70		adr x0, .L._printi_str0
71		bl printf
72		mov x0, #0
73		bl fflush
74		// pop {lr}
75		ldp lr, xzr, [sp], #16
76		ret
77	
78	// length of .L._println_str0
79		.word 0
80	.L._println_str0:
81		.asciz ""
82	.align 4
83	_println:
84		// push {lr}
85		stp lr, xzr, [sp, #-16]!
86		adr x0, .L._println_str0
87		bl puts
88		mov x0, #0
89		bl fflush
90		// pop {lr}
91		ldp lr, xzr, [sp], #16
92		ret
93	
94	// length of .L._errOverflow_str0
95		.word 52
96	.L._errOverflow_str0:
97		.asciz "fatal error: integer overflow or underflow occurred\n"
98	.align 4
99	_errOverflow:
100		adr x0, .L._errOverflow_str0
101		bl _prints
102		mov w0, #-1
103		bl exit
===========================================================
-- Finished

