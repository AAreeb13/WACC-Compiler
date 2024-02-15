./valid/while/whileCount.wacc
calling the reference compiler on ./valid/while/whileCount.wacc
-- Test: whileCount.wacc

-- Uploaded file: 
---------------------------------------------------------------
# simple counting while loop

# Output:
# Can you count to 10?
# 1
# 2
# 3
# 4
# 5
# 6
# 7
# 8
# 9
# 10
#

# Program:

begin
  int i = 1 ;
  println "Can you count to 10?" ;
  while i <= 10 do
    println i ;
    i = i + 1
  done
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
whileCount.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 20
3	.L.str0:
4		.asciz "Can you count to 10?"
5	.align 4
6	.text
7	.global main
8	main:
9		// push {fp, lr}
10		stp fp, lr, [sp, #-16]!
11		// push {x19}
12		stp x19, xzr, [sp, #-16]!
13		mov fp, sp
14		// Stack pointer unchanged, no stack allocated variables
15		mov x8, #1
16		mov x19, x8
17		// Stack pointer unchanged, no stack allocated arguments
18		adrp x8, .L.str0
19		add x8, x8, :lo12:.L.str0
20		// push {x8}
21		stp x8, xzr, [sp, #-16]!
22		// pop {x8}
23		ldp x8, xzr, [sp], #16
24		mov x8, x8
25		mov x0, x8
26		// statement primitives do not return results (but will clobber r0/rax)
27		bl _prints
28		bl _println
29		b .L0
30	.L1:
31		// Stack pointer unchanged, no stack allocated arguments
32		mov x8, x19
33		mov x0, x8
34		// statement primitives do not return results (but will clobber r0/rax)
35		bl _printi
36		bl _println
37		adds w8, w19, #1
38		b.vs _errOverflow
39		sxtw x8, w8
40		// push {x8}
41		stp x8, xzr, [sp, #-16]!
42		// pop {x8}
43		ldp x8, xzr, [sp], #16
44		mov x8, x8
45		mov x19, x8
46	.L0:
47		cmp x19, #10
48		b.le .L1
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

