./valid/expressions/boolExpr1.wacc
calling the reference compiler on ./valid/expressions/boolExpr1.wacc
-- Test: boolExpr1.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evaluating a moderately complex boolean expression

# Output:
# Correct
#

# Program:

begin
  bool b = ! ( ( true && false) || (true && false) );
  if b == true then
    println "Correct"
  else
    println "Wrong"
  fi
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
boolExpr1.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 5
3	.L.str0:
4		.asciz "Wrong"
5	// length of .L.str1
6		.word 7
7	.L.str1:
8		.asciz "Correct"
9	.align 4
10	.text
11	.global main
12	main:
13		// push {fp, lr}
14		stp fp, lr, [sp, #-16]!
15		// push {x19}
16		stp x19, xzr, [sp, #-16]!
17		mov fp, sp
18		// Stack pointer unchanged, no stack allocated variables
19		mov x8, #1
20		cmp x8, #1
21		b.ne .L0
22		mov x8, #0
23		cmp x8, #1
24	.L0:
25		cset x8, eq
26		// push {x8}
27		stp x8, xzr, [sp, #-16]!
28		// pop {x8}
29		ldp x8, xzr, [sp], #16
30		cmp x8, #1
31		b.eq .L1
32		mov x8, #1
33		cmp x8, #1
34		b.ne .L2
35		mov x8, #0
36		cmp x8, #1
37	.L2:
38		cset x8, eq
39		// push {x8}
40		stp x8, xzr, [sp, #-16]!
41		// pop {x8}
42		ldp x8, xzr, [sp], #16
43		cmp x8, #1
44	.L1:
45		cset x8, ne
46		// push {x8}
47		stp x8, xzr, [sp, #-16]!
48		// pop {x8}
49		ldp x8, xzr, [sp], #16
50		mov x8, x8
51		mov x19, x8
52		cmp x19, #1
53		b.eq .L3
54		// Stack pointer unchanged, no stack allocated arguments
55		adrp x8, .L.str0
56		add x8, x8, :lo12:.L.str0
57		// push {x8}
58		stp x8, xzr, [sp, #-16]!
59		// pop {x8}
60		ldp x8, xzr, [sp], #16
61		mov x8, x8
62		mov x0, x8
63		// statement primitives do not return results (but will clobber r0/rax)
64		bl _prints
65		bl _println
66		b .L4
67	.L3:
68		// Stack pointer unchanged, no stack allocated arguments
69		adrp x8, .L.str1
70		add x8, x8, :lo12:.L.str1
71		// push {x8}
72		stp x8, xzr, [sp, #-16]!
73		// pop {x8}
74		ldp x8, xzr, [sp], #16
75		mov x8, x8
76		mov x0, x8
77		// statement primitives do not return results (but will clobber r0/rax)
78		bl _prints
79		bl _println
80	.L4:
81		// Stack pointer unchanged, no stack allocated variables
82		mov x0, #0
83		// pop {x19}
84		ldp x19, xzr, [sp], #16
85		// pop {fp, lr}
86		ldp fp, lr, [sp], #16
87		ret
88	
89	// length of .L._prints_str0
90		.word 4
91	.L._prints_str0:
92		.asciz "%.*s"
93	.align 4
94	_prints:
95		// push {lr}
96		stp lr, xzr, [sp, #-16]!
97		mov x2, x0
98		ldrsw x1, [x0, #-4]
99		adr x0, .L._prints_str0
100		bl printf
101		mov x0, #0
102		bl fflush
103		// pop {lr}
104		ldp lr, xzr, [sp], #16
105		ret
106	
107	// length of .L._println_str0
108		.word 0
109	.L._println_str0:
110		.asciz ""
111	.align 4
112	_println:
113		// push {lr}
114		stp lr, xzr, [sp, #-16]!
115		adr x0, .L._println_str0
116		bl puts
117		mov x0, #0
118		bl fflush
119		// pop {lr}
120		ldp lr, xzr, [sp], #16
121		ret
===========================================================
-- Finished

