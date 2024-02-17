./valid/function/simple_functions/negFunction.wacc
calling the reference compiler on ./valid/function/simple_functions/negFunction.wacc
-- Test: negFunction.wacc

-- Uploaded file: 
---------------------------------------------------------------
# a simple negation function definition and usage

# Output:
# true
# false
# true
#

# Program:

begin
  bool neg(bool b) is
    return !b
  end
  bool b = true ;
  println b ;
  b = call neg(b) ;
  println b ;
  b = call neg(b) ;
  b = call neg(b) ;
  b = call neg(b) ;
  println b
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
negFunction.s contents are:
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
11		mov x8, #1
12		mov x19, x8
13		// Stack pointer unchanged, no stack allocated arguments
14		mov x8, x19
15		mov x0, x8
16		// statement primitives do not return results (but will clobber r0/rax)
17		bl _printb
18		bl _println
19		// Stack pointer unchanged, no stack allocated arguments
20		mov x8, x19
21		mov x0, x8
22		bl wacc_neg
23		mov x16, x0
24		// Stack pointer unchanged, no stack allocated arguments
25		mov x8, x16
26		mov x19, x8
27		// Stack pointer unchanged, no stack allocated arguments
28		mov x8, x19
29		mov x0, x8
30		// statement primitives do not return results (but will clobber r0/rax)
31		bl _printb
32		bl _println
33		// Stack pointer unchanged, no stack allocated arguments
34		mov x8, x19
35		mov x0, x8
36		bl wacc_neg
37		mov x16, x0
38		// Stack pointer unchanged, no stack allocated arguments
39		mov x8, x16
40		mov x19, x8
41		// Stack pointer unchanged, no stack allocated arguments
42		mov x8, x19
43		mov x0, x8
44		bl wacc_neg
45		mov x16, x0
46		// Stack pointer unchanged, no stack allocated arguments
47		mov x8, x16
48		mov x19, x8
49		// Stack pointer unchanged, no stack allocated arguments
50		mov x8, x19
51		mov x0, x8
52		bl wacc_neg
53		mov x16, x0
54		// Stack pointer unchanged, no stack allocated arguments
55		mov x8, x16
56		mov x19, x8
57		// Stack pointer unchanged, no stack allocated arguments
58		mov x8, x19
59		mov x0, x8
60		// statement primitives do not return results (but will clobber r0/rax)
61		bl _printb
62		bl _println
63		// Stack pointer unchanged, no stack allocated variables
64		mov x0, #0
65		// pop {x19}
66		ldp x19, xzr, [sp], #16
67		// pop {fp, lr}
68		ldp fp, lr, [sp], #16
69		ret
70	
71	wacc_neg:
72		// push {fp, lr}
73		stp fp, lr, [sp, #-16]!
74		mov fp, sp
75		cmp x0, #1
76		cset x8, ne
77		// push {x8}
78		stp x8, xzr, [sp, #-16]!
79		// pop {x0}
80		ldp x0, xzr, [sp], #16
81		mov x0, x0
82		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
83		mov sp, fp
84		// pop {fp, lr}
85		ldp fp, lr, [sp], #16
86		ret
87		// 'ere be dragons: this is 100% dead code, functions always end in returns!
88	
89	// length of .L._printb_str0
90		.word 5
91	.L._printb_str0:
92		.asciz "false"
93	// length of .L._printb_str1
94		.word 4
95	.L._printb_str1:
96		.asciz "true"
97	// length of .L._printb_str2
98		.word 4
99	.L._printb_str2:
100		.asciz "%.*s"
101	.align 4
102	_printb:
103		// push {lr}
104		stp lr, xzr, [sp, #-16]!
105		cmp w0, #0
106		b.ne .L_printb0
107		adr x2, .L._printb_str0
108		b .L_printb1
109	.L_printb0:
110		adr x2, .L._printb_str1
111	.L_printb1:
112		ldrsw x1, [x2, #-4]
113		adr x0, .L._printb_str2
114		bl printf
115		mov x0, #0
116		bl fflush
117		// pop {lr}
118		ldp lr, xzr, [sp], #16
119		ret
120	
121	// length of .L._println_str0
122		.word 0
123	.L._println_str0:
124		.asciz ""
125	.align 4
126	_println:
127		// push {lr}
128		stp lr, xzr, [sp, #-16]!
129		adr x0, .L._println_str0
130		bl puts
131		mov x0, #0
132		bl fflush
133		// pop {lr}
134		ldp lr, xzr, [sp], #16
135		ret
===========================================================
-- Finished

