./valid/expressions/ordAndchrExpr.wacc
calling the reference compiler on ./valid/expressions/ordAndchrExpr.wacc
-- Test: ordAndchrExpr.wacc

-- Uploaded file: 
---------------------------------------------------------------
# evalutaing ord and chr

# Output:
# a is 97
# 99 is c
#

# Program:

begin
  char a = 'a' ;
  int i = 99 ;

  print a ;
  print " is " ;
  println ord a ;

  print i ;
  print " is " ;
  println chr i
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
ordAndchrExpr.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 4
3	.L.str0:
4		.asciz " is "
5	.align 4
6	.text
7	.global main
8	main:
9		// push {fp, lr}
10		stp fp, lr, [sp, #-16]!
11		// push {x19, x20}
12		stp x19, x20, [sp, #-16]!
13		mov fp, sp
14		// Stack pointer unchanged, no stack allocated variables
15		mov x8, #97
16		mov x19, x8
17		mov x8, #99
18		mov x20, x8
19		// Stack pointer unchanged, no stack allocated arguments
20		mov x8, x19
21		mov x0, x8
22		// statement primitives do not return results (but will clobber r0/rax)
23		bl _printc
24		// Stack pointer unchanged, no stack allocated arguments
25		adrp x8, .L.str0
26		add x8, x8, :lo12:.L.str0
27		// push {x8}
28		stp x8, xzr, [sp, #-16]!
29		// pop {x8}
30		ldp x8, xzr, [sp], #16
31		mov x8, x8
32		mov x0, x8
33		// statement primitives do not return results (but will clobber r0/rax)
34		bl _prints
35		// Stack pointer unchanged, no stack allocated arguments
36		mov x8, x19
37		mov x0, x8
38		// statement primitives do not return results (but will clobber r0/rax)
39		bl _printi
40		bl _println
41		// Stack pointer unchanged, no stack allocated arguments
42		mov x8, x20
43		mov x0, x8
44		// statement primitives do not return results (but will clobber r0/rax)
45		bl _printi
46		// Stack pointer unchanged, no stack allocated arguments
47		adrp x8, .L.str0
48		add x8, x8, :lo12:.L.str0
49		// push {x8}
50		stp x8, xzr, [sp, #-16]!
51		// pop {x8}
52		ldp x8, xzr, [sp], #16
53		mov x8, x8
54		mov x0, x8
55		// statement primitives do not return results (but will clobber r0/rax)
56		bl _prints
57		// Stack pointer unchanged, no stack allocated arguments
58		mov x8, x20
59		tst x8, #0xffffffffffffff80
60		csel x1, x8, x1, ne
61		b.ne _errBadChar
62		// push {x8}
63		stp x8, xzr, [sp, #-16]!
64		// pop {x8}
65		ldp x8, xzr, [sp], #16
66		mov x8, x8
67		mov x0, x8
68		// statement primitives do not return results (but will clobber r0/rax)
69		bl _printc
70		bl _println
71		// Stack pointer unchanged, no stack allocated variables
72		mov x0, #0
73		// pop {x19, x20}
74		ldp x19, x20, [sp], #16
75		// pop {fp, lr}
76		ldp fp, lr, [sp], #16
77		ret
78	
79	// length of .L._prints_str0
80		.word 4
81	.L._prints_str0:
82		.asciz "%.*s"
83	.align 4
84	_prints:
85		// push {lr}
86		stp lr, xzr, [sp, #-16]!
87		mov x2, x0
88		ldrsw x1, [x0, #-4]
89		adr x0, .L._prints_str0
90		bl printf
91		mov x0, #0
92		bl fflush
93		// pop {lr}
94		ldp lr, xzr, [sp], #16
95		ret
96	
97	// length of .L._printi_str0
98		.word 2
99	.L._printi_str0:
100		.asciz "%d"
101	.align 4
102	_printi:
103		// push {lr}
104		stp lr, xzr, [sp, #-16]!
105		mov x1, x0
106		adr x0, .L._printi_str0
107		bl printf
108		mov x0, #0
109		bl fflush
110		// pop {lr}
111		ldp lr, xzr, [sp], #16
112		ret
113	
114	// length of .L._printc_str0
115		.word 2
116	.L._printc_str0:
117		.asciz "%c"
118	.align 4
119	_printc:
120		// push {lr}
121		stp lr, xzr, [sp, #-16]!
122		mov x1, x0
123		adr x0, .L._printc_str0
124		bl printf
125		mov x0, #0
126		bl fflush
127		// pop {lr}
128		ldp lr, xzr, [sp], #16
129		ret
130	
131	// length of .L._println_str0
132		.word 0
133	.L._println_str0:
134		.asciz ""
135	.align 4
136	_println:
137		// push {lr}
138		stp lr, xzr, [sp, #-16]!
139		adr x0, .L._println_str0
140		bl puts
141		mov x0, #0
142		bl fflush
143		// pop {lr}
144		ldp lr, xzr, [sp], #16
145		ret
146	
147	// length of .L._errBadChar_str0
148		.word 50
149	.L._errBadChar_str0:
150		.asciz "fatal error: int %d is not ascii character 0-127 \n"
151	.align 4
152	_errBadChar:
153		adr x0, .L._errBadChar_str0
154		bl printf
155		mov x0, #0
156		bl fflush
157		mov w0, #-1
158		bl exit
===========================================================
-- Finished

