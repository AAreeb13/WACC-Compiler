./valid/scope/scopeWhileRedefine.wacc
calling the reference compiler on ./valid/scope/scopeWhileRedefine.wacc
-- Test: scopeWhileRedefine.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable scoping test that redefines a variable within a while-loop

# Output:
# counting... 5
# counting... 4
# counting... 3
# counting... 2
# counting... 1
# 0 Boom!
#

# Program:

begin
  int x = 5 ;
  string y = " Boom!" ;
  while x > 0 do
    string y = "counting... " ;
    print y;
    println x;
    x = x - 1
  done ;
  print x;
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeWhileRedefine.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 6
3	.L.str0:
4		.asciz " Boom!"
5	// length of .L.str1
6		.word 12
7	.L.str1:
8		.asciz "counting... "
9	.align 4
10	.text
11	.global main
12	main:
13		// push {fp, lr}
14		stp fp, lr, [sp, #-16]!
15		// push {x19, x20, x21}
16		stp x19, x20, [sp, #-32]!
17		stp x21, xzr, [sp, #16]
18		mov fp, sp
19		// Stack pointer unchanged, no stack allocated variables
20		mov x8, #5
21		mov x19, x8
22		adrp x8, .L.str0
23		add x8, x8, :lo12:.L.str0
24		// push {x8}
25		stp x8, xzr, [sp, #-16]!
26		// pop {x8}
27		ldp x8, xzr, [sp], #16
28		mov x8, x8
29		mov x20, x8
30		b .L0
31	.L1:
32		// Stack pointer unchanged, no stack allocated variables
33		adrp x8, .L.str1
34		add x8, x8, :lo12:.L.str1
35		// push {x8}
36		stp x8, xzr, [sp, #-16]!
37		// pop {x8}
38		ldp x8, xzr, [sp], #16
39		mov x8, x8
40		mov x21, x8
41		// Stack pointer unchanged, no stack allocated arguments
42		mov x8, x21
43		mov x0, x8
44		// statement primitives do not return results (but will clobber r0/rax)
45		bl _prints
46		// Stack pointer unchanged, no stack allocated arguments
47		mov x8, x19
48		mov x0, x8
49		// statement primitives do not return results (but will clobber r0/rax)
50		bl _printi
51		bl _println
52		subs w8, w19, #1
53		b.vs _errOverflow
54		sxtw x8, w8
55		// push {x8}
56		stp x8, xzr, [sp, #-16]!
57		// pop {x8}
58		ldp x8, xzr, [sp], #16
59		mov x8, x8
60		mov x19, x8
61		// Stack pointer unchanged, no stack allocated variables
62	.L0:
63		cmp x19, #0
64		b.gt .L1
65		// Stack pointer unchanged, no stack allocated arguments
66		mov x8, x19
67		mov x0, x8
68		// statement primitives do not return results (but will clobber r0/rax)
69		bl _printi
70		// Stack pointer unchanged, no stack allocated arguments
71		mov x8, x20
72		mov x0, x8
73		// statement primitives do not return results (but will clobber r0/rax)
74		bl _prints
75		bl _println
76		// Stack pointer unchanged, no stack allocated variables
77		mov x0, #0
78		// pop {x19, x20, x21}
79		ldp x21, xzr, [sp, #16]
80		ldp x19, x20, [sp], #32
81		// pop {fp, lr}
82		ldp fp, lr, [sp], #16
83		ret
84	
85	// length of .L._prints_str0
86		.word 4
87	.L._prints_str0:
88		.asciz "%.*s"
89	.align 4
90	_prints:
91		// push {lr}
92		stp lr, xzr, [sp, #-16]!
93		mov x2, x0
94		ldrsw x1, [x0, #-4]
95		adr x0, .L._prints_str0
96		bl printf
97		mov x0, #0
98		bl fflush
99		// pop {lr}
100		ldp lr, xzr, [sp], #16
101		ret
102	
103	// length of .L._printi_str0
104		.word 2
105	.L._printi_str0:
106		.asciz "%d"
107	.align 4
108	_printi:
109		// push {lr}
110		stp lr, xzr, [sp, #-16]!
111		mov x1, x0
112		adr x0, .L._printi_str0
113		bl printf
114		mov x0, #0
115		bl fflush
116		// pop {lr}
117		ldp lr, xzr, [sp], #16
118		ret
119	
120	// length of .L._println_str0
121		.word 0
122	.L._println_str0:
123		.asciz ""
124	.align 4
125	_println:
126		// push {lr}
127		stp lr, xzr, [sp, #-16]!
128		adr x0, .L._println_str0
129		bl puts
130		mov x0, #0
131		bl fflush
132		// pop {lr}
133		ldp lr, xzr, [sp], #16
134		ret
135	
136	// length of .L._errOverflow_str0
137		.word 52
138	.L._errOverflow_str0:
139		.asciz "fatal error: integer overflow or underflow occurred\n"
140	.align 4
141	_errOverflow:
142		adr x0, .L._errOverflow_str0
143		bl _prints
144		mov w0, #-1
145		bl exit
===========================================================
-- Finished

