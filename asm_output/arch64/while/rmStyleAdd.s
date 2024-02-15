./valid/while/rmStyleAdd.wacc
calling the reference compiler on ./valid/while/rmStyleAdd.wacc
-- Test: rmStyleAdd.wacc

-- Uploaded file: 
---------------------------------------------------------------
# register machine style addition

# Output:
# initial value of x: 3
# (+)(+)(+)(+)(+)(+)(+)
# final value of x: 10
#

# Program:

begin
  int x = 3 ;
  int y = 7 ;
  print "initial value of x: " ;
  println x ;
  while y > 0 do
    print "(+)" ;
    x = x + 1 ;
    y = y - 1
  done ;
  println "" ;
  print "final value of x: " ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
rmStyleAdd.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 20
3	.L.str0:
4		.asciz "initial value of x: "
5	// length of .L.str1
6		.word 3
7	.L.str1:
8		.asciz "(+)"
9	// length of .L.str2
10		.word 0
11	.L.str2:
12		.asciz ""
13	// length of .L.str3
14		.word 18
15	.L.str3:
16		.asciz "final value of x: "
17	.align 4
18	.text
19	.global main
20	main:
21		// push {fp, lr}
22		stp fp, lr, [sp, #-16]!
23		// push {x19, x20}
24		stp x19, x20, [sp, #-16]!
25		mov fp, sp
26		// Stack pointer unchanged, no stack allocated variables
27		mov x8, #3
28		mov x19, x8
29		mov x8, #7
30		mov x20, x8
31		// Stack pointer unchanged, no stack allocated arguments
32		adrp x8, .L.str0
33		add x8, x8, :lo12:.L.str0
34		// push {x8}
35		stp x8, xzr, [sp, #-16]!
36		// pop {x8}
37		ldp x8, xzr, [sp], #16
38		mov x8, x8
39		mov x0, x8
40		// statement primitives do not return results (but will clobber r0/rax)
41		bl _prints
42		// Stack pointer unchanged, no stack allocated arguments
43		mov x8, x19
44		mov x0, x8
45		// statement primitives do not return results (but will clobber r0/rax)
46		bl _printi
47		bl _println
48		b .L0
49	.L1:
50		// Stack pointer unchanged, no stack allocated arguments
51		adrp x8, .L.str1
52		add x8, x8, :lo12:.L.str1
53		// push {x8}
54		stp x8, xzr, [sp, #-16]!
55		// pop {x8}
56		ldp x8, xzr, [sp], #16
57		mov x8, x8
58		mov x0, x8
59		// statement primitives do not return results (but will clobber r0/rax)
60		bl _prints
61		adds w8, w19, #1
62		b.vs _errOverflow
63		sxtw x8, w8
64		// push {x8}
65		stp x8, xzr, [sp, #-16]!
66		// pop {x8}
67		ldp x8, xzr, [sp], #16
68		mov x8, x8
69		mov x19, x8
70		subs w8, w20, #1
71		b.vs _errOverflow
72		sxtw x8, w8
73		// push {x8}
74		stp x8, xzr, [sp, #-16]!
75		// pop {x8}
76		ldp x8, xzr, [sp], #16
77		mov x8, x8
78		mov x20, x8
79	.L0:
80		cmp x20, #0
81		b.gt .L1
82		// Stack pointer unchanged, no stack allocated arguments
83		adrp x8, .L.str2
84		add x8, x8, :lo12:.L.str2
85		// push {x8}
86		stp x8, xzr, [sp, #-16]!
87		// pop {x8}
88		ldp x8, xzr, [sp], #16
89		mov x8, x8
90		mov x0, x8
91		// statement primitives do not return results (but will clobber r0/rax)
92		bl _prints
93		bl _println
94		// Stack pointer unchanged, no stack allocated arguments
95		adrp x8, .L.str3
96		add x8, x8, :lo12:.L.str3
97		// push {x8}
98		stp x8, xzr, [sp, #-16]!
99		// pop {x8}
100		ldp x8, xzr, [sp], #16
101		mov x8, x8
102		mov x0, x8
103		// statement primitives do not return results (but will clobber r0/rax)
104		bl _prints
105		// Stack pointer unchanged, no stack allocated arguments
106		mov x8, x19
107		mov x0, x8
108		// statement primitives do not return results (but will clobber r0/rax)
109		bl _printi
110		bl _println
111		// Stack pointer unchanged, no stack allocated variables
112		mov x0, #0
113		// pop {x19, x20}
114		ldp x19, x20, [sp], #16
115		// pop {fp, lr}
116		ldp fp, lr, [sp], #16
117		ret
118	
119	// length of .L._prints_str0
120		.word 4
121	.L._prints_str0:
122		.asciz "%.*s"
123	.align 4
124	_prints:
125		// push {lr}
126		stp lr, xzr, [sp, #-16]!
127		mov x2, x0
128		ldrsw x1, [x0, #-4]
129		adr x0, .L._prints_str0
130		bl printf
131		mov x0, #0
132		bl fflush
133		// pop {lr}
134		ldp lr, xzr, [sp], #16
135		ret
136	
137	// length of .L._printi_str0
138		.word 2
139	.L._printi_str0:
140		.asciz "%d"
141	.align 4
142	_printi:
143		// push {lr}
144		stp lr, xzr, [sp, #-16]!
145		mov x1, x0
146		adr x0, .L._printi_str0
147		bl printf
148		mov x0, #0
149		bl fflush
150		// pop {lr}
151		ldp lr, xzr, [sp], #16
152		ret
153	
154	// length of .L._println_str0
155		.word 0
156	.L._println_str0:
157		.asciz ""
158	.align 4
159	_println:
160		// push {lr}
161		stp lr, xzr, [sp, #-16]!
162		adr x0, .L._println_str0
163		bl puts
164		mov x0, #0
165		bl fflush
166		// pop {lr}
167		ldp lr, xzr, [sp], #16
168		ret
169	
170	// length of .L._errOverflow_str0
171		.word 52
172	.L._errOverflow_str0:
173		.asciz "fatal error: integer overflow or underflow occurred\n"
174	.align 4
175	_errOverflow:
176		adr x0, .L._errOverflow_str0
177		bl _prints
178		mov w0, #-1
179		bl exit
===========================================================
-- Finished

