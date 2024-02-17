./valid/scope/scopeWhileNested.wacc
calling the reference compiler on ./valid/scope/scopeWhileNested.wacc
-- Test: scopeWhileNested.wacc

-- Uploaded file: 
---------------------------------------------------------------
# variable scoping nested within a while-loop

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
  while x > 0 do
    begin
      string x = "counting... " ;
      print x
    end ;
    println x ;
    x = x - 1
  done ;
  print x ;
  println " Boom!"
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
scopeWhileNested.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 12
3	.L.str0:
4		.asciz "counting... "
5	// length of .L.str1
6		.word 6
7	.L.str1:
8		.asciz " Boom!"
9	.align 4
10	.text
11	.global main
12	main:
13		// push {fp, lr}
14		stp fp, lr, [sp, #-16]!
15		// push {x19, x20}
16		stp x19, x20, [sp, #-16]!
17		mov fp, sp
18		// Stack pointer unchanged, no stack allocated variables
19		mov x8, #5
20		mov x19, x8
21		b .L0
22	.L1:
23		// Stack pointer unchanged, no stack allocated variables
24		adrp x8, .L.str0
25		add x8, x8, :lo12:.L.str0
26		// push {x8}
27		stp x8, xzr, [sp, #-16]!
28		// pop {x8}
29		ldp x8, xzr, [sp], #16
30		mov x8, x8
31		mov x20, x8
32		// Stack pointer unchanged, no stack allocated arguments
33		mov x8, x20
34		mov x0, x8
35		// statement primitives do not return results (but will clobber r0/rax)
36		bl _prints
37		// Stack pointer unchanged, no stack allocated variables
38		// Stack pointer unchanged, no stack allocated arguments
39		mov x8, x19
40		mov x0, x8
41		// statement primitives do not return results (but will clobber r0/rax)
42		bl _printi
43		bl _println
44		subs w8, w19, #1
45		b.vs _errOverflow
46		sxtw x8, w8
47		// push {x8}
48		stp x8, xzr, [sp, #-16]!
49		// pop {x8}
50		ldp x8, xzr, [sp], #16
51		mov x8, x8
52		mov x19, x8
53	.L0:
54		cmp x19, #0
55		b.gt .L1
56		// Stack pointer unchanged, no stack allocated arguments
57		mov x8, x19
58		mov x0, x8
59		// statement primitives do not return results (but will clobber r0/rax)
60		bl _printi
61		// Stack pointer unchanged, no stack allocated arguments
62		adrp x8, .L.str1
63		add x8, x8, :lo12:.L.str1
64		// push {x8}
65		stp x8, xzr, [sp, #-16]!
66		// pop {x8}
67		ldp x8, xzr, [sp], #16
68		mov x8, x8
69		mov x0, x8
70		// statement primitives do not return results (but will clobber r0/rax)
71		bl _prints
72		bl _println
73		// Stack pointer unchanged, no stack allocated variables
74		mov x0, #0
75		// pop {x19, x20}
76		ldp x19, x20, [sp], #16
77		// pop {fp, lr}
78		ldp fp, lr, [sp], #16
79		ret
80	
81	// length of .L._prints_str0
82		.word 4
83	.L._prints_str0:
84		.asciz "%.*s"
85	.align 4
86	_prints:
87		// push {lr}
88		stp lr, xzr, [sp, #-16]!
89		mov x2, x0
90		ldrsw x1, [x0, #-4]
91		adr x0, .L._prints_str0
92		bl printf
93		mov x0, #0
94		bl fflush
95		// pop {lr}
96		ldp lr, xzr, [sp], #16
97		ret
98	
99	// length of .L._printi_str0
100		.word 2
101	.L._printi_str0:
102		.asciz "%d"
103	.align 4
104	_printi:
105		// push {lr}
106		stp lr, xzr, [sp, #-16]!
107		mov x1, x0
108		adr x0, .L._printi_str0
109		bl printf
110		mov x0, #0
111		bl fflush
112		// pop {lr}
113		ldp lr, xzr, [sp], #16
114		ret
115	
116	// length of .L._println_str0
117		.word 0
118	.L._println_str0:
119		.asciz ""
120	.align 4
121	_println:
122		// push {lr}
123		stp lr, xzr, [sp, #-16]!
124		adr x0, .L._println_str0
125		bl puts
126		mov x0, #0
127		bl fflush
128		// pop {lr}
129		ldp lr, xzr, [sp], #16
130		ret
131	
132	// length of .L._errOverflow_str0
133		.word 52
134	.L._errOverflow_str0:
135		.asciz "fatal error: integer overflow or underflow occurred\n"
136	.align 4
137	_errOverflow:
138		adr x0, .L._errOverflow_str0
139		bl _prints
140		mov w0, #-1
141		bl exit
===========================================================
-- Finished

