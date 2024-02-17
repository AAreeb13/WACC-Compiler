./valid/function/nested_functions/printTriangle.wacc
calling the reference compiler on ./valid/function/nested_functions/printTriangle.wacc
-- Test: printTriangle.wacc

-- Uploaded file: 
---------------------------------------------------------------
# print a fixed size triangle

# Output:
# --------
# -------
# ------
# -----
# ----
# ---
# --
# -
#

# Program:

begin
  int f(int x) is
    if x == 0 then
      skip
    else
      int i = x ;
      while i > 0 do
        print "-" ;
        i = i - 1
      done ;
      println "" ;
      int s = call f(x - 1)
    fi ;
    return 0
  end

  int s = call f(8)
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
printTriangle.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 1
3	.L.str0:
4		.asciz "-"
5	// length of .L.str1
6		.word 0
7	.L.str1:
8		.asciz ""
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
19		// Stack pointer unchanged, no stack allocated arguments
20		mov x8, #8
21		mov x0, x8
22		bl wacc_f
23		mov x16, x0
24		// Stack pointer unchanged, no stack allocated arguments
25		mov x8, x16
26		mov x19, x8
27		// Stack pointer unchanged, no stack allocated variables
28		mov x0, #0
29		// pop {x19}
30		ldp x19, xzr, [sp], #16
31		// pop {fp, lr}
32		ldp fp, lr, [sp], #16
33		ret
34	
35	wacc_f:
36		// push {fp, lr}
37		stp fp, lr, [sp, #-16]!
38		// push {x19, x20}
39		stp x19, x20, [sp, #-16]!
40		mov fp, sp
41		cmp x0, #0
42		b.eq .L0
43		// Stack pointer unchanged, no stack allocated variables
44		mov x8, x0
45		mov x19, x8
46		b .L2
47	.L3:
48		// push {x0}
49		stp x0, xzr, [sp, #-16]!
50		// Set up X16 as a temporary second base pointer for the caller saved things
51		mov x16, sp
52		// Stack pointer unchanged, no stack allocated arguments
53		adrp x8, .L.str0
54		add x8, x8, :lo12:.L.str0
55		// push {x8}
56		stp x8, xzr, [sp, #-16]!
57		// pop {x8}
58		ldp x8, xzr, [sp], #16
59		mov x8, x8
60		mov x0, x8
61		// statement primitives do not return results (but will clobber r0/rax)
62		bl _prints
63		// pop {x0}
64		ldp x0, xzr, [sp], #16
65		subs w8, w19, #1
66		b.vs _errOverflow
67		sxtw x8, w8
68		// push {x8}
69		stp x8, xzr, [sp, #-16]!
70		// pop {x8}
71		ldp x8, xzr, [sp], #16
72		mov x8, x8
73		mov x19, x8
74	.L2:
75		cmp x19, #0
76		b.gt .L3
77		// push {x0}
78		stp x0, xzr, [sp, #-16]!
79		// Set up X16 as a temporary second base pointer for the caller saved things
80		mov x16, sp
81		// Stack pointer unchanged, no stack allocated arguments
82		adrp x8, .L.str1
83		add x8, x8, :lo12:.L.str1
84		// push {x8}
85		stp x8, xzr, [sp, #-16]!
86		// pop {x8}
87		ldp x8, xzr, [sp], #16
88		mov x8, x8
89		mov x0, x8
90		// statement primitives do not return results (but will clobber r0/rax)
91		bl _prints
92		bl _println
93		// pop {x0}
94		ldp x0, xzr, [sp], #16
95		// push {x0}
96		stp x0, xzr, [sp, #-16]!
97		// Set up X16 as a temporary second base pointer for the caller saved things
98		mov x16, sp
99		// Stack pointer unchanged, no stack allocated arguments
100		subs w8, w0, #1
101		b.vs _errOverflow
102		sxtw x8, w8
103		// push {x8}
104		stp x8, xzr, [sp, #-16]!
105		// pop {x8}
106		ldp x8, xzr, [sp], #16
107		mov x8, x8
108		mov x0, x8
109		bl wacc_f
110		mov x16, x0
111		// Stack pointer unchanged, no stack allocated arguments
112		// pop {x0}
113		ldp x0, xzr, [sp], #16
114		mov x8, x16
115		mov x20, x8
116		// Stack pointer unchanged, no stack allocated variables
117		b .L1
118	.L0:
119	.L1:
120		mov x0, #0
121		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
122		mov sp, fp
123		// pop {x19, x20}
124		ldp x19, x20, [sp], #16
125		// pop {fp, lr}
126		ldp fp, lr, [sp], #16
127		ret
128		// 'ere be dragons: this is 100% dead code, functions always end in returns!
129	
130	// length of .L._prints_str0
131		.word 4
132	.L._prints_str0:
133		.asciz "%.*s"
134	.align 4
135	_prints:
136		// push {lr}
137		stp lr, xzr, [sp, #-16]!
138		mov x2, x0
139		ldrsw x1, [x0, #-4]
140		adr x0, .L._prints_str0
141		bl printf
142		mov x0, #0
143		bl fflush
144		// pop {lr}
145		ldp lr, xzr, [sp], #16
146		ret
147	
148	// length of .L._println_str0
149		.word 0
150	.L._println_str0:
151		.asciz ""
152	.align 4
153	_println:
154		// push {lr}
155		stp lr, xzr, [sp, #-16]!
156		adr x0, .L._println_str0
157		bl puts
158		mov x0, #0
159		bl fflush
160		// pop {lr}
161		ldp lr, xzr, [sp], #16
162		ret
163	
164	// length of .L._errOverflow_str0
165		.word 52
166	.L._errOverflow_str0:
167		.asciz "fatal error: integer overflow or underflow occurred\n"
168	.align 4
169	_errOverflow:
170		adr x0, .L._errOverflow_str0
171		bl _prints
172		mov w0, #-1
173		bl exit
===========================================================
-- Finished

