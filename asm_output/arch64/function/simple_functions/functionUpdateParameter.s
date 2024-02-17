./valid/function/simple_functions/functionUpdateParameter.wacc
calling the reference compiler on ./valid/function/simple_functions/functionUpdateParameter.wacc
-- Test: functionUpdateParameter.wacc

-- Uploaded file: 
---------------------------------------------------------------
# test that the passed parameter can be updated and used
# and that y remains the same

# Output:
# y is 1
# x is 1
# x is now 5
# y is still 1
#

# Program:

begin

  int f(int x) is
    print "x is ";
    println x;
    x = 5;
    print "x is now ";
    println x;
    return x
  end

  int y = 1;
  print "y is ";
  println y;
  int x = call f(y);
  print "y is still ";
  println y
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionUpdateParameter.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 5
3	.L.str0:
4		.asciz "x is "
5	// length of .L.str1
6		.word 9
7	.L.str1:
8		.asciz "x is now "
9	// length of .L.str2
10		.word 5
11	.L.str2:
12		.asciz "y is "
13	// length of .L.str3
14		.word 11
15	.L.str3:
16		.asciz "y is still "
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
27		mov x8, #1
28		mov x19, x8
29		// Stack pointer unchanged, no stack allocated arguments
30		adrp x8, .L.str2
31		add x8, x8, :lo12:.L.str2
32		// push {x8}
33		stp x8, xzr, [sp, #-16]!
34		// pop {x8}
35		ldp x8, xzr, [sp], #16
36		mov x8, x8
37		mov x0, x8
38		// statement primitives do not return results (but will clobber r0/rax)
39		bl _prints
40		// Stack pointer unchanged, no stack allocated arguments
41		mov x8, x19
42		mov x0, x8
43		// statement primitives do not return results (but will clobber r0/rax)
44		bl _printi
45		bl _println
46		// Stack pointer unchanged, no stack allocated arguments
47		mov x8, x19
48		mov x0, x8
49		bl wacc_f
50		mov x16, x0
51		// Stack pointer unchanged, no stack allocated arguments
52		mov x8, x16
53		mov x20, x8
54		// Stack pointer unchanged, no stack allocated arguments
55		adrp x8, .L.str3
56		add x8, x8, :lo12:.L.str3
57		// push {x8}
58		stp x8, xzr, [sp, #-16]!
59		// pop {x8}
60		ldp x8, xzr, [sp], #16
61		mov x8, x8
62		mov x0, x8
63		// statement primitives do not return results (but will clobber r0/rax)
64		bl _prints
65		// Stack pointer unchanged, no stack allocated arguments
66		mov x8, x19
67		mov x0, x8
68		// statement primitives do not return results (but will clobber r0/rax)
69		bl _printi
70		bl _println
71		// Stack pointer unchanged, no stack allocated variables
72		mov x0, #0
73		// pop {x19, x20}
74		ldp x19, x20, [sp], #16
75		// pop {fp, lr}
76		ldp fp, lr, [sp], #16
77		ret
78	
79	wacc_f:
80		// push {fp, lr}
81		stp fp, lr, [sp, #-16]!
82		mov fp, sp
83		// push {x0}
84		stp x0, xzr, [sp, #-16]!
85		// Set up X16 as a temporary second base pointer for the caller saved things
86		mov x16, sp
87		// Stack pointer unchanged, no stack allocated arguments
88		adrp x8, .L.str0
89		add x8, x8, :lo12:.L.str0
90		// push {x8}
91		stp x8, xzr, [sp, #-16]!
92		// pop {x8}
93		ldp x8, xzr, [sp], #16
94		mov x8, x8
95		mov x0, x8
96		// statement primitives do not return results (but will clobber r0/rax)
97		bl _prints
98		// pop {x0}
99		ldp x0, xzr, [sp], #16
100		// push {x0}
101		stp x0, xzr, [sp, #-16]!
102		// Set up X16 as a temporary second base pointer for the caller saved things
103		mov x16, sp
104		// Stack pointer unchanged, no stack allocated arguments
105		mov x8, x0
106		mov x0, x8
107		// statement primitives do not return results (but will clobber r0/rax)
108		bl _printi
109		bl _println
110		// pop {x0}
111		ldp x0, xzr, [sp], #16
112		mov x8, #5
113		mov x0, x8
114		// push {x0}
115		stp x0, xzr, [sp, #-16]!
116		// Set up X16 as a temporary second base pointer for the caller saved things
117		mov x16, sp
118		// Stack pointer unchanged, no stack allocated arguments
119		adrp x8, .L.str1
120		add x8, x8, :lo12:.L.str1
121		// push {x8}
122		stp x8, xzr, [sp, #-16]!
123		// pop {x8}
124		ldp x8, xzr, [sp], #16
125		mov x8, x8
126		mov x0, x8
127		// statement primitives do not return results (but will clobber r0/rax)
128		bl _prints
129		// pop {x0}
130		ldp x0, xzr, [sp], #16
131		// push {x0}
132		stp x0, xzr, [sp, #-16]!
133		// Set up X16 as a temporary second base pointer for the caller saved things
134		mov x16, sp
135		// Stack pointer unchanged, no stack allocated arguments
136		mov x8, x0
137		mov x0, x8
138		// statement primitives do not return results (but will clobber r0/rax)
139		bl _printi
140		bl _println
141		// pop {x0}
142		ldp x0, xzr, [sp], #16
143		mov x0, x0
144		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
145		mov sp, fp
146		// pop {fp, lr}
147		ldp fp, lr, [sp], #16
148		ret
149		// 'ere be dragons: this is 100% dead code, functions always end in returns!
150	
151	// length of .L._prints_str0
152		.word 4
153	.L._prints_str0:
154		.asciz "%.*s"
155	.align 4
156	_prints:
157		// push {lr}
158		stp lr, xzr, [sp, #-16]!
159		mov x2, x0
160		ldrsw x1, [x0, #-4]
161		adr x0, .L._prints_str0
162		bl printf
163		mov x0, #0
164		bl fflush
165		// pop {lr}
166		ldp lr, xzr, [sp], #16
167		ret
168	
169	// length of .L._printi_str0
170		.word 2
171	.L._printi_str0:
172		.asciz "%d"
173	.align 4
174	_printi:
175		// push {lr}
176		stp lr, xzr, [sp, #-16]!
177		mov x1, x0
178		adr x0, .L._printi_str0
179		bl printf
180		mov x0, #0
181		bl fflush
182		// pop {lr}
183		ldp lr, xzr, [sp], #16
184		ret
185	
186	// length of .L._println_str0
187		.word 0
188	.L._println_str0:
189		.asciz ""
190	.align 4
191	_println:
192		// push {lr}
193		stp lr, xzr, [sp, #-16]!
194		adr x0, .L._println_str0
195		bl puts
196		mov x0, #0
197		bl fflush
198		// pop {lr}
199		ldp lr, xzr, [sp], #16
200		ret
===========================================================
-- Finished

