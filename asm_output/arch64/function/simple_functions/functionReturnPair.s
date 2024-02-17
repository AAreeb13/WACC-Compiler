./valid/function/simple_functions/functionReturnPair.wacc
calling the reference compiler on ./valid/function/simple_functions/functionReturnPair.wacc
-- Test: functionReturnPair.wacc

-- Uploaded file: 
---------------------------------------------------------------
# creates a pair which is returned from a function

# Output:
# 10
#

# Program:

begin

  pair(int, int) getPair() is
    pair(int, int) p = newpair(10,15);
    return p
  end

  pair(int, int) p = call getPair();
  int x = fst p;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
functionReturnPair.s contents are:
===========================================================
0	.data
1	.align 4
2	.text
3	.global main
4	main:
5		// push {fp, lr}
6		stp fp, lr, [sp, #-16]!
7		// push {x19, x20}
8		stp x19, x20, [sp, #-16]!
9		mov fp, sp
10		// Stack pointer unchanged, no stack allocated variables
11		// Stack pointer unchanged, no stack allocated arguments
12		bl wacc_getPair
13		mov x16, x0
14		// Stack pointer unchanged, no stack allocated arguments
15		mov x8, x16
16		mov x19, x8
17		cbz x19, _errNull
18		ldr x8, [x19, #0]
19		mov x8, x8
20		mov x20, x8
21		// Stack pointer unchanged, no stack allocated arguments
22		mov x8, x20
23		mov x0, x8
24		// statement primitives do not return results (but will clobber r0/rax)
25		bl _printi
26		bl _println
27		// Stack pointer unchanged, no stack allocated variables
28		mov x0, #0
29		// pop {x19, x20}
30		ldp x19, x20, [sp], #16
31		// pop {fp, lr}
32		ldp fp, lr, [sp], #16
33		ret
34	
35	wacc_getPair:
36		// push {fp, lr}
37		stp fp, lr, [sp, #-16]!
38		// push {x19}
39		stp x19, xzr, [sp, #-16]!
40		mov fp, sp
41		// Stack pointer unchanged, no stack allocated variables
42		mov w0, #16
43		bl _malloc
44		mov x16, x0
45		mov x8, #10
46		str x8, [x16, #0]
47		mov x8, #15
48		str x8, [x16, #8]
49		mov x8, x16
50		mov x19, x8
51		mov x0, x19
52		// reset the stack pointer, undoing any pushes: this is often unnecessary, but is cheap
53		mov sp, fp
54		// pop {x19}
55		ldp x19, xzr, [sp], #16
56		// pop {fp, lr}
57		ldp fp, lr, [sp], #16
58		ret
59		// Stack pointer unchanged, no stack allocated variables
60		// 'ere be dragons: this is 100% dead code, functions always end in returns!
61	
62	// length of .L._prints_str0
63		.word 4
64	.L._prints_str0:
65		.asciz "%.*s"
66	.align 4
67	_prints:
68		// push {lr}
69		stp lr, xzr, [sp, #-16]!
70		mov x2, x0
71		ldrsw x1, [x0, #-4]
72		adr x0, .L._prints_str0
73		bl printf
74		mov x0, #0
75		bl fflush
76		// pop {lr}
77		ldp lr, xzr, [sp], #16
78		ret
79	
80	_malloc:
81		// push {lr}
82		stp lr, xzr, [sp, #-16]!
83		bl malloc
84		cbz x0, _errOutOfMemory
85		// pop {lr}
86		ldp lr, xzr, [sp], #16
87		ret
88	
89	// length of .L._printi_str0
90		.word 2
91	.L._printi_str0:
92		.asciz "%d"
93	.align 4
94	_printi:
95		// push {lr}
96		stp lr, xzr, [sp, #-16]!
97		mov x1, x0
98		adr x0, .L._printi_str0
99		bl printf
100		mov x0, #0
101		bl fflush
102		// pop {lr}
103		ldp lr, xzr, [sp], #16
104		ret
105	
106	// length of .L._println_str0
107		.word 0
108	.L._println_str0:
109		.asciz ""
110	.align 4
111	_println:
112		// push {lr}
113		stp lr, xzr, [sp, #-16]!
114		adr x0, .L._println_str0
115		bl puts
116		mov x0, #0
117		bl fflush
118		// pop {lr}
119		ldp lr, xzr, [sp], #16
120		ret
121	
122	// length of .L._errOutOfMemory_str0
123		.word 27
124	.L._errOutOfMemory_str0:
125		.asciz "fatal error: out of memory\n"
126	.align 4
127	_errOutOfMemory:
128		adr x0, .L._errOutOfMemory_str0
129		bl _prints
130		mov w0, #-1
131		bl exit
132	
133	// length of .L._errNull_str0
134		.word 45
135	.L._errNull_str0:
136		.asciz "fatal error: null pair dereferenced or freed\n"
137	.align 4
138	_errNull:
139		adr x0, .L._errNull_str0
140		bl _prints
141		mov w0, #-1
142		bl exit
===========================================================
-- Finished

