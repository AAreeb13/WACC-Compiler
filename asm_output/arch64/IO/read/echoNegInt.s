./valid/IO/read/echoNegInt.wacc
calling the reference compiler on ./valid/IO/read/echoNegInt.wacc
-- Test: echoNegInt.wacc

-- Uploaded file: 
---------------------------------------------------------------
# echo the user's input int

# Input: -5

# Output:
# enter an integer to echo
# -5
#

# Program:

begin
  int x = 1 ;
  println "enter an integer to echo";
  read x ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
echoNegInt.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 24
3	.L.str0:
4		.asciz "enter an integer to echo"
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
29		// Stack pointer unchanged, no stack allocated arguments
30		// load the current value in the destination of the read so it supports defaults
31		mov x8, x19
32		mov x0, x8
33		bl _readi
34		mov x16, x0
35		mov x8, x16
36		mov x19, x8
37		// Stack pointer unchanged, no stack allocated arguments
38		mov x8, x19
39		mov x0, x8
40		// statement primitives do not return results (but will clobber r0/rax)
41		bl _printi
42		bl _println
43		// Stack pointer unchanged, no stack allocated variables
44		mov x0, #0
45		// pop {x19}
46		ldp x19, xzr, [sp], #16
47		// pop {fp, lr}
48		ldp fp, lr, [sp], #16
49		ret
50	
51	// length of .L._prints_str0
52		.word 4
53	.L._prints_str0:
54		.asciz "%.*s"
55	.align 4
56	_prints:
57		// push {lr}
58		stp lr, xzr, [sp, #-16]!
59		mov x2, x0
60		ldrsw x1, [x0, #-4]
61		adr x0, .L._prints_str0
62		bl printf
63		mov x0, #0
64		bl fflush
65		// pop {lr}
66		ldp lr, xzr, [sp], #16
67		ret
68	
69	// length of .L._readi_str0
70		.word 2
71	.L._readi_str0:
72		.asciz "%d"
73	.align 4
74	_readi:
75		// X0 contains the "original" value of the destination of the read
76		// allocate space on the stack to store the read: preserve alignment!
77		// the passed default argument should be stored in case of EOF
78		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
79		// push {x0, lr}
80		stp x0, lr, [sp, #-16]!
81		mov x1, sp
82		adr x0, .L._readi_str0
83		bl scanf
84		// pop {x0, lr}
85		ldp x0, lr, [sp], #16
86		ret
87	
88	// length of .L._printi_str0
89		.word 2
90	.L._printi_str0:
91		.asciz "%d"
92	.align 4
93	_printi:
94		// push {lr}
95		stp lr, xzr, [sp, #-16]!
96		mov x1, x0
97		adr x0, .L._printi_str0
98		bl printf
99		mov x0, #0
100		bl fflush
101		// pop {lr}
102		ldp lr, xzr, [sp], #16
103		ret
104	
105	// length of .L._println_str0
106		.word 0
107	.L._println_str0:
108		.asciz ""
109	.align 4
110	_println:
111		// push {lr}
112		stp lr, xzr, [sp, #-16]!
113		adr x0, .L._println_str0
114		bl puts
115		mov x0, #0
116		bl fflush
117		// pop {lr}
118		ldp lr, xzr, [sp], #16
119		ret
===========================================================
-- Finished

