./valid/IO/IOSequence.wacc
calling the reference compiler on ./valid/IO/IOSequence.wacc
-- Test: IOSequence.wacc

-- Uploaded file: 
---------------------------------------------------------------
# basic input/output sequence

# Input: 37

# Output:
# Please input an integer: You input: 37
#

# Program:

begin
  int x = 0 ;
  print "Please input an integer: " ;
  read x ;
  print "You input: " ;
  println x
end
---------------------------------------------------------------

-- Compiler Output:
-- Compiling...
-- Printing Assembly...
IOSequence.s contents are:
===========================================================
0	.data
1	// length of .L.str0
2		.word 25
3	.L.str0:
4		.asciz "Please input an integer: "
5	// length of .L.str1
6		.word 11
7	.L.str1:
8		.asciz "You input: "
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
19		mov x8, #0
20		mov x19, x8
21		// Stack pointer unchanged, no stack allocated arguments
22		adrp x8, .L.str0
23		add x8, x8, :lo12:.L.str0
24		// push {x8}
25		stp x8, xzr, [sp, #-16]!
26		// pop {x8}
27		ldp x8, xzr, [sp], #16
28		mov x8, x8
29		mov x0, x8
30		// statement primitives do not return results (but will clobber r0/rax)
31		bl _prints
32		// Stack pointer unchanged, no stack allocated arguments
33		// load the current value in the destination of the read so it supports defaults
34		mov x8, x19
35		mov x0, x8
36		bl _readi
37		mov x16, x0
38		mov x8, x16
39		mov x19, x8
40		// Stack pointer unchanged, no stack allocated arguments
41		adrp x8, .L.str1
42		add x8, x8, :lo12:.L.str1
43		// push {x8}
44		stp x8, xzr, [sp, #-16]!
45		// pop {x8}
46		ldp x8, xzr, [sp], #16
47		mov x8, x8
48		mov x0, x8
49		// statement primitives do not return results (but will clobber r0/rax)
50		bl _prints
51		// Stack pointer unchanged, no stack allocated arguments
52		mov x8, x19
53		mov x0, x8
54		// statement primitives do not return results (but will clobber r0/rax)
55		bl _printi
56		bl _println
57		// Stack pointer unchanged, no stack allocated variables
58		mov x0, #0
59		// pop {x19}
60		ldp x19, xzr, [sp], #16
61		// pop {fp, lr}
62		ldp fp, lr, [sp], #16
63		ret
64	
65	// length of .L._prints_str0
66		.word 4
67	.L._prints_str0:
68		.asciz "%.*s"
69	.align 4
70	_prints:
71		// push {lr}
72		stp lr, xzr, [sp, #-16]!
73		mov x2, x0
74		ldrsw x1, [x0, #-4]
75		adr x0, .L._prints_str0
76		bl printf
77		mov x0, #0
78		bl fflush
79		// pop {lr}
80		ldp lr, xzr, [sp], #16
81		ret
82	
83	// length of .L._readi_str0
84		.word 2
85	.L._readi_str0:
86		.asciz "%d"
87	.align 4
88	_readi:
89		// X0 contains the "original" value of the destination of the read
90		// allocate space on the stack to store the read: preserve alignment!
91		// the passed default argument should be stored in case of EOF
92		// aarch64 mandates 16-byte SP alignment at all times, might as well merge the stores
93		// push {x0, lr}
94		stp x0, lr, [sp, #-16]!
95		mov x1, sp
96		adr x0, .L._readi_str0
97		bl scanf
98		// pop {x0, lr}
99		ldp x0, lr, [sp], #16
100		ret
101	
102	// length of .L._printi_str0
103		.word 2
104	.L._printi_str0:
105		.asciz "%d"
106	.align 4
107	_printi:
108		// push {lr}
109		stp lr, xzr, [sp, #-16]!
110		mov x1, x0
111		adr x0, .L._printi_str0
112		bl printf
113		mov x0, #0
114		bl fflush
115		// pop {lr}
116		ldp lr, xzr, [sp], #16
117		ret
118	
119	// length of .L._println_str0
120		.word 0
121	.L._println_str0:
122		.asciz ""
123	.align 4
124	_println:
125		// push {lr}
126		stp lr, xzr, [sp, #-16]!
127		adr x0, .L._println_str0
128		bl puts
129		mov x0, #0
130		bl fflush
131		// pop {lr}
132		ldp lr, xzr, [sp], #16
133		ret
===========================================================
-- Finished

